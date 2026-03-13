# =============================================================================
# Sim_Functions.R — Core simulation functions for academic job market model
#
# This file provides all functions needed to simulate a two-sided matching
# market for the academic statistics job market, including:
#   - Candidate and department utility computation
#   - Questionnaire-based fit score calculation
#   - Neural network acceptance probability models
#   - Interview selection via sure screening
#   - Offer resolution (two-round offers with scramble)
#   - Burn-in and learned prior infrastructure
#   - Multi-replicate simulation orchestration
#   - Publication-quality figure generation
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(torch)
  library(Matrix)
  library(MASS)
  library(ggplot2)
  library(gridExtra)
  library(viridis)
  library(janitor)
  library(patchwork)
  library(scales)
  library(paletteer)
  library(parallel)
  library(future)
  library(future.apply)
})

norm01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (any(!is.finite(rng)) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

`%||%` <- function(x, y) if (is.null(x)) y else x

assign_tiers_from_quantiles <- function(x, cutpoints = c(0.10, 0.25, 0.50),
                                        tier_labels = c("Tier 1","Tier 2","Tier 3","Tier 4")) {
  stopifnot(length(cutpoints) == 3, length(tier_labels) == 4)
  qq <- stats::quantile(x, probs = 1 - cutpoints, na.rm = TRUE)
  cut(x, breaks = c(-Inf, qq[3], qq[2], qq[1], Inf),
      labels = rev(tier_labels), include.lowest = TRUE, right = TRUE)
}

candidate_utility <- function(v_i_bar, s_j, f_j, cand_tier = NULL,
                              prestige_sensitivity = NULL) {
  # Cobb-Douglas: V = s_j^beta * f_j^(1-beta)
  beta_base <- pmin(0.30 + 0.35 * v_i_bar, 0.9)
  prestige_gap <- s_j - v_i_bar
  beta <- pmin(0.9, pmax(0.05, beta_base + 0.30 * prestige_gap))
  s <- s_j + 1e-8
  f <- f_j + 1e-8
  V_ij <- s^beta * f^(1 - beta)
  pmin(pmax(V_ij, 1e-6), 1 - 1e-6)
}

department_utility <- function(s_j, v_i_bar, f_j, dept_tier = NULL, cand_tier = NULL) {
  # Cobb-Douglas: U = v^alpha * f^(1-alpha)
  alpha <- pmin(0.50 + 0.40 * s_j, 0.9)
  v <- v_i_bar + 1e-8
  f <- f_j + 1e-8
  U <- v^alpha * f^(1 - alpha)
  pmin(pmax(U, 1e-6), 1 - 1e-6)
}

questions <- list(
  numerical = c("q4_cost_of_living", "q6_typical_salary_range", "q14_phd_student_ratio"),
  categorical = list(
    q1_geographic_setting = c("A", "B", "C", "D"),
    q2_region = c("Northeast", "Southeast", "Midwest", "Southwest", "West Coast"),
    q3_airport_proximity = c("A", "B", "C", "D"),
    q5_dual_career = c("Y", "N"),
    q7_typical_startup = c("A", "B", "C", "D", "E"),
    q8_guaranteed_summer = c("A", "B", "C", "D"),
    q9_typical_teaching_load = c("A", "B", "C", "D"),
    q10_course_types = c("A", "B", "C", "D"),
    q11_mentoring_program = c("A", "B", "C", "D"),
    q12_research_culture = c("A", "B", "C", "D", "E"),
    q13_publication_venues = c("A", "B", "C", "D", "E"),
    q15_medical_school_proximity = c("0", "1")
  )
)

safe_categorical_to_index <- function(values, levels, var_name = "unknown") {
  values_char <- as.character(values)
  levels_char <- as.character(levels)
  n_levels <- length(levels_char)
  values_char[is.na(values_char) | values_char == "" | values_char == "NA"] <- levels_char[1]
  indices <- match(values_char, levels_char)
  unmatched_mask <- is.na(indices)
  if (any(unmatched_mask)) {
    cat("WARNING [", var_name, "]: Unmatched values: ",
        paste(head(unique(values_char[unmatched_mask]), 5), collapse = ", "), "\
")
    indices[unmatched_mask] <- 1L
  }
  indices <- as.integer(indices)
  out_of_range <- indices < 1 | indices > n_levels
  if (any(out_of_range, na.rm = TRUE)) indices <- pmax(1L, pmin(indices, n_levels))
  indices
}

# =============================================================================
# compute_f_j — Fit score computation
#
# Computes f_j for all candidates vs one department.
# Uses the department's weight_vector (generated in prepare_departments)
# to weight each questionnaire dimension, implementing department-specific
# normalized weights {w_tilde_jk}.
# =============================================================================
compute_f_j <- function(candidates_df, dept_row, questions, gamma = 1.5) {
  n <- nrow(candidates_df)
  if (n == 0) return(numeric(0))
  if (is.data.frame(dept_row)) dept_row <- as.list(dept_row[1, ])
  eps <- 1e-6
  
  #  Retrieve department-specific weights 
  dept_weights <- dept_row[["weight_vector"]]
  if (is.null(dept_weights) || !is.list(dept_weights)) {
    if (is.list(dept_weights)) dept_weights <- dept_weights[[1]]
  } else if (is.list(dept_weights)) {
    dept_weights <- dept_weights[[1]]
  }
  n_num <- length(questions$numerical)
  n_cat <- length(questions$categorical)
  n_total <- n_num + n_cat
  if (is.null(dept_weights) || length(dept_weights) != n_total) {
    dept_weights <- rep(1 / n_total, n_total)
  }
  num_weights <- dept_weights[1:n_num]
  cat_weights_vec <- dept_weights[(n_num + 1):n_total]
  names(cat_weights_vec) <- names(questions$categorical)

  num_scores <- list()
  num_w <- numeric(0)
  
  dept_col_val <- dept_row[["q4_cost_of_living"]]
  if (!is.null(dept_col_val) && !is.na(dept_col_val)) {
    cand_raw <- as.numeric(candidates_df[["q_q4_cost_of_living"]])
    cand_norm <- pmin(pmax((cand_raw - 60000) / 80000, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_col_val) - 60000) / 80000, 0), 1)
    diff_val <- dept_norm - cand_norm - 0.1
    s_col <- ifelse(is.na(cand_raw), NA_real_,
                    ifelse(diff_val <= 0, 1.0, pmax(0, 1 - 1.5 * diff_val)))
    num_scores[[length(num_scores) + 1]] <- s_col
    num_w <- c(num_w, num_weights[1])
  }
  
  dept_sal_val <- dept_row[["q6_typical_salary_range"]]
  if (!is.null(dept_sal_val) && !is.na(dept_sal_val)) {
    cand_raw <- as.numeric(candidates_df[["q_q6_typical_salary_range"]])
    cand_norm <- pmin(pmax((cand_raw - 80000) / 120000, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_sal_val) - 80000) / 120000, 0), 1)
    diff_val <- cand_norm - dept_norm - 0.1
    s_sal <- ifelse(is.na(cand_raw), NA_real_,
                    ifelse(diff_val <= 0, 1.0, pmax(0, 1 - 1.5 * diff_val)))
    num_scores[[length(num_scores) + 1]] <- s_sal
    num_w <- c(num_w, num_weights[2])
  }
  
  dept_phd_val <- dept_row[["q14_phd_student_ratio"]]
  if (!is.null(dept_phd_val) && !is.na(dept_phd_val)) {
    cand_raw <- as.numeric(candidates_df[["q_q14_phd_student_ratio"]])
    cand_norm <- pmin(pmax((cand_raw - 0.5) / 4.5, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_phd_val) - 0.5) / 4.5, 0), 1)
    s_phd <- ifelse(is.na(cand_raw), NA_real_, 1 - abs(cand_norm - dept_norm))
    num_scores[[length(num_scores) + 1]] <- s_phd
    num_w <- c(num_w, num_weights[3])
  }
  
  if (length(num_scores) > 0) {
    num_mat <- do.call(cbind, num_scores)
    w_vec <- num_w / sum(num_w)
    not_na <- !is.na(num_mat)
    num_mat_clean <- num_mat
    num_mat_clean[!not_na] <- 0
    weighted_sum <- as.numeric(num_mat_clean %*% w_vec)
    effective_w <- as.numeric(not_na %*% w_vec)
    num_avg <- ifelse(effective_w > 0, weighted_sum / effective_w, 0.5)
  } else {
    num_avg <- rep(0.5, n)
  }
  
  # CATEGORICAL SCORES 
  cat_score_sum <- rep(0, n)
  cat_weight_sum <- rep(0, n)
  
  for (q_idx in seq_along(names(questions$categorical))) {
    q_name <- names(questions$categorical)[q_idx]
    dept_val <- dept_row[[q_name]]
    if (is.null(dept_val) || is.na(dept_val)) next
    dept_char <- as.character(dept_val)
    cand_col_name <- paste0("q_", q_name)
    if (!cand_col_name %in% names(candidates_df)) next
    cand_vals <- candidates_df[[cand_col_name]]
    valid <- !is.na(cand_vals)
    if (!any(valid)) next
    
    # Use department-specific weight for this question
    w <- cat_weights_vec[[q_name]]
    if (is.null(w) || is.na(w)) w <- 1.0 / n_cat
    
    if (q_name == "q2_region") {
      cand_str <- paste0(",", gsub("\\s+", "", as.character(cand_vals)), ",")
dept_tok <- paste0(",", gsub("\\s+", "", dept_char), ",")
s_k <- as.numeric(grepl(dept_tok, cand_str, fixed = TRUE))

      s_k[!valid] <- 0
    } else {
      levels_q <- questions$categorical[[q_name]]
      max_dist <- length(levels_q) - 1
      dept_pos <- match(dept_char, levels_q)
      cand_char <- as.character(cand_vals)
      cand_pos <- match(cand_char, levels_q)
      if (!is.na(dept_pos)) {
        s_k <- ifelse(is.na(cand_pos), 0.5,
                      ifelse(cand_pos == dept_pos, 1.0,
                             pmax(0, 1 - abs(cand_pos - dept_pos) / max_dist)))
      } else {
        s_k <- rep(0.5, n)
      }
      s_k[!valid] <- 0
    }
    
    cat_score_sum <- cat_score_sum + s_k * w
    cat_weight_sum <- cat_weight_sum + valid * w
  }
  cat_avg <- ifelse(cat_weight_sum > 0, cat_score_sum / cat_weight_sum, 0.5)

  total_num_w <- sum(num_weights)
  total_cat_w <- sum(cat_weights_vec)
  total_w <- total_num_w + total_cat_w
  num_share <- total_num_w / total_w
  cat_share <- total_cat_w / total_w
  
  S_ij <- num_share * num_avg + cat_share * cat_avg

  # Per-department normalization: rescale S_ij to [0, 1] within this department
  # so every department sees candidates spanning the full alignment range.
  S_min <- min(S_ij, na.rm = TRUE)
  S_max <- max(S_ij, na.rm = TRUE)
  if (S_max - S_min > eps) {
    S_ij <- (S_ij - S_min) / (S_max - S_min)
  }

  f_raw <- (exp(gamma * S_ij) - 1) / (exp(gamma) - 1)
  f <- 0.5 + 0.5 * f_raw   # remap [0,1] -> [0.5,1]
  pmin(pmax(f, 0.5), 1 - eps)
}


# =============================================================================
# generate_candidates — Sample a cohort of candidates
# =============================================================================
generate_candidates <- function(n_candidates, questions, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  candidates <- tibble(
    cand_id = 1:n_candidates,
    v_i1 = rbeta(n_candidates, 2, 1.1),
    v_i2 = rbeta(n_candidates, 2, 1.1),
    v_i3 = rbeta(n_candidates, 2, 1.1)
  ) %>% mutate(v_i_bar = (v_i1 * v_i2 * v_i3)^(1/3))
  
  candidates <- candidates %>%
    mutate(quality_pctl = percent_rank(v_i_bar))

  n <- n_candidates; v_pctl <- candidates$quality_pctl

  # Preference heterogeneity
  v_centered <- candidates$v_i_bar - 0.62
  flexibility <- pmin(1.0, pmax(0.2, runif(n, 0.3, 1.0) - 0.25 * v_centered))

  # Idiosyncratic preference dimensions with quality-correlated shifts
  teaching_focus <- pmin(0.95, pmax(0.05, rbeta(n, 2, 2) - 0.20 * v_centered))
  location_priority <- pmin(0.95, pmax(0.05, rbeta(n, 2, 2) + 0.20 * v_centered))
  collab_preference <- rbeta(n, 2, 2)   # 0 = independent, 1 = collaborative

  # Helper: weighted sampling
  vsample <- function(choices, prob_matrix) {
    rs <- rowSums(prob_matrix)
    pm <- prob_matrix / rs
    u <- runif(nrow(pm))
    nc <- ncol(pm)
    cum <- pm
    for (k in 2:nc) cum[, k] <- cum[, k - 1L] + pm[, k]
    idx <- nc - rowSums(u < cum) + 1L
    choices[pmin(pmax(idx, 1L), nc)]
  }
  
  # Numerical questions 
  candidates$q_q6_typical_salary_range <- pmax(80000, pmin(180000,
                                                           80000 + 60000 * runif(n) + rnorm(n, 0, 12000)))
  candidates$q_q4_cost_of_living <- pmax(60000, pmin(140000,
                                                     70000 + 40000 * runif(n) + rnorm(n, 0, 15000)))
  candidates$q_q14_phd_student_ratio <- pmax(0.5, pmin(5.0,
                                                       1.0 + 2.5 * runif(n) + rnorm(n, 0, 0.5)))
  
  # Geographic setting
  pm <- cbind(
    (1-flexibility)*0.50 + flexibility*0.25 - location_priority*0.2 + collab_preference*0.15,  # A (Major city) - collaborative types prefer cities
    (1-flexibility)*0.30 + flexibility*0.30 + location_priority*0.05,                          # B (Mid-size)
    (1-flexibility)*0.15 + flexibility*0.30 + location_priority*0.15 - collab_preference*0.1,  # C (College town)
    (1-flexibility)*0.05 + flexibility*0.15 + location_priority*0.05 - collab_preference*0.05  # D (Rural)
  )
  pm <- pmax(pm, 0.02)
  pm <- pm / rowSums(pm)
  candidates$q_q1_geographic_setting <- vsample(c("A","B","C","D"), pm)
  
  # Region (multi-select)
  n_reg <- pmin(pmax(round(1 + 3 * flexibility + runif(n)), 1), 5)
  reg_names <- c("Northeast","West Coast","Midwest","Southeast","Southwest")
  reg_probs <- c(0.22, 0.22, 0.20, 0.20, 0.16)
  candidates$q_q2_region <- vapply(n_reg, function(nr)
    paste(sample(reg_names, nr, prob = reg_probs, replace = FALSE), collapse = ","), character(1))
  
  # Teaching load 
  pm <- cbind(
    pmax(0.50-0.3*flexibility, 0.02) * (1 - teaching_focus*0.4),  # A (1-1 load) - research-focused prefer this
    0.30,                                                          # B (1-2 load)
    pmax(0.15+0.2*flexibility, 0.02) * (1 + teaching_focus*0.3),  # C (2-2 load)
    pmax(0.05+0.1*flexibility, 0.02) * (1 + teaching_focus*0.5)   # D (2-3+ load) - teaching-focused more open
  )
  pm <- pmax(pm, 0.02)
  pm <- pm / rowSums(pm)
  candidates$q_q9_typical_teaching_load <- vsample(c("A","B","C","D"), pm)
  
  # Startup
  pm <- cbind(0.05 + 0.1 * flexibility,
              0.15 + 0.1 * flexibility,
              rep(0.30, n),
              0.30 - 0.1 * flexibility,
              0.20 - 0.1 * flexibility)
  pm <- pmax(pm, 0.02)
  candidates$q_q7_typical_startup <- vsample(c("A","B","C","D","E"), pm)
  
  # Simple categoricals 
  candidates$q_q3_airport_proximity <- sample(c("A","B","C","D"), n, replace=TRUE, prob=c(0.35,0.35,0.20,0.10))
  candidates$q_q5_dual_career <- sample(c("Y","N"), n, replace=TRUE, prob=c(0.35,0.65))
  candidates$q_q8_guaranteed_summer <- sample(c("A","B","C","D"), n, replace=TRUE, prob=c(0.35,0.35,0.20,0.10))
  
  # Course types - influenced by teaching_focus
  pm <- cbind(
    pmax(0.35-0.15*flexibility, 0.02) * (1 - teaching_focus*0.3),  # A (Graduate only) - research-focused
    0.35,                                                           # B (Mix)
    pmax(0.20+0.1*flexibility, 0.02) * (1 + teaching_focus*0.2),   # C (Mostly undergrad)
    pmax(0.10+0.05*flexibility, 0.02) * (1 + teaching_focus*0.4)   # D (Undergrad only) - teaching-focused
  )
  pm <- pmax(pm, 0.02)
  pm <- pm / rowSums(pm)
  candidates$q_q10_course_types <- vsample(c("A","B","C","D"), pm)
  
  candidates$q_q11_mentoring_program <- sample(c("A","B","C","D"), n, replace=TRUE, prob=c(0.25,0.35,0.25,0.15))

  # Research culture
  pm <- cbind(
    0.15 * (1.5 - collab_preference),     # A (Individual) - independent types prefer this
    0.20,                                  # B (Mostly individual)
    0.30,                                  # C (Balanced)
    0.25 * (1 + collab_preference*0.6),   # D (Collaborative)
    0.10 * (1 + collab_preference*0.8)    # E (Highly collaborative) - collaborative types prefer this
  )
  pm <- pmax(pm, 0.02)
  pm <- pm / rowSums(pm)
  candidates$q_q12_research_culture <- vsample(c("A","B","C","D","E"), pm)
  
  # Publication venues
  pm <- cbind(rep(0.05, n), rep(0.15, n), rep(0.30, n),
              rep(0.30, n), rep(0.20, n))
  candidates$q_q13_publication_venues <- vsample(c("A","B","C","D","E"), pm)
  
  candidates$q_q15_medical_school_proximity <- sample(c("0","1"), n, replace=TRUE, prob=c(0.60,0.40))

  candidates %>% dplyr::select(-quality_pctl)
}


# =============================================================================
# prepare_departments — Initialize department attributes and weight vectors
# =============================================================================
prepare_departments <- function(sampled_depts, questions, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  num_q <- length(questions$numerical); cat_q <- length(questions$categorical)
  n_total_q <- num_q + cat_q
  result <- sampled_depts %>%
    mutate(prestige_tier = tier,
           q1_geographic_setting = as.character(q1_geographic_setting),
           q2_region = as.character(q2_region), q3_airport_proximity = as.character(q3_airport_proximity),
           q5_dual_career = as.character(q5_dual_career), q7_typical_startup = as.character(q7_typical_startup),
           q8_guaranteed_summer = as.character(q8_guaranteed_summer),
           q9_typical_teaching_load = as.character(q9_typical_teaching_load),
           q10_course_types = as.character(q10_course_types),
           q11_mentoring_program = as.character(q11_mentoring_program),
           q12_research_culture = as.character(q12_research_culture),
           q13_publication_venues = as.character(q13_publication_venues),
           q15_medical_school_proximity = as.character(q15_medical_school_proximity))
  n_depts <- nrow(result)
  q_names <- c(questions$numerical, names(questions$categorical))
  
  # Question index masks
  geo_idx <- which(q_names %in% c("q1_geographic_setting","q2_region","q3_airport_proximity"))
  col_idx <- which(q_names == "q4_cost_of_living")
  research_idx <- which(q_names %in% c("q14_phd_student_ratio","q12_research_culture","q13_publication_venues"))
  teaching_idx <- which(q_names %in% c("q9_typical_teaching_load","q10_course_types"))
  comp_idx <- which(q_names %in% c("q6_typical_salary_range","q7_typical_startup","q8_guaranteed_summer"))
  t1_idx <- which(q_names %in% c("q13_publication_venues","q12_research_culture"))
  t4_idx <- which(q_names %in% c("q1_geographic_setting","q4_cost_of_living"))
  
  # Sample department types
  dept_types <- sample(1:4, n_depts, replace = TRUE, prob = c(0.25, 0.25, 0.30, 0.20))
  dept_tiers <- as.character(result$tier)
  
  # Build weight matrix (n_depts x n_total_q)
  weight_matrix <- matrix(1, nrow = n_depts, ncol = n_total_q)
  
  # Type 1: uniform random weights
  mask1 <- dept_types == 1
  if (any(mask1)) weight_matrix[mask1, ] <- matrix(runif(sum(mask1) * n_total_q, 0.7, 1.3), nrow = sum(mask1))
  
  # Type 2: geography-focused
  mask2 <- dept_types == 2
  n2 <- sum(mask2)
  if (n2 > 0) {
    if (length(geo_idx) > 0) weight_matrix[mask2, geo_idx] <- matrix(runif(n2 * length(geo_idx), 2.0, 4.0), nrow = n2)
    if (length(col_idx) > 0) weight_matrix[mask2, col_idx] <- runif(n2, 1.5, 2.5)
  }
  
  # Type 3: research-focused
  mask3 <- dept_types == 3
  n3 <- sum(mask3)
  if (n3 > 0) {
    if (length(research_idx) > 0) weight_matrix[mask3, research_idx] <- matrix(runif(n3 * length(research_idx), 2.5, 4.5), nrow = n3)
    if (length(teaching_idx) > 0) weight_matrix[mask3, teaching_idx] <- matrix(runif(n3 * length(teaching_idx), 1.5, 2.5), nrow = n3)
  }
  
  # Type 4: compensation-focused
  mask4 <- dept_types == 4
  n4 <- sum(mask4)
  if (n4 > 0 && length(comp_idx) > 0) {
    weight_matrix[mask4, comp_idx] <- matrix(runif(n4 * length(comp_idx), 2.0, 4.0), nrow = n4)
  }
  
  # Tier-specific adjustments
  mask_t1 <- dept_tiers == "Tier 1"
  nt1 <- sum(mask_t1)
  if (nt1 > 0 && length(t1_idx) > 0) {
    weight_matrix[mask_t1, t1_idx] <- weight_matrix[mask_t1, t1_idx] * matrix(runif(nt1 * length(t1_idx), 1.3, 1.8), nrow = nt1)
  }
  mask_t4 <- dept_tiers == "Tier 4"
  nt4 <- sum(mask_t4)
  if (nt4 > 0 && length(t4_idx) > 0) {
    weight_matrix[mask_t4, t4_idx] <- weight_matrix[mask_t4, t4_idx] * matrix(runif(nt4 * length(t4_idx), 1.2, 1.6), nrow = nt4)
  }
  
  row_sums <- rowSums(weight_matrix)
  weight_matrix <- weight_matrix / row_sums
  
  weight_list <- lapply(seq_len(n_depts), function(j) weight_matrix[j, ])
  result$weight_vector <- weight_list
  
  colnames(weight_matrix) <- q_names
  cat("\n=== DEPARTMENT WEIGHT SUMMARY ===\n")
  cat("Mean weights by question:\n"); print(round(colMeans(weight_matrix), 3))
  cat("\nWeight range (min-max) by question:\n")
  for (i in 1:ncol(weight_matrix)) cat(sprintf("  %s: [%.3f, %.3f]\n", q_names[i], min(weight_matrix[,i]), max(weight_matrix[,i])))
  result
}


generate_yearly_hiring_schedule <- function(n_departments, n_years, departments, seed = 123) {
  set.seed(seed)
  hire_prob_by_tier <- c("Tier 1"=0.6,"Tier 2"=0.6,"Tier 3"=0.6,"Tier 4"=0.6)
  tiers <- as.character(departments$prestige_tier)
  probs <- hire_prob_by_tier[tiers]
  probs[is.na(probs)] <- 0.5
  hiring_schedule <- matrix(rbinom(n_departments * n_years, size = 1, prob = rep(probs, n_years)),
                            nrow = n_departments, ncol = n_years)
  cat("\n=== YEARLY HIRING SCHEDULE SUMMARY ===\n")
  cat("Total department-years:", n_departments * n_years, "\n")
  cat("Total hiring events:", sum(hiring_schedule), "\n")
  cat("Overall hiring rate:", round(mean(hiring_schedule), 3), "\n\n")
  for (tier in c("Tier 1","Tier 2","Tier 3","Tier 4")) {
    tier_idx <- which(tiers == tier)
    if (length(tier_idx) > 0) cat(tier, ": ", sum(hiring_schedule[tier_idx,]), " hires over ",
                                  length(tier_idx)*n_years, " dept-years (rate = ", round(mean(hiring_schedule[tier_idx,]),3), ")\n", sep="")
  }
  hiring_schedule
}


# =============================================================================
# Neural network components — Acceptance probability models (torch)
# =============================================================================
acceptance_net <- nn_module(
  "AcceptancePredictionNet",
  initialize = function(n_cont_features, embedding_specs, hidden_dims = c(16, 8)) {
    self$n_cont <- n_cont_features; self$embedding_specs <- embedding_specs
    self$embeddings <- nn_module_list(); total_embed_dim <- 0
    if (length(embedding_specs) > 0) for (i in seq_along(embedding_specs)) {
      spec <- embedding_specs[[i]]; self$embeddings$append(nn_embedding(spec$n_levels, spec$embed_dim))
      total_embed_dim <- total_embed_dim + spec$embed_dim
    }
    input_dim <- n_cont_features + total_embed_dim
    self$layers <- nn_module_list(); self$batch_norms <- nn_module_list()
    prev_dim <- input_dim
    for (hidden_dim in hidden_dims) {
      self$layers$append(nn_linear(prev_dim, hidden_dim))
      self$batch_norms$append(nn_batch_norm1d(hidden_dim, momentum = 0.005))
      prev_dim <- hidden_dim
    }
    self$output <- nn_linear(prev_dim, 1)
  },
  forward = function(x_cont, x_cat_list = list()) {
    h <- x_cont
    if (length(x_cat_list) > 0 && length(self$embeddings) > 0) {
      embedded_features <- lapply(seq_along(x_cat_list), function(i) self$embeddings[[i]](x_cat_list[[i]]))
      h <- torch_cat(c(list(h), embedded_features), dim = 2)
    }
    for (i in seq_along(self$layers)) {
      h <- self$layers[[i]](h); h <- self$batch_norms[[i]](h); h <- torch_relu(h)
    }
    self$output(h)
  }
)

prepare_nn_data <- function(data, questions, include_fit = TRUE, include_questions = TRUE) {
  cont_features <- c("s_j")
  if ("v_i_bar" %in% colnames(data)) { data$log_v_i_bar <- log(data$v_i_bar + 1e-6); cont_features <- c(cont_features, "log_v_i_bar") }
  if (include_fit && "f_j" %in% colnames(data)) { data$log_f_j <- log(data$f_j + 1e-6); cont_features <- c(cont_features, "log_f_j") }
  if (include_questions) {
    for (q in questions$numerical) { if (q %in% colnames(data)) cont_features <- c(cont_features, q) }
    for (q in questions$numerical) { qcol <- paste0("q_", q); if (qcol %in% colnames(data)) cont_features <- c(cont_features, qcol) }
  }
  x_cat_list <- list(); embedding_specs <- list(); cat_i <- 0L
  if (include_questions && length(questions$categorical) > 0) {
    for (q_name in names(questions$categorical)) {
      levels <- as.character(questions$categorical[[q_name]])
      if (is.null(levels) || length(levels)==0) levels <- c("unknown")
      n_levels <- length(levels); embed_dim <- min(8, max(2, ceiling(n_levels/2)))
      qcol <- paste0("q_", q_name)
      if (qcol %in% colnames(data)) {
        cat_i <- cat_i + 1L; raw_values <- data[[qcol]]
        if (q_name == "q2_region") {
          # Extract first region before comma
          rv_char <- as.character(raw_values)
          rv_char[is.na(rv_char) | rv_char == ""] <- levels[1]
          first_comma <- regexpr(",", rv_char, fixed = TRUE)
          values <- ifelse(first_comma > 0, trimws(substr(rv_char, 1, first_comma - 1)), trimws(rv_char))
          values[values == "" | is.na(values)] <- levels[1]
        } else values <- as.character(raw_values)
        x_cat_list[[cat_i]] <- safe_categorical_to_index(values, levels, paste0("cand_", q_name))
        embedding_specs[[cat_i]] <- list(n_levels=n_levels, embed_dim=embed_dim)
      }
      if (q_name %in% colnames(data)) {
        cat_i <- cat_i + 1L
        x_cat_list[[cat_i]] <- safe_categorical_to_index(as.character(data[[q_name]]), levels, paste0("dept_", q_name))
        embedding_specs[[cat_i]] <- list(n_levels=n_levels, embed_dim=embed_dim)
      }
    }
  }
  cont_features <- intersect(cont_features, colnames(data))
  if (length(cont_features)==0) stop("No continuous features available for neural network")
  cont_matrix <- as.matrix(data[, cont_features, drop=FALSE])
  # Replace NAs with column means
  na_mask <- is.na(cont_matrix)
  if (any(na_mask)) {
    col_means <- colMeans(cont_matrix, na.rm = TRUE)
    col_means[is.na(col_means)] <- 0
    cont_matrix[na_mask] <- col_means[col(cont_matrix)[na_mask]]
  }
  x_cont_tensor <- torch_tensor(cont_matrix, dtype=torch_float())
  x_cat_tensors <- list()
  for (i in seq_along(x_cat_list)) {
    idx_vec <- x_cat_list[[i]]; n_lvl <- embedding_specs[[i]]$n_levels
    if (any(idx_vec<1|idx_vec>n_lvl, na.rm=TRUE)) idx_vec <- pmax(1L, pmin(idx_vec, n_lvl))
    x_cat_tensors[[i]] <- torch_tensor(as.integer(idx_vec), dtype=torch_long())
  }
  list(x_cont=x_cont_tensor, x_cat=x_cat_tensors, embedding_specs=embedding_specs, n_cont_features=ncol(x_cont_tensor))
}

initialize_department_model <- function(questions, include_fit = TRUE, include_questions = TRUE) {
  embedding_specs <- list()
  if (include_questions && length(questions$categorical) > 0) {
    for (q_name in names(questions$categorical)) {
      levels <- questions$categorical[[q_name]]; if (is.null(levels)||length(levels)==0) levels <- c("unknown")
      n_levels <- length(levels); embed_dim <- min(8, max(2, ceiling(n_levels/2)))
      embedding_specs[[length(embedding_specs)+1]] <- list(n_levels=n_levels, embed_dim=embed_dim)
      embedding_specs[[length(embedding_specs)+1]] <- list(n_levels=n_levels, embed_dim=embed_dim)
    }
  }
  n_cont <- 2L; if (include_fit) n_cont <- n_cont + 1L
  if (include_questions) n_cont <- n_cont + length(questions$numerical) * 2
  model <- acceptance_net(n_cont, embedding_specs)
  list(model=model, optimizer=optim_adam(model$parameters, lr=0.001), historical_data=tibble(),
       questions=questions, is_trained=FALSE, include_fit=include_fit, include_questions=include_questions)
}

train_department_model <- function(dept_model, train_data, n_epochs = 60,
                                   include_fit = dept_model$include_fit,
                                   include_questions = dept_model$include_questions,
                                   seed = NULL) {
  train_data <- dplyr::filter(train_data, offered == 1L)
  if (nrow(train_data) < 1) return(dept_model)
  if (!is.null(seed)) { set.seed(seed); torch::torch_manual_seed(seed) }
  
  nn_data <- prepare_nn_data(train_data, dept_model$questions,
                             include_fit = include_fit,
                             include_questions = include_questions)
  y_tensor <- torch::torch_tensor(train_data$accepted,
                                  dtype = torch::torch_float())$unsqueeze(2)
  
  n_train <- nrow(train_data)
  use_validation <- n_train >= 40
  
  if (use_validation) {
    val_idx   <- sample.int(n_train, size = ceiling(0.2 * n_train))
    train_idx <- setdiff(seq_len(n_train), val_idx)
    x_train     <- nn_data$x_cont[train_idx, ]
    y_train     <- y_tensor[train_idx, ]
    x_val       <- nn_data$x_cont[val_idx, ]
    y_val       <- y_tensor[val_idx, ]
    x_cat_train <- lapply(nn_data$x_cat, function(t) t[train_idx])
    x_cat_val   <- lapply(nn_data$x_cat, function(t) t[val_idx])
  } else {
    x_train     <- nn_data$x_cont
    y_train     <- y_tensor
    x_cat_train <- nn_data$x_cat
  }
  
  optimizer <- optim_adam(dept_model$model$parameters, lr = 0.002)
  scheduler <- lr_multiplicative(optimizer, lr_lambda = function(epoch) 0.98)
  best_val_loss <- Inf; patience <- 20; patience_counter <- 0; best_state <- NULL
  
  dept_model$model$train()
  for (epoch in 1:n_epochs) {
    log_odds <- dept_model$model(x_train, x_cat_train)
    
    loss <- nn_bce_with_logits_loss()(log_odds, y_train)
    
    l2_loss <- 0
    for (param in dept_model$model$parameters)
      l2_loss <- l2_loss + torch_sum(param^2)
    total_loss <- loss + 0.01 * l2_loss
    
    optimizer$zero_grad()
    total_loss$backward()
    nn_utils_clip_grad_norm_(dept_model$model$parameters, max_norm = 1.0)
    optimizer$step()
    scheduler$step()
    
    if (use_validation && epoch %% 5 == 0) {
      dept_model$model$eval()
      with_no_grad({
        val_log_odds <- dept_model$model(x_val, x_cat_val)
        val_loss <- as.numeric(nn_bce_with_logits_loss()(val_log_odds, y_val))
      })
      dept_model$model$train()
      if (val_loss < best_val_loss) {
        best_val_loss   <- val_loss
        patience_counter <- 0
        best_state <- lapply(dept_model$model$parameters,
                             function(p) p$clone()$detach())
      } else {
        patience_counter <- patience_counter + 1
        if (patience_counter >= patience) {
          if (!is.null(best_state)) {
            pl <- dept_model$model$parameters
            for (i in seq_along(pl)) pl[[i]]$data <- best_state[[i]]$data
          }
          break
        }
      }
    }
  }
  
  dept_model$is_trained <- TRUE
  dept_model
}

# =============================================================================
# train_department_model_from_tensors — Trains from pre-computed tensors
#
# Same training logic as train_department_model, but accepts pre-computed
# tensors + bootstrap indices instead of a dataframe.
# =============================================================================
train_department_model_from_tensors <- function(dept_model, x_cont_all, x_cat_all, y_all,
                                                boot_indices, n_epochs = 5, seed = NULL) {
  n_boot <- length(boot_indices)
  if (n_boot < 1) return(dept_model)
  if (!is.null(seed)) { set.seed(seed); torch::torch_manual_seed(seed) }
  
  idx_tensor <- torch::torch_tensor(as.integer(boot_indices), dtype = torch::torch_long())
  x_cont  <- x_cont_all$index_select(1, idx_tensor)
  x_cat   <- lapply(x_cat_all, function(t) t$index_select(1, idx_tensor))
  y_tensor <- y_all$index_select(1, idx_tensor)
  
  n_train <- n_boot
  use_validation <- n_train >= 40
  
  if (use_validation) {
    val_idx   <- sample.int(n_train, size = ceiling(0.2 * n_train))
    train_idx <- setdiff(seq_len(n_train), val_idx)
    
    # Convert to 1-indexed torch tensors for indexing
    train_idx_t <- torch::torch_tensor(as.integer(train_idx), dtype = torch::torch_long())
    val_idx_t   <- torch::torch_tensor(as.integer(val_idx), dtype = torch::torch_long())
    
    x_train     <- x_cont$index_select(1, train_idx_t)
    y_train     <- y_tensor$index_select(1, train_idx_t)
    x_val       <- x_cont$index_select(1, val_idx_t)
    y_val       <- y_tensor$index_select(1, val_idx_t)
    x_cat_train <- lapply(x_cat, function(t) t$index_select(1, train_idx_t))
    x_cat_val   <- lapply(x_cat, function(t) t$index_select(1, val_idx_t))
  } else {
    x_train     <- x_cont
    y_train     <- y_tensor
    x_cat_train <- x_cat
  }
  
  optimizer <- optim_adam(dept_model$model$parameters, lr = 0.002)
  scheduler <- lr_multiplicative(optimizer, lr_lambda = function(epoch) 0.98)
  best_val_loss <- Inf; patience <- 20; patience_counter <- 0; best_state <- NULL
  
  dept_model$model$train()
  for (epoch in 1:n_epochs) {
    log_odds <- dept_model$model(x_train, x_cat_train)
    loss <- nn_bce_with_logits_loss()(log_odds, y_train)
    
    l2_loss <- 0
    for (param in dept_model$model$parameters)
      l2_loss <- l2_loss + torch_sum(param^2)
    total_loss <- loss + 0.01 * l2_loss
    
    optimizer$zero_grad()
    total_loss$backward()
    nn_utils_clip_grad_norm_(dept_model$model$parameters, max_norm = 1.0)
    optimizer$step()
    scheduler$step()
    
    if (use_validation && epoch %% 5 == 0) {
      dept_model$model$eval()
      with_no_grad({
        val_log_odds <- dept_model$model(x_val, x_cat_val)
        val_loss <- as.numeric(nn_bce_with_logits_loss()(val_log_odds, y_val))
      })
      dept_model$model$train()
      if (val_loss < best_val_loss) {
        best_val_loss    <- val_loss
        patience_counter <- 0
        best_state <- lapply(dept_model$model$parameters,
                             function(p) p$clone()$detach())
      } else {
        patience_counter <- patience_counter + 1
        if (patience_counter >= patience) {
          if (!is.null(best_state)) {
            pl <- dept_model$model$parameters
            for (i in seq_along(pl)) pl[[i]]$data <- best_state[[i]]$data
          }
          break
        }
      }
    }
  }
  
  dept_model$is_trained <- TRUE
  dept_model
}

# =============================================================================
# predict_acceptance_probability — Bootstrap acceptance prediction
# =============================================================================
predict_acceptance_probability <- function(dept_model, applicant_data, n_bootstrap = 50,
                                           include_fit = dept_model$include_fit,
                                           include_questions = dept_model$include_questions,
                                           seed = NULL, participation_rate = NULL) {
  if (!is.null(seed)) { set.seed(seed); torch::torch_manual_seed(seed) }
  is_baseline <- is.null(participation_rate) || participation_rate == 0 || !include_fit
  if (!dept_model$is_trained || nrow(applicant_data) == 0) {
    base <- applicant_data %>% mutate(
      tier_gap = pmax(0, dept_tier - cand_tier),
      baseline_accept = case_when(tier_gap==0~0.6,tier_gap==1~0.3,tier_gap==2~0.12,tier_gap>=3~0.05),
      questionnaire_base = baseline_accept,
      questionnaire_accept = pmin(0.90, questionnaire_base + 0.15 * (f_j - 0.5)),
      pi_pred = if (is_baseline) pmax(0.10,pmin(0.90,baseline_accept+rnorm(n(),0,0.02)))
      else pmax(0.10,pmin(0.90,questionnaire_accept+rnorm(n(),0,0.02))))
    mu <- pmin(pmax(base$pi_pred, 1e-5), 1-1e-5)
    res <- base %>% mutate(pi_var=0) %>% dplyr::select(-tier_gap,-baseline_accept,-questionnaire_base,-questionnaire_accept)
    attr(res, "pi_draws") <- matrix(mu, nrow=1); return(res)
  }
  hist_data <- dept_model$historical_data %>% filter(offered == 1L)
  if (nrow(hist_data) < 10) {
    nn_data <- tryCatch(prepare_nn_data(applicant_data, dept_model$questions, include_fit=include_fit, include_questions=include_questions), error=function(e) NULL)
    if (is.null(nn_data)) {
      base <- applicant_data %>% mutate(tier_gap=pmax(0,dept_tier-cand_tier),
                                        pi_pred=pmax(0.10,pmin(0.90,case_when(tier_gap==0~0.6,tier_gap==1~0.3,tier_gap==2~0.12,TRUE~0.05)+rnorm(n(),0,0.02))),pi_var=0) %>% dplyr::select(-tier_gap)
      attr(base,"pi_draws") <- matrix(base$pi_pred, nrow=1); return(base)
    }
    dept_model$model$eval()
    with_no_grad({ log_odds <- dept_model$model(nn_data$x_cont, nn_data$x_cat); pi_mean <- as.numeric(torch_sigmoid(log_odds)$squeeze()$to(device="cpu")) })
    pi_mean <- pmin(pmax(pi_mean,1e-5),1-1e-5)
    res <- applicant_data %>% mutate(pi_pred=pi_mean, pi_var=0)
    attr(res,"pi_draws") <- matrix(pi_mean, nrow=1); return(res)
  }
  n_hist <- nrow(hist_data); pi_draws_matrix <- matrix(NA_real_, nrow=n_bootstrap, ncol=nrow(applicant_data))
  
  # Cache inference data
  nn_data_cached <- tryCatch(prepare_nn_data(applicant_data, dept_model$questions, include_fit=include_fit, include_questions=include_questions), error=function(e) NULL)
  if (is.null(nn_data_cached)) {
    tg <- pmax(0, applicant_data$dept_tier-applicant_data$cand_tier)
    mu <- pmin(pmax(case_when(tg==0~0.55,tg==1~0.45,tg==2~0.35,TRUE~0.25)+rnorm(nrow(applicant_data),0,0.05),1e-5),1-1e-5)
    res <- applicant_data %>% mutate(pi_pred=mu, pi_var=0)
    attr(res,"pi_draws") <- matrix(mu, nrow=1); return(res)
  }
  
  nn_data_train_all <- tryCatch(
    prepare_nn_data(hist_data, dept_model$questions,
                    include_fit = include_fit, include_questions = include_questions),
    error = function(e) NULL)
  y_all <- torch::torch_tensor(hist_data$accepted, dtype = torch::torch_float())$unsqueeze(2)
  
  trained_state <- if (dept_model$is_trained) lapply(dept_model$model$parameters, function(p) p$clone()$detach()) else NULL
  
  # Allocate temp model once outside the loop
  temp_model <- initialize_department_model(dept_model$questions, include_fit=include_fit, include_questions=include_questions)
  
  for (b in 1:n_bootstrap) {
    boot_indices <- sample.int(n_hist, n_hist, replace = TRUE)
    
    # Reset weights to trained state
    if (!is.null(trained_state)) {
      tp <- temp_model$model$parameters
      with_no_grad({ for (i in seq_along(tp)) tp[[i]]$copy_(trained_state[[i]]) })
    }
    temp_model$is_trained <- FALSE
    
    # Train from cached tensors
    if (!is.null(nn_data_train_all)) {
      temp_model <- tryCatch(
        train_department_model_from_tensors(
          temp_model, nn_data_train_all$x_cont, nn_data_train_all$x_cat, y_all,
          boot_indices, n_epochs = 5, seed = seed + b),
        error = function(e) temp_model)
    } else {
      boot_data <- hist_data[boot_indices, ]
      temp_model$historical_data <- boot_data
      temp_model <- tryCatch(
        train_department_model(temp_model, boot_data, n_epochs = 5,
                               include_fit = include_fit, include_questions = include_questions,
                               seed = seed + b),
        error = function(e) temp_model)
    }
    
    if (!temp_model$is_trained) {
      tg <- pmax(0, applicant_data$dept_tier-applicant_data$cand_tier)
      pi_draws_matrix[b,] <- pmin(pmax(case_when(tg==0~0.6,tg==1~0.3,tg==2~0.12,TRUE~0.05)+rnorm(nrow(applicant_data),0,0.05),1e-5),1-1e-5); next
    }
    temp_model$model$eval()
    with_no_grad({ log_odds <- temp_model$model(nn_data_cached$x_cont, nn_data_cached$x_cat); pi_b <- as.numeric(torch_sigmoid(log_odds)$squeeze()$to(device="cpu")) })
    pi_draws_matrix[b,] <- pmin(pmax(pi_b,1e-5),1-1e-5)
  }
  pi_means <- colMeans(pi_draws_matrix, na.rm = TRUE)
  pi_vars <- colMeans(sweep(pi_draws_matrix, 2, pi_means)^2, na.rm = TRUE) * nrow(pi_draws_matrix) / (nrow(pi_draws_matrix) - 1)
  res <- applicant_data %>% mutate(pi_pred = pi_means, pi_var = pi_vars)
  attr(res,"pi_draws") <- pi_draws_matrix; res
}

# =============================================================================
# make_repeated_rank_draws — Legacy fallback for ranking uncertainty
#
# Called when NN bootstrap pi_draws are unavailable. The primary path
# propagates pi_draws directly via compute_pairwise_lower_ranks.
# =============================================================================
make_repeated_rank_draws <- function(applicant_data, L = 200, tuple_size = NULL,
                                     seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(applicant_data)
  if (!"U_det" %in% names(applicant_data))
    applicant_data <- applicant_data %>% mutate(U_det = department_utility(s_j, v_i_bar, f_j))
  mu <- pmin(pmax(applicant_data$pi_pred, 1e-5), 1-1e-5)
  Uhat_full <- applicant_data$U_det * mu
  if (is.null(tuple_size)) idx <- seq_len(n) else { M_use <- min(tuple_size, n); idx <- order(Uhat_full, decreasing=TRUE)[seq_len(M_use)] }
  M <- length(idx); U_hat <- Uhat_full[idx]

  if (!"U_true" %in% names(applicant_data))
    applicant_data <- applicant_data %>% mutate(dept_tier=dept_tier%||%4L, cand_tier=cand_tier%||%4L,
                                                U_true=department_utility(s_j,v_i_bar,f_j,dept_tier,cand_tier))
  U_true_sub <- applicant_data$U_true[idx]; resid <- U_true_sub - U_hat
  if (all(!is.finite(resid)) || sd(resid,na.rm=TRUE)==0)
    return(list(U_draws=matrix(rep(U_hat,each=L),nrow=L,ncol=M), idx=idx, U_hat=U_hat))
  boot_idx <- sample.int(M, size=M*L, replace=TRUE)
  eps_mat <- matrix(resid[boot_idx], nrow=L, ncol=M)
  U_draws <- sweep(eps_mat, 2, U_hat, "+"); U_draws[U_draws<0] <- 0
  list(U_draws=U_draws, idx=idx, U_hat=U_hat)
}


# =============================================================================
# compute_c_alpha_from_draws — Confidence threshold for rankings
# =============================================================================
compute_c_alpha_from_draws <- function(U_hat, U_draws, alpha = 0.05, ridge = 0.01) {
  stopifnot(is.numeric(U_hat), is.matrix(U_draws))
  n <- length(U_hat); B <- nrow(U_draws)
  if (n <= 1L || B <= 1L) return(list(c_alpha=0, sigma_hat=matrix(0,n,n)))
  
  # Compute sigma_hat via Var(X_i - X_l) = Var(X_i) + Var(X_l) - 2*Cov(X_i,X_l)
  col_means <- colMeans(U_draws, na.rm = TRUE)
  col_vars <- colMeans(sweep(U_draws, 2, col_means)^2, na.rm = TRUE) * nrow(U_draws) / (nrow(U_draws) - 1)
  col_vars[!is.finite(col_vars)] <- 0
  cov_mat <- cov(U_draws, use="pairwise.complete.obs"); cov_mat[!is.finite(cov_mat)] <- 0
  var_diff <- outer(col_vars, col_vars, "+") - 2*cov_mat
  var_diff[var_diff < 0] <- 0
  sigma_hat <- pmax(sqrt(var_diff), ridge)
  
  # delta_hat matrix
  delta_hat_mat <- outer(U_hat, U_hat, "-")
  
  # Compute max Z per bootstrap draw
  if (n <= 200) {
    # Extract all upper-triangle pairs
    pairs <- which(upper.tri(sigma_hat), arr.ind=TRUE); n_pairs <- nrow(pairs)
    delta_b_pairs <- U_draws[, pairs[,1], drop=FALSE] - U_draws[, pairs[,2], drop=FALSE]
    delta_hat_pairs <- delta_hat_mat[pairs]; sigma_pairs <- sigma_hat[pairs]
    z_pairs <- sweep(sweep(delta_b_pairs, 2, delta_hat_pairs, "-"), 2, sigma_pairs, "/")
    z_pairs[!is.finite(z_pairs)] <- -Inf
    Zb <- z_pairs[, 1]
    if (ncol(z_pairs) > 1) for (cc in 2:ncol(z_pairs)) Zb <- pmax(Zb, z_pairs[, cc])
  } else {
    # Row-chunked for larger n
    Zb <- rep(-Inf, B)
    for (i in 1:(n-1)) {
      l_idx <- (i+1):n
      z_batch <- sweep(U_draws[,i]-U_draws[,l_idx,drop=FALSE], 2, delta_hat_mat[i,l_idx], "-")
      z_batch <- sweep(z_batch, 2, sigma_hat[i,l_idx], "/")
      z_batch[!is.finite(z_batch)] <- -Inf
      row_max <- z_batch[, 1]
      if (ncol(z_batch) > 1) for (cc in 2:ncol(z_batch)) row_max <- pmax(row_max, z_batch[, cc])
      Zb <- pmax(Zb, row_max)
    }
  }
  Zb[!is.finite(Zb)] <- -Inf
  c_alpha <- stats::quantile(Zb, probs=1-alpha, na.rm=TRUE, names=FALSE)
  list(c_alpha=as.numeric(c_alpha), sigma_hat=sigma_hat)
}


# =============================================================================
# compute_pairwise_lower_ranks — Pairwise ranking with uncertainty
# =============================================================================
compute_pairwise_lower_ranks <- function(applicant_data, n_bootstrap=20, tuple_size=NULL,
                                         alpha=0.05, seed=NULL,
                                         pi_draws=NULL) {
  if (!"U_det" %in% names(applicant_data) || !"U_hat" %in% names(applicant_data))
    applicant_data <- applicant_data %>% mutate(U_det=department_utility(s_j, v_i_bar, f_j),
                                                U_hat=U_det*pmin(pmax(pi_pred,1e-5),1-1e-5))

  
  if (is.null(pi_draws)) pi_draws <- attr(applicant_data, "pi_draws")
  
  n <- nrow(applicant_data)
  Uhat_full <- applicant_data$U_hat
  if (is.null(tuple_size)) idx <- seq_len(n) else {
    M_use <- min(tuple_size, n); idx <- order(Uhat_full, decreasing=TRUE)[seq_len(M_use)]
  }
  U_hat <- Uhat_full[idx]
  
  if (!is.null(pi_draws) && is.matrix(pi_draws) && nrow(pi_draws) > 1) {
    U_det_sub <- applicant_data$U_det[idx]
    pi_draws_sub <- pi_draws[, idx, drop=FALSE]
    U_draws <- sweep(pi_draws_sub, 2, U_det_sub, "*")
  } else {
    repack <- make_repeated_rank_draws(applicant_data, L=n_bootstrap, tuple_size=tuple_size,
                                       seed=seed)
    idx <- repack$idx; U_draws <- repack$U_draws; U_hat <- repack$U_hat
  }
  cand_ids <- applicant_data$cand_id[idx]; M <- length(U_hat)
  if (M <= 1L) return(tibble(cand_id=cand_ids, rank_lower=1L, pair_score=0L, mean_rank=1) %>%
                        right_join(applicant_data %>% dplyr::select(cand_id), by="cand_id") %>%
                        mutate(rank_lower=ifelse(is.na(rank_lower),Inf,rank_lower), mean_rank=ifelse(is.na(mean_rank),Inf,mean_rank)))
  
  cal <- compute_c_alpha_from_draws(U_hat, U_draws, alpha=alpha, ridge=0.0001)
  c_alpha <- cal$c_alpha; sigma_hat <- cal$sigma_hat
  if (is.na(c_alpha) || !is.finite(c_alpha)) c_alpha <- 1.96
  
  # z_mat[l,i] = (U_hat[l]-U_hat[i])/sigma[l,i]
  delta_mat <- outer(U_hat, U_hat, "-")
  safe_sigma <- sigma_hat; diag(safe_sigma) <- 1
  z_mat <- delta_mat / safe_sigma; z_mat[!is.finite(z_mat)] <- 0; diag(z_mat) <- 0
  losses_cert <- as.integer(colSums(z_mat > c_alpha))
  wins_cert <- as.integer(colSums(z_mat < -c_alpha))
  rank_lower <- 1L + losses_cert
  pair_score <- wins_cert - losses_cert
  
  R <- t(apply(-U_draws, 1, rank, ties.method="average"))
  mean_rank <- colMeans(R, na.rm=TRUE); mean_rank[is.na(mean_rank)] <- M
  
  tibble(cand_id=cand_ids, rank_lower=rank_lower, pair_score=pair_score, mean_rank=mean_rank) %>%
    right_join(applicant_data %>% dplyr::select(cand_id), by="cand_id") %>%
    mutate(rank_lower=ifelse(is.na(rank_lower),Inf,rank_lower), mean_rank=ifelse(is.na(mean_rank),Inf,mean_rank))
}


select_interviews_sure_screening <- function(applicant_data, k_j, h_j, alpha=0.05, n_bootstrap=20,
                                             tuple_size=NULL, seed=NULL, tier_width=0.1, pi_draws=NULL) {
  n <- nrow(applicant_data)
  if (n == 0L) return(integer()); if (n <= k_j) return(applicant_data$cand_id)
  if (!"U_det" %in% names(applicant_data) || !"U_hat" %in% names(applicant_data))
    applicant_data <- applicant_data %>% dplyr::mutate(U_det=department_utility(s_j, v_i_bar, f_j),
                                                       U_hat=U_det*pmin(pmax(pi_pred,1e-5),1-1e-5))
  rank_tbl <- compute_pairwise_lower_ranks(applicant_data, n_bootstrap, tuple_size, alpha, seed, pi_draws=pi_draws)
  applicant_data <- applicant_data %>% dplyr::select(-dplyr::any_of(c("rank_lower","pair_score","mean_rank"))) %>%
    dplyr::left_join(rank_tbl %>% dplyr::select(cand_id,rank_lower,pair_score,mean_rank), by="cand_id")
  sure_idx <- which(applicant_data$rank_lower <= k_j)
  if (length(sure_idx) >= k_j) {
    selected_ids <- applicant_data %>% dplyr::slice(sure_idx) %>%
      dplyr::arrange(mean_rank, dplyr::desc(pair_score), dplyr::desc(U_hat)) %>%
      dplyr::slice_head(n=k_j) %>% dplyr::pull(cand_id)
  } else {
    sure_ids <- applicant_data$cand_id[sure_idx]; remaining_slots <- k_j-length(sure_idx)
    remaining_pool <- applicant_data %>% dplyr::filter(!(cand_id %in% sure_ids))
    if (nrow(remaining_pool)==0) { selected_ids <- sure_ids
    } else {
      sure_U_hat <- applicant_data %>% dplyr::filter(cand_id %in% sure_ids) %>% dplyr::pull(U_hat)
      if (length(sure_U_hat) > 0) {
        tier_threshold <- min(sure_U_hat, na.rm=TRUE) - tier_width
        tier_candidates <- remaining_pool %>% dplyr::filter(U_hat >= tier_threshold)
        if (nrow(tier_candidates) > 0) {
          additional_ids <- tier_candidates %>%
            dplyr::arrange(dplyr::desc(U_hat), cand_id) %>%
            dplyr::slice_head(n=remaining_slots) %>%
            dplyr::pull(cand_id)
        } else additional_ids <- remaining_pool %>% dplyr::arrange(dplyr::desc(U_hat)) %>% dplyr::slice_head(n=remaining_slots) %>% dplyr::pull(cand_id)
      } else additional_ids <- remaining_pool %>% dplyr::arrange(dplyr::desc(U_hat)) %>% dplyr::slice_head(n=remaining_slots) %>% dplyr::pull(cand_id)
      selected_ids <- c(sure_ids, additional_ids)
    }
  }
  attr(selected_ids, "sure_ids") <- applicant_data$cand_id[sure_idx]; selected_ids
}


generate_applications <- function(candidates, departments, questions, ...)
  tibble(cand_id=candidates$cand_id) %>% tidyr::crossing(tibble(dept_id=departments$dept_id))



add_participation_signals <- function(candidates, departments, questions,
                                      dept_weights = NULL,
                                      nonparticipant_penalty = 0.03) {
  n_candidates <- nrow(candidates)
  n_departments <- nrow(departments)
  if (n_candidates == 0L) {
    return(candidates %>%
             mutate(
               pool_alignment = numeric(0),
               pool_alignment_norm = numeric(0),
               v_pctl = numeric(0),
               participation_score = numeric(0),
               nonparticipant_fit_proxy = numeric(0)
             ))
  }
  if (n_departments == 0L) {
    v_pctl <- if (n_candidates > 1L)
      (rank(candidates$v_i_bar, ties.method = "average") - 1) / (n_candidates - 1)
    else rep(0.5, n_candidates)
    return(candidates %>%
             mutate(
               pool_alignment = 0.5,
               pool_alignment_norm = 0.5,
               v_pctl = v_pctl,
               participation_score = 0.5 * v_pctl,
               nonparticipant_fit_proxy = pmin(0.95, pmax(0.05, 0.5 - nonparticipant_penalty))
             ))
  }
  
  if (is.null(dept_weights) || length(dept_weights) != n_departments) {
    dept_weights <- rep(1, n_departments)
  }
  dept_weights <- as.numeric(dept_weights)
  dept_weights[!is.finite(dept_weights) | dept_weights < 0] <- 0
  if (sum(dept_weights) <= 0) dept_weights <- rep(1, n_departments)
  dept_weights <- dept_weights / sum(dept_weights)
  
  fit_mat <- matrix(NA_real_, nrow = n_candidates, ncol = n_departments)
  for (j in seq_len(n_departments)) {
    fit_mat[, j] <- compute_f_j(candidates, departments[j, , drop = FALSE], questions)
  }
  
  pool_alignment <- as.vector(fit_mat %*% dept_weights)
  pool_alignment_norm <- norm01(pool_alignment)
  v_pctl <- if (n_candidates > 1L)
    (rank(candidates$v_i_bar, ties.method = "average") - 1) / (n_candidates - 1)
  else rep(0.5, n_candidates)
  
  # Strategic opt-out: high-quality candidates with below-median pool alignment
  # can have some incentive to avoid revealing weak fit.
  align_mid <- stats::median(pool_alignment_norm, na.rm = TRUE)
  misalignment <- pmax(0, align_mid - pool_alignment_norm)
  opt_out_value <- misalignment * v_pctl
  
  participation_score <- 0.70 * pool_alignment_norm + 0.30 * v_pctl - 0.35 * opt_out_value
  nonparticipant_fit_proxy <- pmin(0.95, pmax(0.05, pool_alignment - nonparticipant_penalty))
  
  candidates %>%
    mutate(
      pool_alignment = pool_alignment,
      pool_alignment_norm = pool_alignment_norm,
      v_pctl = v_pctl,
      participation_score = participation_score,
      nonparticipant_fit_proxy = nonparticipant_fit_proxy
    )
}

generate_nested_participation_assignments <- function(n_candidates = NULL,
                                                      participation_rates = c(0.05,0.20,0.50,0.90),
                                                      seed = 123,
                                                      candidates = NULL,
                                                      departments = NULL,
                                                      questions = NULL,
                                                      dept_weights = NULL) {
  rates <- sort(unique(participation_rates))
  rates <- rates[is.finite(rates) & rates >= 0 & rates <= 1]
  participation_sets <- list()
  participation_sets[["0"]] <- integer(0)
  
  if (!is.null(candidates)) {
    if (!"cand_id" %in% names(candidates)) candidates$cand_id <- seq_len(nrow(candidates))
    
    if (!"participation_score" %in% names(candidates)) {
      if (!is.null(departments) && !is.null(questions)) {
        candidates <- add_participation_signals(
          candidates = candidates,
          departments = departments,
          questions = questions,
          dept_weights = dept_weights
        )
      } else if ("v_i_bar" %in% names(candidates)) {
        n_local <- nrow(candidates)
        v_pctl <- if (n_local > 1L)
          (rank(candidates$v_i_bar, ties.method = "average") - 1) / (n_local - 1)
        else rep(0.5, n_local)
        candidates <- candidates %>%
          mutate(
            pool_alignment = v_pctl,
            pool_alignment_norm = v_pctl,
            v_pctl = v_pctl,
            participation_score = v_pctl,
            nonparticipant_fit_proxy = pmin(0.95, pmax(0.05, 0.5 + 0.25 * v_pctl))
          )
      } else {
        set.seed(seed)
        candidates <- candidates %>% mutate(participation_score = runif(nrow(candidates)))
      }
    }
    
    if (!"v_i_bar" %in% names(candidates)) candidates$v_i_bar <- 0
    ordered_ids <- candidates %>%
      arrange(desc(participation_score), desc(v_i_bar), cand_id) %>%
      pull(cand_id)
    
    n_local <- length(ordered_ids)
    for (rate in rates[rates > 0 & rates < 1]) {
      take_n <- floor(n_local * rate)
      participation_sets[[as.character(rate)]] <- if (take_n > 0L) ordered_ids[seq_len(take_n)] else integer(0)
    }
    participation_sets[["1"]] <- ordered_ids
    
    attr(participation_sets, "candidate_scores") <- candidates %>%
      dplyr::select(cand_id, participation_score, pool_alignment, v_pctl, nonparticipant_fit_proxy)
    return(participation_sets)
  }
  
  if (is.null(n_candidates))
    stop("Either n_candidates or candidates must be supplied.")
  
  set.seed(seed)
  propensities <- runif(n_candidates)
  cand_ids <- seq_len(n_candidates)
  sorted_idx <- order(propensities, decreasing = TRUE)
  
  for (rate in rates[rates > 0 & rates < 1]) {
    take_n <- floor(n_candidates * rate)
    participation_sets[[as.character(rate)]] <- if (take_n > 0L) cand_ids[sorted_idx[seq_len(take_n)]] else integer(0)
  }
  participation_sets[["1"]] <- cand_ids
  participation_sets
}

compute_nonparticipant_fj <- function(applicant_data,
                                      participation_rate = 0,
                                      proxy_col = "nonparticipant_fit_proxy",
                                      fallback_value = 0.5) {
  n_part <- sum(applicant_data$participates, na.rm = TRUE)
  if (participation_rate == 0 || n_part == 0) {
    min_f_j_part <- NA_real_
  } else {
    min_f_j_part <- min(applicant_data$f_j[applicant_data$participates], na.rm = TRUE)
  }
  
  dplyr::case_when(
    !applicant_data$participates & !is.na(min_f_j_part) ~ min_f_j_part,
    !applicant_data$participates & is.na(min_f_j_part)  ~ fallback_value,
    TRUE ~ applicant_data$f_j
  )
}

print_hiring_allocation <- function(all_results, year, participation_rate) {
  if (nrow(all_results)==0) { cat("\
  No results in year", year, "\
"); return(invisible(NULL)) }
  hires <- all_results %>% filter(accepted==1)
  if (nrow(hires)==0) { cat("\
  No hires in year", year, "\
"); return(invisible(NULL)) }
  hire_matrix <- hires %>%
    count(cand_tier, dept_tier) %>% complete(cand_tier=1:4, dept_tier=1:4, fill=list(n=0)) %>%
    pivot_wider(names_from=dept_tier, values_from=n, names_prefix="Dept_T", values_fill=0) %>%
    arrange(cand_tier) %>% mutate(Total=Dept_T1+Dept_T2+Dept_T3+Dept_T4, Cand_Tier=paste0("Tier ",cand_tier)) %>%
    dplyr::select(Cand_Tier, Dept_T1, Dept_T2, Dept_T3, Dept_T4, Total)
  col_totals <- tibble(Cand_Tier="Total", Dept_T1=sum(hire_matrix$Dept_T1), Dept_T2=sum(hire_matrix$Dept_T2),
                       Dept_T3=sum(hire_matrix$Dept_T3), Dept_T4=sum(hire_matrix$Dept_T4), Total=sum(hire_matrix$Total))
  hire_matrix <- bind_rows(hire_matrix, col_totals)
  cat(sprintf("\
  \u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557\
"))
  cat(sprintf("  \u2551  HIRING ALLOCATION MATRIX - Year %d (\u03c1 = %.0f%%)            \u2551\
", year, participation_rate*100))
  cat(sprintf("  \u2560\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2563\
"))
  cat(sprintf("  \u2551 %-10s \u2502 %7s %7s %7s %7s \u2502 %7s \u2551\
","","Dept T1","Dept T2","Dept T3","Dept T4","Total"))
  cat(sprintf("  \u2560\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2563\
"))
  for (i in 1:(nrow(hire_matrix)-1)) { row <- hire_matrix[i,]
  cat(sprintf("  \u2551 %-10s \u2502 %7d %7d %7d %7d \u2502 %7d \u2551\
", row$Cand_Tier, row$Dept_T1, row$Dept_T2, row$Dept_T3, row$Dept_T4, row$Total)) }
  cat(sprintf("  \u2560\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2563\
"))
  totals <- hire_matrix[nrow(hire_matrix),]
  cat(sprintf("  \u2551 %-10s \u2502 %7d %7d %7d %7d \u2502 %7d \u2551\
", totals$Cand_Tier, totals$Dept_T1, totals$Dept_T2, totals$Dept_T3, totals$Dept_T4, totals$Total))
  cat(sprintf("  \u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d\
"))
  cat("\
  Mean f_j by allocation:\
")
  fit_summary <- hires %>% group_by(cand_tier,dept_tier) %>% summarise(n=n(),mean_f_j=mean(f_j,na.rm=TRUE),.groups="drop") %>% filter(n>0) %>% arrange(cand_tier,dept_tier)
  for (i in 1:nrow(fit_summary)) { row <- fit_summary[i,]; cat(sprintf("    Cand T%d \u2192 Dept T%d: n=%3d, mean_f_j=%.3f\
", row$cand_tier, row$dept_tier, row$n, row$mean_f_j)) }
  invisible(hire_matrix)
}

add_dept_info_to_learning_data <- function(ld_selected, dept_info) {
  n <- nrow(ld_selected)
  ld_selected$q4_cost_of_living <- dept_info$q4_cost_of_living
  ld_selected$q6_typical_salary_range <- dept_info$q6_typical_salary_range
  ld_selected$q14_phd_student_ratio <- dept_info$q14_phd_student_ratio
  cat_qs <- c("q1_geographic_setting","q2_region","q3_airport_proximity","q5_dual_career","q7_typical_startup",
              "q8_guaranteed_summer","q9_typical_teaching_load","q10_course_types","q11_mentoring_program",
              "q12_research_culture","q13_publication_venues","q15_medical_school_proximity")
  for (q in cat_qs) ld_selected[[q]] <- as.character(dept_info[[q]])
  ld_selected
}

# =============================================================================
# simulate_market_year_adaptive_sequential — Core market year simulation
# =============================================================================
simulate_market_year_adaptive_sequential <- function(candidates, departments, questions, year,
                                                     dept_models_pairwise, participants_this_year, yearly_hiring_schedule,
                                                     participation_rate=0, alpha=0.05, n_bootstrap=20, tuple_size=NULL,
                                                     shortlist_enabled=TRUE,
                                                     collect_ranking_panel=TRUE, cand_tier_col="quality_tier", max_offer_rounds=5, seed=NULL,
                                                     verbose=TRUE) {
  if (!is.null(seed)) set.seed(seed)
  candidates <- candidates %>% mutate(participates = cand_id %in% participants_this_year)
  tier_to_int <- function(x) as.integer(factor(as.character(x), levels=c("Tier 1","Tier 2","Tier 3","Tier 4")))
  applications <- generate_applications(candidates, departments, questions)
  n_departments <- nrow(departments)
  learning_data_pairwise <- vector("list", n_departments)
  diag_list <- vector("list", n_departments)
  interviewed_chunks <- vector("list", n_departments)
  rank_panel_chunks <- if (collect_ranking_panel) vector("list", n_departments) else NULL
  if (verbose) cat(sprintf("\
=== YEAR %d: Interview Selection Phase ===\
", year))
  for (j in 1:n_departments) {
    dept <- departments[j, ]; h_j_this_year <- yearly_hiring_schedule[j, year]; k_j_this_year <- 5L*h_j_this_year
    if (h_j_this_year == 0L) {
      da <- applications %>% filter(dept_id==dept$dept_id)
      if (nrow(da) > 0) { aa <- da %>% left_join(candidates, by="cand_id") %>% mutate(s_j=dept$s_j)
      diag_list[[j]] <- aa %>% mutate(year=year, dept_id=dept$dept_id, strategy="pairwise", pi_pred=NA_real_, U_true=NA_real_, U_hat=NA_real_, r_true=NA_real_, r_hat=NA_real_, interviewed=0L, considered=0L, h_j=0L, k_j=0L) }
      next
    }
    da <- applications %>% filter(dept_id==dept$dept_id); if (nrow(da)==0) next
    apps_all <- da %>% left_join(candidates, by="cand_id") %>% mutate(s_j=dept$s_j)
    if (!cand_tier_col %in% names(apps_all)) apps_all[[cand_tier_col]] <- factor("Tier 4", levels=c("Tier 1","Tier 2","Tier 3","Tier 4"))
    else apps_all[[cand_tier_col]] <- factor(as.character(apps_all[[cand_tier_col]]), levels=c("Tier 1","Tier 2","Tier 3","Tier 4"))
    apps_all <- apps_all %>% mutate(dept_tier=tier_to_int(dept$prestige_tier %||% "Tier 4"), cand_tier=tier_to_int(.data[[cand_tier_col]]))
    # Batch compute f_j for all candidates vs this department
    apps_all$f_j <- compute_f_j(apps_all, dept, questions)
    if (shortlist_enabled) {
      allow_map <- list("Tier 1"=c("Tier 1"),"Tier 2"=c("Tier 1","Tier 2"),"Tier 3"=c("Tier 1","Tier 2","Tier 3"),"Tier 4"=c("Tier 1","Tier 2","Tier 3","Tier 4"))
      pt <- as.character(dept$prestige_tier %||% "Tier 4"); allowed <- allow_map[[pt]] %||% allow_map[["Tier 4"]]
      considered_mask <- as.character(apps_all[[cand_tier_col]]) %in% allowed
    } else considered_mask <- rep(TRUE, nrow(apps_all))
    applicant_data <- apps_all[considered_mask, , drop=FALSE]
    if (nrow(applicant_data)==0) { diag_list[[j]] <- apps_all %>% mutate(year=year,dept_id=dept$dept_id,strategy="pairwise",pi_pred=NA_real_,U_true=NA_real_,U_hat=NA_real_,r_true=NA_real_,r_hat=NA_real_,interviewed=0L,considered=0L,h_j=h_j_this_year,k_j=k_j_this_year); next }
    applicant_data <- applicant_data %>% mutate(row_id=row_number())
    applicant_data_pw <- applicant_data %>% mutate(
      f_j_used = compute_nonparticipant_fj(
        applicant_data = applicant_data,
        participation_rate = participation_rate
      ))
    actual_f_j <- applicant_data_pw$f_j; ad_tmp <- applicant_data_pw; ad_tmp$f_j <- applicant_data_pw$f_j_used
    for (q in questions$numerical) ad_tmp[[q]] <- dept[[q]]
    for (qn in names(questions$categorical)) ad_tmp[[qn]] <- as.character(dept[[qn]])
    applicant_data_pw <- predict_acceptance_probability(dept_models_pairwise[[j]], ad_tmp, n_bootstrap=n_bootstrap, seed=j*1000+year, participation_rate=participation_rate)
    # Save pi_draws before dplyr operations strip the attribute
    pi_draws_pw <- attr(applicant_data_pw, "pi_draws")
    applicant_data_pw$f_j <- actual_f_j
    U_det_pw <- department_utility(applicant_data_pw$s_j, applicant_data_pw$v_i_bar, applicant_data_pw$f_j_used, applicant_data_pw$dept_tier, applicant_data_pw$cand_tier)
    applicant_data_pw <- applicant_data_pw %>% dplyr::mutate(f_j_used=applicant_data_pw$f_j_used, U_det=U_det_pw, U_hat=U_det*pmin(pmax(pi_pred,1e-5),1-1e-5))
    U_true_pw <- department_utility(applicant_data_pw$s_j, applicant_data_pw$v_i_bar, applicant_data_pw$f_j, applicant_data_pw$dept_tier, applicant_data_pw$cand_tier)
    applicant_data_pw$U_true <- U_true_pw; applicant_data_pw$r_true <- rank(-U_true_pw, ties.method="average"); applicant_data_pw$r_point <- rank(-applicant_data_pw$U_hat, ties.method="average")
    if (collect_ranking_panel) { rpair <- compute_pairwise_lower_ranks(applicant_data_pw, n_bootstrap, tuple_size, alpha, pi_draws=pi_draws_pw)
    applicant_data_pw <- applicant_data_pw %>% dplyr::left_join(rpair, by="cand_id") %>% dplyr::mutate(r_pair_lb=rank_lower) %>% dplyr::select(-rank_lower) }
    interviewed_ids_pw <- select_interviews_sure_screening(applicant_data_pw, k_j=k_j_this_year, h_j=h_j_this_year, alpha=alpha, n_bootstrap=n_bootstrap, tuple_size=tuple_size, pi_draws=pi_draws_pw)
    applicant_data_pw$interviewed_flag <- as.integer(applicant_data_pw$cand_id %in% interviewed_ids_pw)
    if (collect_ranking_panel) {
      rank_panel_chunks[[j]] <- applicant_data_pw %>%
        dplyr::transmute(year, dept_id=dept$dept_id, strategy="pairwise", cand_id, s_j, v_i_bar, f_j,
                         participates, U_true, U_hat, r_true, r_point,
                         r_pair_lb=dplyr::if_else(is.finite(r_pair_lb),r_pair_lb,NA_real_),
                         interviewed=interviewed_flag, k_j=k_j_this_year, h_j=h_j_this_year)
    }
    idpw <- applicant_data_pw %>% dplyr::filter(cand_id %in% interviewed_ids_pw) %>% dplyr::mutate(interviewed=1L, year=year, dept_id=dept$dept_id, strategy="pairwise", h_j=h_j_this_year, k_j=k_j_this_year)
    if (nrow(idpw)>0) interviewed_chunks[[j]] <- idpw
    diag_list[[j]] <- apps_all %>% dplyr::mutate(considered=as.integer(cand_id %in% applicant_data$cand_id), interviewed=as.integer(cand_id %in% interviewed_ids_pw), h_j=h_j_this_year, k_j=k_j_this_year) %>%
      dplyr::left_join(applicant_data_pw %>% dplyr::select(cand_id,pi_pred,U_true,U_hat,r_true), by="cand_id") %>%
      dplyr::transmute(year=year,dept_id=dept$dept_id,strategy="pairwise",cand_id,s_j,v_i_bar,f_j,pi_pred,U_true,U_hat,r_true,interviewed,considered,participates,h_j,k_j)
  }
  if (verbose) cat(sprintf("\
=== YEAR %d: Sequential Offer Resolution ===\
", year))
  all_interviewed <- bind_rows(interviewed_chunks)
  rank_panel <- if (collect_ranking_panel) bind_rows(rank_panel_chunks) else tibble()
  if (nrow(all_interviewed)>0) {
    all_results <- resolve_offers_sequential(interviewed_data=all_interviewed, departments=departments, questions=questions, max_rounds=max_offer_rounds, seed=year, temperature=0.05, noise_sd=0.01, shortlist_enabled=shortlist_enabled, year=year, verbose=verbose)
    if (verbose) print_hiring_allocation(all_results, year, participation_rate)
  } else all_results <- tibble()
  for (j in 1:nrow(departments)) {
    ld_pw <- all_results %>% dplyr::filter(dept_id==departments$dept_id[j], strategy=="pairwise", interviewed==1L)
    if (nrow(ld_pw)>0) learning_data_pairwise[[j]] <- ld_pw %>% dplyr::select(year,dept_id,cand_id,s_j,v_i_bar,f_j,offered,accepted,dplyr::starts_with("q_")) %>% add_dept_info_to_learning_data(departments[j,])
  }
  diag_list <- diag_list[!vapply(diag_list, is.null, logical(1))]
  list(results=all_results, learning_data_pairwise=learning_data_pairwise, rank_panel=rank_panel, diagnostics=list(applicant_level=bind_rows(diag_list)))
}


# =============================================================================
# resolve_offers_sequential — Two-round offer resolution with scramble
#
# Round 1: Each department offers its top h_j interviewed candidates;
#   candidates accept their best offer and reject all others.
# Round 2 (scramble): Departments with unfilled positions offer their
#   next-best remaining interviewed candidate.
resolve_offers_sequential <- function(interviewed_data, departments, questions,
                                      all_candidates = NULL, max_rounds = 3, seed = NULL,
                                      temperature = 0.3, noise_sd = 0.4,
                                      shortlist_enabled = TRUE, year = NA_integer_,
                                      verbose = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  if (nrow(interviewed_data) == 0)
    return(interviewed_data %>% dplyr::mutate(offered = 0L, accepted = 0L, offer_round = NA_integer_))
  
  interviewed_data <- interviewed_data %>%
    dplyr::mutate(offered = 0L, accepted = 0L, offer_round = NA_integer_)
  
  # Candidate utilities
  if (verbose) cat("  Calculating candidate utilities with unified f_j...\n")
  interviewed_data <- interviewed_data %>%
    dplyr::mutate(cand_util = candidate_utility(v_i_bar, s_j, f_j))
  
  # Department quotas and indexed vectors
  dept_quota <- interviewed_data %>%
    dplyr::distinct(dept_id, h_j) %>%
    tibble::deframe()
  dept_names <- names(dept_quota)
  dept_ids_data <- interviewed_data$dept_id
  cand_ids_data <- interviewed_data$cand_id
  u_hat_data <- interviewed_data$U_hat
  dept_row_groups <- split(seq_len(nrow(interviewed_data)), as.character(dept_ids_data))
  data_keys <- paste(interviewed_data$dept_id, interviewed_data$cand_id, sep = "_")
  id_util_lookup <- setNames(interviewed_data$cand_util,
                             paste(interviewed_data$cand_id, interviewed_data$dept_id, sep = "_"))
  
  # Helper: candidates choose best offer in a given round 
  choose_best_offers <- function(offers_this_round) {
    if (nrow(offers_this_round) == 0) return(tibble())
    offer_cands <- unique(offers_this_round$cand_id)
    offer_cand_depts <- split(offers_this_round$dept_id, offers_this_round$cand_id)
    accepted_list <- vector("list", length(offer_cands))
    n_acc <- 0L
    for (cand in offer_cands) {
      cand_char <- as.character(cand)
      depts <- offer_cand_depts[[cand_char]]
      keys <- paste(cand, depts, sep = "_")
      utils <- as.numeric(id_util_lookup[keys])
      if (length(utils) == 0 || all(!is.finite(utils))) next
      best_i <- which.max(utils)  # deterministic tie break: first max
      n_acc <- n_acc + 1L
      accepted_list[[n_acc]] <- tibble(
        cand_id = cand,
        dept_id = depts[best_i],
        round = offers_this_round$round[1]
      )
    }
    if (n_acc == 0L) return(tibble())
    dplyr::bind_rows(accepted_list[1:n_acc])
  }

  # Round 1: departments make initial offers
  if (verbose) cat("  Offer Round 1 (initial interviewed offers)...\n")
  offer_dept_list <- vector("list", length(dept_names))
  offer_cand_list <- vector("list", length(dept_names))
  ol <- 0L
  for (d_char in dept_names) {
    h_j <- as.integer(dept_quota[[d_char]])
    if (is.na(h_j) || h_j <= 0L) next
    rows_d <- dept_row_groups[[d_char]]
    if (length(rows_d) == 0) next
    ord <- order(u_hat_data[rows_d], decreasing = TRUE)
    top_n <- min(h_j, length(rows_d))
    selected_rows <- rows_d[ord[seq_len(top_n)]]
    ol <- ol + 1L
    offer_dept_list[[ol]] <- dept_ids_data[selected_rows]
    offer_cand_list[[ol]] <- cand_ids_data[selected_rows]
  }
  offers_round1 <- if (ol > 0) {
    tibble(
      dept_id = unlist(offer_dept_list[1:ol], use.names = FALSE),
      cand_id = unlist(offer_cand_list[1:ol], use.names = FALSE),
      round = 1L
    )
  } else tibble()
  
  if (nrow(offers_round1) > 0) {
    offer_keys <- paste(offers_round1$dept_id, offers_round1$cand_id, sep = "_")
    match_idx <- match(offer_keys, data_keys)
    valid_matches <- !is.na(match_idx)
    if (any(valid_matches)) {
      interviewed_data$offered[match_idx[valid_matches]] <- 1L
      interviewed_data$offer_round[match_idx[valid_matches]] <- 1L
    }
    accepted_round1 <- choose_best_offers(offers_round1)
    if (nrow(accepted_round1) > 0) {
      accept_keys <- paste(accepted_round1$dept_id, accepted_round1$cand_id, sep = "_")
      accept_idx <- match(accept_keys, data_keys)
      valid_accept <- !is.na(accept_idx)
      if (any(valid_accept))
        interviewed_data$accepted[accept_idx[valid_accept]] <- 1L
    }
  } else {
    accepted_round1 <- tibble()
  }
  
  accepted_counts <- table(interviewed_data$dept_id[interviewed_data$accepted == 1L])
  filled_vec <- as.integer(accepted_counts[dept_names])
  filled_vec[is.na(filled_vec)] <- 0L
  dept_positions_filled <- tibble::tibble(
    dept_id = type.convert(dept_names, as.is = TRUE),
    h_j = as.integer(dept_quota[dept_names]),
    filled = filled_vec
  )
  total_positions <- sum(dept_positions_filled$h_j)
  total_filled_r1 <- sum(dept_positions_filled$filled)
  total_remaining <- total_positions - total_filled_r1
  if (verbose) cat(sprintf("  After round 1: %d positions filled, %d positions unfilled\n",
                           total_filled_r1, total_remaining))

  # Round 2 scramble: unfilled departments make one additional offer
  if (total_remaining > 0 && max_rounds >= 2L) {
    if (verbose) cat(sprintf("\n  Offer Round 2 (scramble): %d departments/positions still unfilled\n", total_remaining))
    accepted_candidates <- interviewed_data$cand_id[interviewed_data$accepted == 1L]
    offer2_dept <- vector("list", length(dept_names))
    offer2_cand <- vector("list", length(dept_names))
    o2 <- 0L
    for (d_char in dept_names) {
      h_j <- as.integer(dept_quota[[d_char]])
      if (is.na(h_j) || h_j <= 0L) next
      filled_d <- dept_positions_filled$filled[match(type.convert(d_char, as.is = TRUE), dept_positions_filled$dept_id)]
      filled_d <- ifelse(length(filled_d) == 0 || is.na(filled_d), 0L, filled_d)
      if (filled_d >= h_j) next
      rows_d <- dept_row_groups[[d_char]]
      if (length(rows_d) == 0) next
      is_available <- !(cand_ids_data[rows_d] %in% accepted_candidates) &
        interviewed_data$offered[rows_d] == 0L
      eligible_rows <- rows_d[is_available]
      if (length(eligible_rows) == 0) next
      best_row <- eligible_rows[which.max(u_hat_data[eligible_rows])]
      o2 <- o2 + 1L
      offer2_dept[[o2]] <- dept_ids_data[best_row]
      offer2_cand[[o2]] <- cand_ids_data[best_row]
    }
    
    offers_round2 <- if (o2 > 0) {
      tibble(
        dept_id = unlist(offer2_dept[1:o2], use.names = FALSE),
        cand_id = unlist(offer2_cand[1:o2], use.names = FALSE),
        round = 2L
      )
    } else tibble()
    
    if (nrow(offers_round2) > 0) {
      offer2_keys <- paste(offers_round2$dept_id, offers_round2$cand_id, sep = "_")
      offer2_idx <- match(offer2_keys, data_keys)
      valid_offer2 <- !is.na(offer2_idx)
      if (any(valid_offer2)) {
        interviewed_data$offered[offer2_idx[valid_offer2]] <- 1L
        interviewed_data$offer_round[offer2_idx[valid_offer2]] <- 2L
      }
      accepted_round2 <- choose_best_offers(offers_round2)
      if (nrow(accepted_round2) > 0) {
        accept2_keys <- paste(accepted_round2$dept_id, accepted_round2$cand_id, sep = "_")
        accept2_idx <- match(accept2_keys, data_keys)
        valid_accept2 <- !is.na(accept2_idx)
        if (any(valid_accept2))
          interviewed_data$accepted[accept2_idx[valid_accept2]] <- 1L
      }
      if (verbose) cat(sprintf("  Round 2 issued %d offers\n", nrow(offers_round2)))
    } else if (verbose) {
      cat("  Round 2 issued 0 offers (no remaining interviewed candidates available).\n")
    }
  } else if (total_remaining > 0 && max_rounds < 2L && verbose) {
    cat(sprintf("  Round 2 scramble skipped because max_rounds=%d\n", max_rounds))
  }

  accepted_counts_final <- table(interviewed_data$dept_id[interviewed_data$accepted == 1L])
  filled_final <- as.integer(accepted_counts_final[dept_names])
  filled_final[is.na(filled_final)] <- 0L
  fs <- tibble::tibble(
    dept_id = type.convert(dept_names, as.is = TRUE),
    h_j = as.integer(dept_quota[dept_names]),
    filled = filled_final
  ) %>% dplyr::mutate(unfilled = h_j - filled)
  if (verbose) {
    cat("\n  Final hiring summary:\n")
    cat(sprintf("    Total positions: %d\n", sum(fs$h_j)))
    cat(sprintf("    Positions filled: %d\n", sum(fs$filled)))
    cat(sprintf("    Positions unfilled: %d\n", sum(fs$unfilled)))
  }

  interviewed_data
}
# =============================================================================
# BURN-IN PHASE
# =============================================================================
# =============================================================================
# run_burn_in_phase — Burn-in phase to learn department priors
#
# Returns burn-in models used as initialization for sim-phase models,
# so that sim-phase models inherit the burn-in prior's knowledge.
# =============================================================================
run_burn_in_phase <- function(departments, questions, n_candidates = 500, burn_in_years = 10,
                              yearly_candidate_cohorts = NULL, yearly_hiring_schedule = NULL, seed = 123,
                              alpha = 0.05, n_bootstrap = 20, tuple_size = NULL,
                              cand_tier_cutpoints = c(0.10, 0.25, 0.50),
                              max_offer_rounds = 5, print_diagnostics = TRUE,
                              return_yearly_objects = FALSE,
                              retrain_interval = 3L, min_offered_to_train = 10L) {
  set.seed(seed); torch::torch_manual_seed(seed); n_departments <- nrow(departments)
  cat("\n", strrep("=", 70), "\nBURN-IN PHASE: Learning Priors (Incremental)\n", strrep("=", 70), "\n")
  cat(sprintf("Running %d years of baseline simulation...\n", burn_in_years))
  cat(sprintf("  Incremental retraining every %d years (min %d offered obs)\n",
              retrain_interval, min_offered_to_train))
  if (is.null(yearly_hiring_schedule))
    yearly_hiring_schedule <- generate_yearly_hiring_schedule(n_departments, burn_in_years, departments, seed + 500)
  if (is.null(yearly_candidate_cohorts)) {
    yearly_candidate_cohorts <- vector("list", burn_in_years)
    for (year in 1:burn_in_years)
      yearly_candidate_cohorts[[year]] <- generate_candidates(n_candidates, questions, seed = seed + year)
  }

  mdl <- purrr::map(1:n_departments,
    ~initialize_department_model(questions, include_fit = TRUE, include_questions = FALSE))

  res_all <- list(); cand_roster_all <- list()
  for (year in 1:burn_in_years) {
    cat(sprintf("\n--- Burn-in Year %d/%d ---\n", year, burn_in_years))
    candidates <- yearly_candidate_cohorts[[year]] %>%
      mutate(quality_tier = assign_tiers_from_quantiles(v_i_bar, cutpoints = cand_tier_cutpoints))
    cand_roster_all[[year]] <- candidates %>%
      mutate(participates = FALSE) %>%
      transmute(year = !!year, cand_id, quality_tier, v_i_bar, participates)
    out <- simulate_market_year_adaptive_sequential(candidates, departments, questions, year,
                                                    dept_models_pairwise = mdl, participants_this_year = integer(0),
                                                    yearly_hiring_schedule = yearly_hiring_schedule, participation_rate = 0, alpha = alpha,
                                                    n_bootstrap = n_bootstrap, tuple_size = tuple_size,
                                                    shortlist_enabled = TRUE, collect_ranking_panel = FALSE,
                                                    cand_tier_col = "quality_tier", max_offer_rounds = max_offer_rounds, seed = year)
    res_all[[year]] <- out$results

    # Accumulate learning data — keep real f_j
    for (j in 1:n_departments) {
      if (!is.null(out$learning_data_pairwise[[j]]) && nrow(out$learning_data_pairwise[[j]]) > 0) {
        mdl[[j]]$historical_data <- bind_rows(mdl[[j]]$historical_data,
                                              out$learning_data_pairwise[[j]])
      }
    }

    # Incremental retraining: retrain models every retrain_interval years
    if (year %% retrain_interval == 0L) {
      n_retrained <- 0L
      for (j in 1:n_departments) {
        n_offered <- if (nrow(mdl[[j]]$historical_data) > 0 && "offered" %in% names(mdl[[j]]$historical_data))
          sum(mdl[[j]]$historical_data$offered == 1L, na.rm = TRUE) else 0L
        if (n_offered >= min_offered_to_train) {
          mdl[[j]] <- tryCatch(
            train_department_model(mdl[[j]], mdl[[j]]$historical_data,
                                   n_epochs = 60, include_fit = TRUE,
                                   include_questions = FALSE,
                                   seed = j * 1000 + year),
            error = function(e) mdl[[j]])
          n_retrained <- n_retrained + 1L
        }
      }
      cat(sprintf("  [Year %d] Retrained %d/%d department models\n",
                  year, n_retrained, n_departments))
    }
  }

  # Final training pass on all accumulated data
  cat("\n", strrep("=", 70), "\nFinal training on all burn-in data...\n", strrep("=", 70), "\n")
  for (j in 1:n_departments) {
    n_offered <- if (nrow(mdl[[j]]$historical_data) > 0 && "offered" %in% names(mdl[[j]]$historical_data))
      sum(mdl[[j]]$historical_data$offered == 1L, na.rm = TRUE) else 0L
    if (n_offered >= min_offered_to_train) {
      mdl[[j]] <- tryCatch(
        train_department_model(mdl[[j]], mdl[[j]]$historical_data,
                               n_epochs = 100, include_fit = TRUE,
                               include_questions = FALSE, seed = j * 1000),
        error = function(e) mdl[[j]])
      cat(sprintf("  Dept %d: Trained on %d observations (%d offered)\n",
                  j, nrow(mdl[[j]]$historical_data), n_offered))
    } else {
      cat(sprintf("  Dept %d: Insufficient offered data (%d offered)\n", j, n_offered))
    }
  }

  total_hires <- sum(vapply(res_all, function(r) sum(r$accepted == 1, na.rm = TRUE), numeric(1)))
  total_offers <- sum(vapply(res_all, function(r) sum(r$offered == 1, na.rm = TRUE), numeric(1)))
  cat(sprintf("\nBurn-in complete. Offers: %d | Hires: %d | Yield: %.1f%%\n",
              total_offers, total_hires, 100 * total_hires / max(total_offers, 1)))

  burn_in_historical <- purrr::map(mdl, ~.x$historical_data)

  burn_in_out <- list(
    trained_models = mdl,
    burn_in_results = bind_rows(res_all),
    burn_in_historical = burn_in_historical,
    cand_roster = bind_rows(cand_roster_all)
  )
  if (isTRUE(return_yearly_objects)) {
    burn_in_out$yearly_candidate_cohorts <- yearly_candidate_cohorts
    burn_in_out$yearly_hiring_schedule <- yearly_hiring_schedule
  }
  burn_in_out
}

# =============================================================================
# predict_acceptance_probability_with_learned_prior
#
# Blends sim-phase model predictions with burned-in learned priors.
# When the sim-phase model is untrained, uses the learned prior directly.
# =============================================================================
predict_acceptance_probability_with_learned_prior <- function(dept_model, applicant_data,
                                                              learned_prior_model = NULL,
                                                              n_bootstrap = 50,
                                                              include_fit = dept_model$include_fit,
                                                              include_questions = dept_model$include_questions,
                                                              seed = NULL,
                                                              participation_rate = NULL) {
  if (!is.null(seed)) { set.seed(seed); torch::torch_manual_seed(seed) }
  is_baseline <- is.null(participation_rate) || participation_rate == 0 || !include_fit
  blend_denom <- if (is_baseline) 40 else 15
  n_applicants <- nrow(applicant_data)
  
  # Learned prior from burn-in model
  if (!is.null(learned_prior_model) && learned_prior_model$is_trained) {
    prior_include_fit <- isTRUE(learned_prior_model$include_fit)
    prior_include_questions <- isTRUE(learned_prior_model$include_questions)
    nn_data_prior <- tryCatch(
      prepare_nn_data(applicant_data, learned_prior_model$questions,
                      include_fit = prior_include_fit,
                      include_questions = prior_include_questions),
      error = function(e) NULL)
    if (!is.null(nn_data_prior)) {
      learned_prior_model$model$eval()
      with_no_grad({
        lo <- learned_prior_model$model(nn_data_prior$x_cont, nn_data_prior$x_cat)
        pi_prior <- as.numeric(torch_sigmoid(lo)$squeeze()$to(device = "cpu"))
      })
      pi_prior <- pmin(pmax(pi_prior, 0.05), 0.95)
    } else {
      tg <- pmax(0, applicant_data$dept_tier - applicant_data$cand_tier)
      pi_prior <- dplyr::case_when(tg == 0 ~ 0.6, tg == 1 ~ 0.3,
                                   tg == 2 ~ 0.12, TRUE ~ 0.05)
    }
  } else {
    tg <- pmax(0, applicant_data$dept_tier - applicant_data$cand_tier)
    pi_prior <- dplyr::case_when(tg == 0 ~ 0.6, tg == 1 ~ 0.3,
                                 tg == 2 ~ 0.12, TRUE ~ 0.05)
  }
  
  # Tier-gap fallback
  if (!dept_model$is_trained || n_applicants == 0) {
    pi_point <- pmax(0.10, pmin(0.90, pi_prior + rnorm(n_applicants, 0, 0.02)))
    res <- applicant_data %>% dplyr::mutate(pi_pred = pi_point, pi_var = 0)
    attr(res, "pi_draws") <- matrix(pi_point, nrow = 1)
    return(res)
  }
  
  hist_data <- dept_model$historical_data %>% dplyr::filter(offered == 1L)
  n_hist <- nrow(hist_data)
  
  # Insufficient data for bootstrap
  if (n_hist < 10) {
    nn_data <- tryCatch(
      prepare_nn_data(applicant_data, dept_model$questions,
                      include_fit = include_fit,
                      include_questions = include_questions),
      error = function(e) NULL)
    if (!is.null(nn_data)) {
      dept_model$model$eval()
      with_no_grad({
        lo <- dept_model$model(nn_data$x_cont, nn_data$x_cat)
        pi_model <- as.numeric(torch_sigmoid(lo)$squeeze()$to(device = "cpu"))
      })
      pi_model <- pmin(pmax(pi_model, 1e-5), 1 - 1e-5)
      mw <- pmin(1, n_hist / blend_denom)
      pi_blended <- mw * pi_model + (1 - mw) * pi_prior
    } else {
      pi_blended <- pmax(0.10, pmin(0.90, pi_prior + rnorm(n_applicants, 0, 0.02)))
    }
    res <- applicant_data %>% dplyr::mutate(pi_pred = pi_blended, pi_var = 0)
    attr(res, "pi_draws") <- matrix(pi_blended, nrow = 1)
    return(res)
  }
  
  # Full bootstrap
  mw <- pmin(1, n_hist / blend_denom)
  pi_draws_matrix <- matrix(NA_real_, nrow = n_bootstrap, ncol = n_applicants)
  
  # Cache inference data
  nn_data_cached <- tryCatch(
    prepare_nn_data(applicant_data, dept_model$questions,
                    include_fit = include_fit,
                    include_questions = include_questions),
    error = function(e) NULL)
  if (is.null(nn_data_cached)) {
    pi_point <- pmax(0.10, pmin(0.90, pi_prior + rnorm(n_applicants, 0, 0.02)))
    res <- applicant_data %>% dplyr::mutate(pi_pred = pi_point, pi_var = 0)
    attr(res, "pi_draws") <- matrix(pi_point, nrow = 1)
    return(res)
  }
  
  # Cache training data once (tensorize hist_data, then index per bootstrap)
  nn_data_train_all <- tryCatch(
    prepare_nn_data(hist_data, dept_model$questions,
                    include_fit = include_fit,
                    include_questions = include_questions),
    error = function(e) NULL)
  y_all <- torch::torch_tensor(hist_data$accepted, dtype = torch::torch_float())$unsqueeze(2)
  
  # Save trained model state for warm-start bootstrap
  trained_state <- if (dept_model$is_trained) lapply(dept_model$model$parameters, function(p) p$clone()$detach()) else NULL
  
  # Allocate temp model once outside the loop
  temp_model <- initialize_department_model(dept_model$questions,
                                            include_fit = include_fit,
                                            include_questions = include_questions)
  
  for (b in 1:n_bootstrap) {
    boot_indices <- sample.int(n_hist, n_hist, replace = TRUE)
    
    # Reset weights to trained state 
    if (!is.null(trained_state)) {
      tp <- temp_model$model$parameters
      with_no_grad({ for (i in seq_along(tp)) tp[[i]]$copy_(trained_state[[i]]) })
    }
    temp_model$is_trained <- FALSE
    
    # Train from cached tensors
    if (!is.null(nn_data_train_all)) {
      temp_model <- tryCatch(
        train_department_model_from_tensors(
          temp_model, nn_data_train_all$x_cont, nn_data_train_all$x_cat, y_all,
          boot_indices, n_epochs = 5, seed = seed + b),
        error = function(e) temp_model)
    } else {
      boot_data <- hist_data[boot_indices, ]
      temp_model$historical_data <- boot_data
      temp_model <- tryCatch(
        train_department_model(temp_model, boot_data, n_epochs = 5,
                               include_fit = include_fit,
                               include_questions = include_questions,
                               seed = seed + b),
        error = function(e) temp_model)
    }
    
    if (!temp_model$is_trained) {
      pi_draws_matrix[b, ] <- pmin(pmax(
        pi_prior + rnorm(n_applicants, 0, 0.05), 1e-5), 1 - 1e-5)
      next
    }
    
    temp_model$model$eval()
    with_no_grad({
      lo <- temp_model$model(nn_data_cached$x_cont, nn_data_cached$x_cat)
      pi_b <- as.numeric(torch_sigmoid(lo)$squeeze()$to(device = "cpu"))
    })
    pi_b <- pmin(pmax(pi_b, 1e-5), 1 - 1e-5)
    
    # Blend with prior
    pi_draws_matrix[b, ] <- mw * pi_b + (1 - mw) * pi_prior
  }
  
  # Aggregate bootstrap draws
  pi_mean <- colMeans(pi_draws_matrix, na.rm = TRUE)
  pi_var  <- colMeans(sweep(pi_draws_matrix, 2, pi_mean)^2, na.rm = TRUE) * nrow(pi_draws_matrix) / (nrow(pi_draws_matrix) - 1)
  
  res <- applicant_data %>%
    dplyr::mutate(pi_pred = pi_mean, pi_var = pi_var)
  attr(res, "pi_draws") <- pi_draws_matrix
  res
}

select_interviews_sure_screening_precomputed <- function(
    applicant_data, rank_tbl, k_j, h_j, tier_width = 0.1) {
  
  n <- nrow(applicant_data)
  if (n == 0L) return(integer())
  if (n <= k_j) return(applicant_data$cand_id)
  
  if (!"U_hat" %in% names(applicant_data))
    applicant_data <- applicant_data %>%
      dplyr::mutate(U_hat = U_det * pmin(pmax(pi_pred, 1e-5), 1 - 1e-5))
  
  # Join pre-computed ranks (no recomputation)
  applicant_data <- applicant_data %>%
    dplyr::select(-dplyr::any_of(c("rank_lower", "pair_score", "mean_rank"))) %>%
    dplyr::left_join(
      rank_tbl %>% dplyr::select(cand_id, rank_lower, pair_score, mean_rank),
      by = "cand_id")
  
  sure_idx <- which(applicant_data$rank_lower <= k_j)
  
  if (length(sure_idx) >= k_j) {
    selected_ids <- applicant_data %>%
      dplyr::slice(sure_idx) %>%
      dplyr::arrange(mean_rank, dplyr::desc(pair_score), dplyr::desc(U_hat)) %>%
      dplyr::slice_head(n = k_j) %>%
      dplyr::pull(cand_id)
  } else {
    sure_ids <- applicant_data$cand_id[sure_idx]
    remaining_slots <- k_j - length(sure_idx)
    remaining_pool <- applicant_data %>% dplyr::filter(!(cand_id %in% sure_ids))
    if (nrow(remaining_pool) == 0) {
      selected_ids <- sure_ids
    } else {
      sure_U_hat <- applicant_data %>%
        dplyr::filter(cand_id %in% sure_ids) %>% dplyr::pull(U_hat)
      if (length(sure_U_hat) > 0) {
        tier_threshold <- min(sure_U_hat, na.rm = TRUE) - tier_width
        tier_candidates <- remaining_pool %>% dplyr::filter(U_hat >= tier_threshold)
        if (nrow(tier_candidates) > 0) {
          additional_ids <- tier_candidates %>%
            dplyr::arrange(dplyr::desc(U_hat), cand_id) %>%
            dplyr::slice_head(n = remaining_slots) %>%
            dplyr::pull(cand_id)
        } else {
          additional_ids <- remaining_pool %>%
            dplyr::arrange(dplyr::desc(U_hat)) %>%
            dplyr::slice_head(n = remaining_slots) %>% dplyr::pull(cand_id)
        }
      } else {
        additional_ids <- remaining_pool %>%
          dplyr::arrange(dplyr::desc(U_hat)) %>%
          dplyr::slice_head(n = remaining_slots) %>% dplyr::pull(cand_id)
      }
      selected_ids <- c(sure_ids, additional_ids)
    }
  }
  attr(selected_ids, "sure_ids") <- applicant_data$cand_id[sure_idx]
  selected_ids
}

# =============================================================================
# simulate_market_year_with_learned_prior
# =============================================================================
simulate_market_year_with_learned_prior <- function(candidates, departments, questions, year,
                                                    dept_models_pairwise, learned_prior_models, participants_this_year,
                                                    yearly_hiring_schedule, participation_rate=0, alpha=0.05, n_bootstrap=20, tuple_size=NULL,
                                                    shortlist_enabled=TRUE, collect_ranking_panel=TRUE,
                                                    cand_tier_col="quality_tier", max_offer_rounds=5, seed=NULL,
                                                    verbose=TRUE, precomputed_fj=NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (!"nonparticipant_fit_proxy" %in% names(candidates) ||
      !"participation_score" %in% names(candidates)) {
    dept_weights <- tryCatch(yearly_hiring_schedule[, year], error = function(e) NULL)
    candidates <- add_participation_signals(
      candidates = candidates,
      departments = departments,
      questions = questions,
      dept_weights = dept_weights
    )
  }
  candidates$participates <- candidates$cand_id %in% participants_this_year
  tier_to_int <- function(x) as.integer(factor(as.character(x), levels=c("Tier 1","Tier 2","Tier 3","Tier 4")))
  n_departments <- nrow(departments)
  learning_data_pairwise <- vector("list", n_departments)
  diag_list <- vector("list", n_departments)
  interviewed_chunks <- vector("list", n_departments)
  rank_panel_chunks <- if (collect_ranking_panel) vector("list", n_departments) else NULL

  # Pre-compute tier info once (base R, avoids repeated factor/mutate per dept)
  if (!cand_tier_col %in% names(candidates)) {
    candidates[[cand_tier_col]] <- factor("Tier 4", levels=c("Tier 1","Tier 2","Tier 3","Tier 4"))
  } else {
    candidates[[cand_tier_col]] <- factor(as.character(candidates[[cand_tier_col]]),
                                          levels=c("Tier 1","Tier 2","Tier 3","Tier 4"))
  }
  cand_tier_int <- tier_to_int(candidates[[cand_tier_col]])
  cand_tier_char <- as.character(candidates[[cand_tier_col]])

  # Shortlist allow map (computed once)
  allow_map <- list("Tier 1"=c("Tier 1"),"Tier 2"=c("Tier 1","Tier 2"),
                    "Tier 3"=c("Tier 1","Tier 2","Tier 3"),"Tier 4"=c("Tier 1","Tier 2","Tier 3","Tier 4"))

  if (verbose) cat(sprintf("\n=== YEAR %d: Interview Selection Phase ===\n", year))
  for (j in 1:n_departments) {
    dept <- departments[j,]; h_j <- yearly_hiring_schedule[j,year]; k_j <- 5L*h_j
    dept_s_j <- dept$s_j; dept_id_j <- dept$dept_id
    dept_tier_int <- tier_to_int(dept$prestige_tier %||% "Tier 4")

    if (h_j==0L) {
      d_j <- candidates
      d_j$s_j <- dept_s_j; d_j$dept_id <- dept_id_j; d_j$year <- year
      d_j$strategy <- "pairwise"; d_j$pi_pred <- NA_real_; d_j$U_true <- NA_real_
      d_j$U_hat <- NA_real_; d_j$r_true <- NA_real_; d_j$r_hat <- NA_real_
      d_j$interviewed <- 0L; d_j$considered <- 0L; d_j$h_j <- 0L; d_j$k_j <- 0L
      diag_list[[j]] <- d_j
      next
    }

    apps_all <- candidates
    apps_all$s_j <- dept_s_j
    apps_all$dept_id <- dept_id_j
    apps_all$dept_tier <- dept_tier_int
    apps_all$cand_tier <- cand_tier_int

    # Use pre-computed f_j if available, otherwise compute
    if (!is.null(precomputed_fj) && j <= ncol(precomputed_fj)) {
      apps_all$f_j <- precomputed_fj[, j]
    } else {
      apps_all$f_j <- compute_f_j(apps_all, dept, questions)
    }

    if (shortlist_enabled) {
      pt <- as.character(dept$prestige_tier %||% "Tier 4")
      allowed <- allow_map[[pt]] %||% allow_map[["Tier 4"]]
      considered_mask <- cand_tier_char %in% allowed
    } else considered_mask <- rep(TRUE, nrow(apps_all))
    applicant_data <- apps_all[considered_mask,,drop=FALSE]
    if (nrow(applicant_data)==0) {
      d_j <- apps_all
      d_j$year <- year; d_j$strategy <- "pairwise"
      d_j$pi_pred <- NA_real_; d_j$U_true <- NA_real_; d_j$U_hat <- NA_real_
      d_j$r_true <- NA_real_; d_j$r_hat <- NA_real_
      d_j$interviewed <- 0L; d_j$considered <- 0L; d_j$h_j <- h_j; d_j$k_j <- k_j
      diag_list[[j]] <- d_j
      next
    }
    applicant_data$row_id <- seq_len(nrow(applicant_data))
    applicant_data_pw <- applicant_data
    applicant_data_pw$f_j_used <- compute_nonparticipant_fj(
      applicant_data = applicant_data,
      participation_rate = participation_rate
    )
    actual_f_j <- applicant_data_pw$f_j
    ad_tmp <- applicant_data_pw; ad_tmp$f_j <- applicant_data_pw$f_j_used
    for (q in questions$numerical) ad_tmp[[q]] <- dept[[q]]
    for (qn in names(questions$categorical)) ad_tmp[[qn]] <- as.character(dept[[qn]])
    applicant_data_pw <- predict_acceptance_probability_with_learned_prior(
      dept_models_pairwise[[j]], ad_tmp, learned_prior_model=learned_prior_models[[j]],
      n_bootstrap=n_bootstrap, seed=j*1000+year, participation_rate=participation_rate)
    pi_draws_pw <- attr(applicant_data_pw, "pi_draws")
    applicant_data_pw$f_j <- actual_f_j
    U_det <- department_utility(applicant_data_pw$s_j, applicant_data_pw$v_i_bar,
                          applicant_data_pw$f_j_used, applicant_data_pw$dept_tier, applicant_data_pw$cand_tier)
    applicant_data_pw$f_j_used <- applicant_data_pw$f_j_used
    applicant_data_pw$U_det <- U_det
    applicant_data_pw$U_hat <- U_det * pmin(pmax(applicant_data_pw$pi_pred, 1e-5), 1 - 1e-5)
    U_true <- department_utility(applicant_data_pw$s_j, applicant_data_pw$v_i_bar,
                           applicant_data_pw$f_j, applicant_data_pw$dept_tier, applicant_data_pw$cand_tier)
    applicant_data_pw$U_true <- U_true
    applicant_data_pw$r_true <- rank(-U_true, ties.method="average")
    applicant_data_pw$r_point <- rank(-applicant_data_pw$U_hat, ties.method="average")
    rpair <- compute_pairwise_lower_ranks(applicant_data_pw, n_bootstrap, tuple_size,
                                          alpha, pi_draws=pi_draws_pw)
    rpair_idx <- match(applicant_data_pw$cand_id, rpair$cand_id)
    applicant_data_pw$r_pair_lb <- rpair$rank_lower[rpair_idx]
    applicant_data_pw$pair_score <- rpair$pair_score[rpair_idx]
    applicant_data_pw$mean_rank <- rpair$mean_rank[rpair_idx]

    interviewed_ids <- select_interviews_sure_screening_precomputed(
      applicant_data_pw, rpair, k_j=k_j, h_j=h_j)
    applicant_data_pw$interviewed_flag <- as.integer(applicant_data_pw$cand_id %in% interviewed_ids)
    if (collect_ranking_panel) {
      rank_panel_chunks[[j]] <- applicant_data_pw %>% transmute(
        year, dept_id=dept_id_j, strategy="pairwise", cand_id, s_j, v_i_bar, f_j,
        participates, U_true, U_hat, r_true, r_point,
        r_pair_lb=if_else(is.finite(r_pair_lb), r_pair_lb, NA_real_),
        interviewed=interviewed_flag, k_j=k_j, h_j=h_j)
    }
    interviewed_mask <- applicant_data_pw$cand_id %in% interviewed_ids
    if (any(interviewed_mask)) {
      idpw <- applicant_data_pw[interviewed_mask, , drop=FALSE]
      idpw$interviewed <- 1L; idpw$year <- year; idpw$dept_id <- dept_id_j
      idpw$strategy <- "pairwise"; idpw$h_j <- h_j; idpw$k_j <- k_j
      interviewed_chunks[[j]] <- idpw
    }
    d_j <- apps_all
    d_j$considered <- as.integer(d_j$cand_id %in% applicant_data$cand_id)
    d_j$interviewed <- as.integer(d_j$cand_id %in% interviewed_ids)
    d_j$h_j <- h_j; d_j$k_j <- k_j
    pw_idx <- match(d_j$cand_id, applicant_data_pw$cand_id)
    d_j$pi_pred <- applicant_data_pw$pi_pred[pw_idx]
    d_j$U_true <- applicant_data_pw$U_true[pw_idx]
    d_j$U_hat <- applicant_data_pw$U_hat[pw_idx]
    d_j$r_true <- applicant_data_pw$r_true[pw_idx]
    d_j$year <- year; d_j$strategy <- "pairwise"
    diag_list[[j]] <- d_j[, c("year","dept_id","strategy","cand_id","s_j","v_i_bar","f_j",
                               "pi_pred","U_true","U_hat","r_true","interviewed","considered",
                               "participates","h_j","k_j"), drop=FALSE]
  }
  if (verbose) cat(sprintf("\n=== YEAR %d: Sequential Offer Resolution ===\n", year))
  all_interviewed <- bind_rows(interviewed_chunks)
  rank_panel <- if (collect_ranking_panel) bind_rows(rank_panel_chunks) else tibble()
  if (nrow(all_interviewed)>0) {
    all_results <- resolve_offers_sequential(interviewed_data=all_interviewed,
                                             departments=departments, questions=questions, all_candidates=candidates,
                                             max_rounds=max_offer_rounds, seed=year, temperature=0.05, noise_sd=0.01,
                                             shortlist_enabled=shortlist_enabled, year=year, verbose=verbose)
    if (verbose) print_hiring_allocation(all_results, year, participation_rate)
  } else all_results <- tibble()
  for (j in 1:nrow(departments)) {
    ld <- all_results %>% filter(dept_id==departments$dept_id[j], strategy=="pairwise", interviewed==1L)
    if (nrow(ld)>0)
      learning_data_pairwise[[j]] <- ld %>%
        dplyr::select(year,dept_id,cand_id,s_j,v_i_bar,f_j,offered,accepted,starts_with("q_")) %>%
        add_dept_info_to_learning_data(departments[j,])
  }
  diag_list <- diag_list[!vapply(diag_list, is.null, logical(1))]
  list(results=all_results, learning_data_pairwise=learning_data_pairwise,
       rank_panel=rank_panel, diagnostics=list(applicant_level=bind_rows(diag_list)))
}

# =============================================================================
# run_job_market_sim_with_learned_prior — Simulation phase with learned priors
#
# Sim-phase models are seeded with burn-in historical data so they inherit
# baseline observations. New observations (including fit features for the
# questionnaire scenario) are added incrementally each year.
# =============================================================================
run_job_market_sim_with_learned_prior <- function(departments, questions, n_candidates = 500,
                                                  burn_in_years = 10, sim_years = 10, participation_rate,
                                                  yearly_candidate_cohorts_burn_in = NULL,
                                                  yearly_candidate_cohorts_sim = NULL, participation_sets = NULL,
                                                  yearly_participation_sets_sim = NULL,
                                                  yearly_hiring_schedule_burn_in = NULL, yearly_hiring_schedule_sim = NULL,
                                                  learned_prior_models = NULL,
                                                  burn_in_historical = NULL,
                                                  seed = 123, alpha = 0.05, n_bootstrap = 20, tuple_size = NULL,
                                                  cand_tier_cutpoints = c(0.10, 0.25, 0.50),
                                                  max_offer_rounds = 5, print_diagnostics = FALSE,
                                                  return_dept_models = FALSE,
                                                  return_yearly_results = FALSE,
                                                  collect_ranking_panel = FALSE,
                                                  keep_diagnostics = FALSE,
                                                  verbose = TRUE,
                                                  precomputed_fj_list = NULL) {
  
  set.seed(seed); torch::torch_manual_seed(seed); n_departments <- nrow(departments)
  is_baseline <- (participation_rate == 0)
  if (is.null(yearly_hiring_schedule_sim))
    yearly_hiring_schedule_sim <- generate_yearly_hiring_schedule(n_departments, sim_years, departments, seed + 600)
  if (is.null(yearly_candidate_cohorts_sim)) {
    yearly_candidate_cohorts_sim <- vector("list", sim_years)
    for (year in 1:sim_years)
      yearly_candidate_cohorts_sim[[year]] <- generate_candidates(n_candidates, questions, seed = seed + burn_in_years + year)
  }
  cand_roster_all <- list(); rank_all <- list(); diag_all <- list()
  
  # Initialize sim-phase models (burn-in knowledge enters via learned_prior_models)
  mdl <- purrr::map(1:n_departments, function(j) {
    initialize_department_model(questions,
                                include_fit = TRUE,
                                include_questions = !is_baseline)
  })
  
  res_all <- list(); dept_tier_info <- departments %>% dplyr::select(dept_id, tier)
  
  for (year in 1:sim_years) {
    if (verbose) {
      cat("\n", strrep("=", 70), "\n")
      cat(sprintf("SIMULATING YEAR %d with participation rate %.0f%% [%s]\n",
                  year, participation_rate * 100, if (is_baseline) "BASELINE" else "QUESTIONNAIRE"))
      cat(strrep("=", 70), "\n")
    }
    candidates <- yearly_candidate_cohorts_sim[[year]]
    if (!"participation_score" %in% names(candidates) ||
        !"nonparticipant_fit_proxy" %in% names(candidates)) {
      candidates <- add_participation_signals(
        candidates = candidates,
        departments = departments,
        questions = questions,
        dept_weights = yearly_hiring_schedule_sim[, year]
      )
      yearly_candidate_cohorts_sim[[year]] <- candidates
    }
    rate_key <- as.character(participation_rate)
    participants <- integer(0)
    if (!is.null(yearly_participation_sets_sim) && length(yearly_participation_sets_sim) >= year) {
      year_sets <- yearly_participation_sets_sim[[year]]
      if (!is.null(year_sets) && rate_key %in% names(year_sets)) {
        participants <- year_sets[[rate_key]]
      }
    } else if (!is.null(participation_sets) && rate_key %in% names(participation_sets)) {
      participants <- participation_sets[[rate_key]]
    }
    candidates <- candidates %>% mutate(
      quality_tier = assign_tiers_from_quantiles(v_i_bar, cutpoints = cand_tier_cutpoints))
    cand_roster_all[[year]] <- candidates %>%
      mutate(participates = cand_id %in% participants) %>%
      transmute(year = !!year, cand_id, quality_tier, v_i_bar, participates)
    
    # Use pre-computed f_j if available for this year
    year_fj <- if (!is.null(precomputed_fj_list) && year <= length(precomputed_fj_list)) precomputed_fj_list[[year]] else NULL
    out <- simulate_market_year_with_learned_prior(candidates, departments, questions, year,
                                                   dept_models_pairwise = mdl, learned_prior_models = learned_prior_models,
                                                   participants_this_year = participants,
                                                   yearly_hiring_schedule = yearly_hiring_schedule_sim,
                                                   participation_rate = participation_rate, alpha = alpha, n_bootstrap = n_bootstrap,
                                                   tuple_size = tuple_size,
                                                   shortlist_enabled = TRUE, collect_ranking_panel = collect_ranking_panel, cand_tier_col = "quality_tier",
                                                   max_offer_rounds = max_offer_rounds, seed = year,
                                                   verbose = verbose, precomputed_fj = year_fj)
    
    if (keep_diagnostics) diag_all[[year]] <- out$diagnostics$applicant_level
    res_all[[year]] <- out$results
    if (collect_ranking_panel) rank_all[[year]] <- out$rank_panel
    
    # Cumulative diagnostics
    if (print_diagnostics && verbose && nrow(out$results) > 0) {
      cumulative_results <- bind_rows(res_all[1:year])
      if (nrow(cumulative_results) > 0) {
        cumulative_with_tiers <- cumulative_results %>%
          left_join(dept_tier_info, by = "dept_id")
        cumulative_quota <- tibble(dept_id = 1:n_departments) %>%
          left_join(dept_tier_info, by = "dept_id") %>%
          crossing(yr = 1:year) %>%
          mutate(h_j = yearly_hiring_schedule_sim[cbind(dept_id, yr)]) %>%
          group_by(tier) %>%
          summarise(total_quota = sum(h_j), .groups = "drop")
        overall_offers <- sum(cumulative_with_tiers$offered, na.rm = TRUE)
        overall_accepts <- sum(cumulative_with_tiers$accepted, na.rm = TRUE)
        overall_yield <- if (overall_offers > 0) overall_accepts / overall_offers else NA
        overall_mean_U <- mean(cumulative_with_tiers$U_true[cumulative_with_tiers$accepted == 1], na.rm = TRUE)
        overall_mean_f <- mean(cumulative_with_tiers$f_j[cumulative_with_tiers$accepted == 1], na.rm = TRUE)
        tier_metrics <- cumulative_with_tiers %>%
          group_by(tier) %>%
          summarise(
            n_offers = sum(offered, na.rm = TRUE),
            n_accepts = sum(accepted, na.rm = TRUE),
            yield = if_else(n_offers > 0, n_accepts / n_offers, NA_real_),
            mean_U_true = mean(U_true[accepted == 1], na.rm = TRUE),
            mean_f_j = mean(f_j[accepted == 1], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          left_join(cumulative_quota, by = "tier") %>%
          mutate(
            fill_rate = if_else(total_quota > 0, n_accepts / total_quota, NA_real_),
            tier = factor(tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
          ) %>%
          arrange(tier)
        
        cat("\n")
        cat(strrep("\u2500", 78), "\n")
        cat(sprintf("  CUMULATIVE RESULTS (Years 1-%d) | \u03c1 = %.0f%%\n", year, participation_rate * 100))
        cat(strrep("\u2500", 78), "\n")
        cat(sprintf("  Overall: Offers=%d | Accepts=%d | Yield=%.1f%% | Mean U=%.4f | Mean f=%.4f\n",
                    overall_offers, overall_accepts, overall_yield * 100,
                    overall_mean_U, overall_mean_f))
        cat(strrep("\u2500", 78), "\n")
        cat(sprintf("  %-8s %7s %7s %7s %7s %9s %9s %9s\n",
                    "Dept", "Quota", "Offers", "Accepts", "Yield", "FillRate", "Mean_U", "Mean_f"))
        cat(strrep("\u2500", 78), "\n")
        for (i in 1:nrow(tier_metrics)) {
          row <- tier_metrics[i, ]
          cat(sprintf("  %-8s %7d %7d %7d %6.1f%% %8.1f%% %9.4f %9.4f\n",
                      as.character(row$tier), row$total_quota, row$n_offers, row$n_accepts,
                      row$yield * 100, row$fill_rate * 100, row$mean_U_true, row$mean_f_j))
        }
        cat(strrep("\u2500", 78), "\n")
        
        tier_int_to_str_local <- function(x) {
          dplyr::case_when(x == 1 ~ "Tier 1", x == 2 ~ "Tier 2",
                           x == 3 ~ "Tier 3", x == 4 ~ "Tier 4", TRUE ~ NA_character_)
        }
        cumulative_roster <- bind_rows(cand_roster_all[1:year])
        n_cand_by_tier <- cumulative_roster %>% count(quality_tier, name = "n_total")
        hires <- cumulative_with_tiers %>%
          filter(accepted == 1) %>%
          mutate(cand_tier_str = tier_int_to_str_local(cand_tier),
                 dept_tier_str = tier_int_to_str_local(dept_tier),
                 V_ij = candidate_utility(v_i_bar, s_j, f_j))
        cand_tier_metrics <- hires %>%
          group_by(cand_tier_str) %>%
          summarise(n_hired = n(), mean_V = mean(V_ij, na.rm = TRUE),
                    mean_f_j = mean(f_j, na.rm = TRUE), mean_s_j = mean(s_j, na.rm = TRUE),
                    .groups = "drop") %>%
          rename(quality_tier = cand_tier_str) %>%
          left_join(n_cand_by_tier %>% mutate(quality_tier = as.character(quality_tier)), by = "quality_tier") %>%
          mutate(match_rate = if_else(n_total > 0, n_hired / n_total, NA_real_),
                 quality_tier = factor(quality_tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))) %>%
          arrange(quality_tier)
        cat(sprintf("\n  %-8s %7s %7s %9s %9s %9s %9s\n",
                    "Cand", "Pool", "Hired", "MatchRt", "Mean_V", "Mean_f", "Mean_s"))
        cat(strrep("\u2500", 78), "\n")
        for (i in 1:nrow(cand_tier_metrics)) {
          row <- cand_tier_metrics[i, ]
          cat(sprintf("  %-8s %7d %7d %8.1f%% %9.4f %9.4f %9.4f\n",
                      as.character(row$quality_tier), row$n_total, row$n_hired,
                      row$match_rate * 100, row$mean_V, row$mean_f_j, row$mean_s_j))
        }
        cat(strrep("\u2500", 78), "\n")
        alloc_matrix <- hires %>%
          count(cand_tier_str, dept_tier_str) %>%
          complete(cand_tier_str = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"),
                   dept_tier_str = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"),
                   fill = list(n = 0))
        cat(sprintf("\n  Allocation Matrix (Cand Tier -> Dept Tier):\n"))
        cat(sprintf("  %-8s %8s %8s %8s %8s\n", "", "Dept T1", "Dept T2", "Dept T3", "Dept T4"))
        for (ct in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
          row_data <- alloc_matrix %>% filter(cand_tier_str == ct) %>% arrange(dept_tier_str)
          vals <- row_data$n
          if (length(vals) < 4) vals <- c(vals, rep(0, 4 - length(vals)))
          cat(sprintf("  %-8s %8d %8d %8d %8d\n", ct, vals[1], vals[2], vals[3], vals[4]))
        }
        alloc_V <- hires %>%
          group_by(cand_tier_str, dept_tier_str) %>%
          summarise(mean_V = mean(V_ij, na.rm = TRUE), n = n(), .groups = "drop") %>%
          filter(n > 0)
        if (nrow(alloc_V) > 0) {
          cat(sprintf("\n  Mean V_ij by Allocation Cell:\n"))
          cat(sprintf("  %-8s %8s %8s %8s %8s\n", "", "Dept T1", "Dept T2", "Dept T3", "Dept T4"))
          for (ct in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
            vals <- character(4)
            for (di in 1:4) {
              dt <- paste0("Tier ", di)
              cell <- alloc_V %>% filter(cand_tier_str == ct, dept_tier_str == dt)
              vals[di] <- if (nrow(cell) > 0) sprintf("%.3f", cell$mean_V[1]) else "   -   "
            }
            cat(sprintf("  %-8s %8s %8s %8s %8s\n", ct, vals[1], vals[2], vals[3], vals[4]))
          }
        }
        cat(strrep("\u2500", 78), "\n\n")
      }
    }
    
    # Update models with new data
    for (j in 1:n_departments) {
      if (!is.null(out$learning_data_pairwise[[j]]) && nrow(out$learning_data_pairwise[[j]]) > 0) {
        mdl[[j]]$historical_data <- bind_rows(mdl[[j]]$historical_data, out$learning_data_pairwise[[j]])
        n_offered_j <- sum(mdl[[j]]$historical_data$offered == 1L, na.rm = TRUE)
        if (n_offered_j >= 5)
          mdl[[j]] <- tryCatch(
            train_department_model(mdl[[j]], mdl[[j]]$historical_data, n_epochs = 10,
                                   include_fit = TRUE, include_questions = !is_baseline,
                                   seed = j * 1000 + year),
            error = function(e) mdl[[j]])
      }
    }
  }
  
  sim_out <- list(
    results = bind_rows(res_all),
    rank_panel = if (collect_ranking_panel) bind_rows(rank_all) else tibble(),
    cand_roster = bind_rows(cand_roster_all),
    diagnostics = if (keep_diagnostics) bind_rows(diag_all) else tibble()
  )
  if (isTRUE(return_dept_models)) sim_out$dept_models <- mdl
  if (isTRUE(return_yearly_results)) sim_out$yearly_results <- res_all
  sim_out
}




reconstruct_learned_prior_models <- function(burn_in_historical, questions, seed = 123,
                                             min_offered_to_train = 10L) {
  set.seed(seed); torch::torch_manual_seed(seed)
  n_depts <- length(burn_in_historical)
  models <- vector("list", n_depts)
  for (j in seq_len(n_depts)) {
    mdl_j <- initialize_department_model(
      questions,
      include_fit = TRUE,
      include_questions = FALSE
    )
    hist_data <- burn_in_historical[[j]]
    if (!is.null(hist_data) && is.data.frame(hist_data)) {
      mdl_j$historical_data <- hist_data
      n_offered <- sum(hist_data$offered == 1L, na.rm = TRUE)
      if (n_offered >= min_offered_to_train) {
        mdl_j <- tryCatch(
          train_department_model(
            mdl_j, hist_data,
            n_epochs = 100,
            include_fit = TRUE,
            include_questions = FALSE,
            seed = j * 1000 + seed
          ),
          error = function(e) mdl_j
        )
      }
    }
    models[[j]] <- mdl_j
  }
  models
}


# =============================================================================
# extract_model_weights / reconstruct_from_weights
#
# Extract torch model weights as plain R objects for serialization, and
# reconstruct models from saved weights
# =============================================================================
extract_model_weights <- function(models) {
  lapply(models, function(mdl) {
    if (!mdl$is_trained) return(NULL)
    param_list <- mdl$model$parameters
    lapply(param_list, function(p) {
      as.array(p$cpu()$detach())
    })
  })
}

reconstruct_from_weights <- function(weight_list, historical_list, questions, seed = 123) {
  torch::torch_manual_seed(seed)
  n_depts <- length(weight_list)
  models <- vector("list", n_depts)
  for (j in seq_len(n_depts)) {
    mdl_j <- initialize_department_model(
      questions, include_fit = TRUE, include_questions = FALSE
    )
    if (!is.null(historical_list) && j <= length(historical_list)) {
      hist_data <- historical_list[[j]]
      if (!is.null(hist_data) && is.data.frame(hist_data))
        mdl_j$historical_data <- hist_data
    }
    w <- weight_list[[j]]
    if (!is.null(w)) {
      params <- mdl_j$model$parameters
      torch::with_no_grad({
        for (i in seq_along(params)) {
          target_shape <- params[[i]]$shape
          params[[i]]$copy_(torch::torch_tensor(w[[i]], dtype = torch::torch_float())$reshape(target_shape))
        }
      })
      mdl_j$is_trained <- TRUE
    }
    models[[j]] <- mdl_j
  }
  models
}


run_multi_replicate_simulation <- function(
    sampled_depts,
    questions,
    n_candidates       = 300,
    burn_in_years      = 20,
    sim_years          = 10,
    participation_rates = c(0, 1.00),
    base_seed          = 42,
    n_replicates       = 10,
    alpha              = 0.05,
    n_bootstrap          = 20,
    max_offer_rounds   = 3,
    cand_tier_cutpoints = c(0.10, 0.25, 0.50),
    tuple_size         = NULL,
    print_sim_diagnostics = FALSE,
    collect_rank_panel = FALSE,
    keep_diagnostics = FALSE,
    save_rep_results_dir = NULL,
    keep_raw_replicates = TRUE,
    replicate_shared_burn_in = FALSE,
    gc_every_replicate = TRUE,
    n_workers = NULL,
    show_replicate_progress = TRUE
) {
  
  # =========================================================================
  # STEP 1: Fix shared infrastructure (computed once, reused every replicate)
  # =========================================================================
  set.seed(base_seed); torch::torch_manual_seed(base_seed)
  
  departments <- prepare_departments(sampled_depts, questions, seed = base_seed)
  n_departments <- nrow(departments)
  
  # Fixed hiring schedules
  yearly_hiring_schedule_burn_in <- generate_yearly_hiring_schedule(
    n_departments, burn_in_years, departments, base_seed + 500)
  yearly_hiring_schedule_sim <- generate_yearly_hiring_schedule(
    n_departments, sim_years, departments, base_seed + 600)
  
  # Fallback only; strategic participation sets are generated per year/cohort.
  participation_sets <- NULL
  
  # =========================================================================
  # STEP 2: SHARED BURN-IN (run ONCE for all replicates)
  # =========================================================================
  cat("\n", strrep("=", 80), "\n")
  cat("SHARED BURN-IN: Running once for all replicates\n")
  cat(strrep("=", 80), "\n")
  
  burn_in_seed <- base_seed
  yearly_candidate_cohorts_burn_in <- vector("list", burn_in_years)
  for (year in 1:burn_in_years)
    yearly_candidate_cohorts_burn_in[[year]] <- generate_candidates(
      n_candidates, questions, seed = burn_in_seed + year)
  
  burn_in <- run_burn_in_phase(
    departments = departments, questions = questions,
    n_candidates = n_candidates, burn_in_years = burn_in_years,
    yearly_candidate_cohorts = yearly_candidate_cohorts_burn_in,
    yearly_hiring_schedule = yearly_hiring_schedule_burn_in,
    seed = burn_in_seed, alpha = alpha, n_bootstrap = n_bootstrap,
    tuple_size = tuple_size, cand_tier_cutpoints = cand_tier_cutpoints,
    max_offer_rounds = max_offer_rounds,
    return_yearly_objects = FALSE)
  
  learned_prior_models <- burn_in$trained_models
  burn_in_historical <- burn_in$burn_in_historical

  # Extract model weights for efficient reconstruction in parallel workers
  learned_prior_weights <- extract_model_weights(learned_prior_models)

  cat("\n", strrep("=", 80), "\n")
  cat("BURN-IN COMPLETE. Starting replicated sim-phase runs.\n")
  cat(strrep("=", 80), "\n")
  
  if (!is.null(save_rep_results_dir) && !dir.exists(save_rep_results_dir)) {
    dir.create(save_rep_results_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  append_replicate_table <- function(bucket, field, tbl, rep_id) {
    if (!is.null(tbl) && nrow(tbl) > 0) {
      bucket[[field]][[length(bucket[[field]]) + 1L]] <- tbl %>%
        dplyr::mutate(replicate = rep_id)
    }
    bucket
  }
  
  rate_keys <- as.character(participation_rates)
  sim_acc <- setNames(
    lapply(rate_keys, function(x) list(
      results = list(),
      rank_panel = list(),
      cand_roster = list(),
      diagnostics = list()
    )),
    rate_keys
  )
  raw_replicates <- if (keep_raw_replicates) vector("list", n_replicates) else NULL
  
  # =========================================================================
  # STEP 3: Run replicate tasks (serial or parallel)
  # =========================================================================
  if (is.null(n_workers)) {
    n_workers <- min(n_replicates, max(1L, parallel::detectCores(logical = FALSE) - 1L))
  } else {
    n_workers <- as.integer(n_workers)
    if (is.na(n_workers)) n_workers <- 1L
    n_workers <- max(1L, min(n_workers, n_replicates))
  }
  
  use_parallel <- (n_workers > 1L && n_replicates > 1L)
  if (use_parallel && (!requireNamespace("future", quietly = TRUE) ||
                       !requireNamespace("future.apply", quietly = TRUE))) {
    warning("Packages 'future' and 'future.apply' are required for parallel replicates. Falling back to serial.")
    use_parallel <- FALSE
    n_workers <- 1L
  }
  
  run_single_replicate <- function(rep, learned_prior_models_override = NULL) {
    rep_seed <- base_seed + rep * 10000
    rep_verbose <- !use_parallel
    if (rep_verbose) {
      cat("\n\n")
      cat(strrep("*", 80), "\n")
      cat(sprintf("***  REPLICATE %d / %d  (seed = %d)  ***\n", rep, n_replicates, rep_seed))
      cat(strrep("*", 80), "\n\n")
    }

    set.seed(rep_seed); torch::torch_manual_seed(rep_seed)
    try(torch::torch_set_num_threads(1L), silent = TRUE)
    try(torch::torch_set_num_interop_threads(1L), silent = TRUE)

    # Fresh candidate cohorts for sim phase only
    yearly_candidate_cohorts_sim <- vector("list", sim_years)
    for (year in 1:sim_years)
      yearly_candidate_cohorts_sim[[year]] <- generate_candidates(
        n_candidates, questions, seed = rep_seed + burn_in_years + year)

    # Strategic participation: score each cohort by pool alignment + quality
    yearly_participation_sets_sim <- vector("list", sim_years)
    yearly_fj_matrices <- vector("list", sim_years)
    positive_rates <- participation_rates[participation_rates > 0 & participation_rates < 1]
    for (year in 1:sim_years) {
      cohort <- add_participation_signals(
        candidates = yearly_candidate_cohorts_sim[[year]],
        departments = departments,
        questions = questions,
        dept_weights = yearly_hiring_schedule_sim[, year]
      )
      yearly_candidate_cohorts_sim[[year]] <- cohort
      yearly_participation_sets_sim[[year]] <- generate_nested_participation_assignments(
        candidates = cohort,
        participation_rates = positive_rates
      )
      # Pre-compute f_j for all candidate-department pairs (reused across rates)
      n_cands_year <- nrow(cohort)
      fj_mat <- matrix(NA_real_, nrow = n_cands_year, ncol = n_departments)
      for (jj in 1:n_departments) {
        fj_mat[, jj] <- compute_f_j(cohort, departments[jj, , drop = FALSE], questions)
      }
      yearly_fj_matrices[[year]] <- fj_mat
    }

    # Reconstruct models from saved weights
    learned_prior_models_rep <- learned_prior_models_override
    if (is.null(learned_prior_models_rep)) {
      learned_prior_models_rep <- reconstruct_from_weights(
        weight_list = learned_prior_weights,
        historical_list = burn_in_historical,
        questions = questions,
        seed = burn_in_seed
      )
    }

    # SIMULATION RUNS (one per participation rate)
    sim_results_rep <- list()
    for (rate in participation_rates) {
      rate_chr <- as.character(rate)
      if (rep_verbose) {
        cat(sprintf("\n### REPLICATE %d | participation_rate = %.0f%% ###\n",
                    rep, rate * 100))
      }

      sr <- run_job_market_sim_with_learned_prior(
        departments = departments, questions = questions,
        n_candidates = n_candidates,
        burn_in_years = burn_in_years, sim_years = sim_years,
        participation_rate = rate,
        yearly_candidate_cohorts_sim = yearly_candidate_cohorts_sim,
        participation_sets = participation_sets,
        yearly_participation_sets_sim = yearly_participation_sets_sim,
        yearly_hiring_schedule_sim = yearly_hiring_schedule_sim,
        learned_prior_models = learned_prior_models_rep,
        seed = rep_seed, alpha = alpha, n_bootstrap = n_bootstrap,
        tuple_size = tuple_size, cand_tier_cutpoints = cand_tier_cutpoints,
        max_offer_rounds = max_offer_rounds,
        print_diagnostics = print_sim_diagnostics,
        return_dept_models = FALSE,
        return_yearly_results = FALSE,
        collect_ranking_panel = collect_rank_panel,
        keep_diagnostics = keep_diagnostics,
        verbose = rep_verbose,
        precomputed_fj_list = yearly_fj_matrices)

      sim_results_rep[[rate_chr]] <- sr
    }

    if (!is.null(save_rep_results_dir)) {
      rep_file <- file.path(save_rep_results_dir, sprintf("replicate_%03d.rds", rep))
      saveRDS(
        list(replicate = rep, seed = rep_seed, sim_results = sim_results_rep),
        rep_file,
        compress = "gzip"
      )
    }

    rm(yearly_candidate_cohorts_sim, yearly_participation_sets_sim, learned_prior_models_rep, yearly_fj_matrices)
    if (isTRUE(gc_every_replicate)) invisible(gc(verbose = FALSE))
    list(replicate = rep, seed = rep_seed, sim_results = sim_results_rep)
  }
  
  if (use_parallel) {
    cat(sprintf("Parallel replicate mode: %d worker(s) for %d replicate(s)\n", n_workers, n_replicates))
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    # Must use multisession (not multicore) because torch is not fork-safe:
    # forked processes share parent torch memory, causing segfaults
    future::plan(future::multisession, workers = n_workers)
    if (isTRUE(show_replicate_progress) && requireNamespace("progressr", quietly = TRUE)) {
      progressr::handlers(global = TRUE)
      progressr::handlers("txtprogressbar")
      rep_outputs <- progressr::with_progress({
        p <- progressr::progressor(steps = n_replicates)
        future.apply::future_lapply(
          seq_len(n_replicates),
          function(rep) {
            out <- run_single_replicate(rep, learned_prior_models_override = NULL)
            p(sprintf("Replicate %d/%d", rep, n_replicates))
            out
          },
          future.seed = TRUE,
          future.scheduling = 1,
          future.globals = TRUE,
          future.packages = c("torch", "dplyr", "tidyr", "purrr", "tibble")
        )
      })
    } else {
      if (isTRUE(show_replicate_progress)) {
        cat("progressr not installed; running without progress bar in parallel mode.\n")
      }
      rep_outputs <- future.apply::future_lapply(
        seq_len(n_replicates),
        function(rep) run_single_replicate(rep, learned_prior_models_override = NULL),
        future.seed = TRUE,
        future.scheduling = 1,
        future.globals = TRUE,
        future.packages = c("torch", "dplyr", "tidyr", "purrr", "tibble")
      )
    }
  } else {
    cat(sprintf("Serial replicate mode: %d replicate(s)\n", n_replicates))
    rep_outputs <- vector("list", n_replicates)
    pb <- NULL
    if (isTRUE(show_replicate_progress)) {
      pb <- utils::txtProgressBar(min = 0, max = n_replicates, style = 3)
      on.exit(try(close(pb), silent = TRUE), add = TRUE)
    }
    for (rep in seq_len(n_replicates)) {
      rep_outputs[[rep]] <- run_single_replicate(
        rep,
        learned_prior_models_override = learned_prior_models
      )
      if (!is.null(pb)) utils::setTxtProgressBar(pb, rep)
    }
    if (!is.null(pb)) cat("\n")
  }
  
  for (rep_out in rep_outputs) {
    rep <- rep_out$replicate
    sim_results_rep <- rep_out$sim_results
    for (rate_chr in names(sim_results_rep)) {
      sr <- sim_results_rep[[rate_chr]]
      bucket <- sim_acc[[rate_chr]]
      bucket <- append_replicate_table(bucket, "results", sr$results, rep)
      bucket <- append_replicate_table(bucket, "rank_panel", sr$rank_panel, rep)
      bucket <- append_replicate_table(bucket, "cand_roster", sr$cand_roster, rep)
      bucket <- append_replicate_table(bucket, "diagnostics", sr$diagnostics, rep)
      sim_acc[[rate_chr]] <- bucket
    }
    if (keep_raw_replicates) {
      raw_replicates[[rep]] <- list(
        replicate = rep,
        seed = rep_out$seed,
        sim_results = sim_results_rep
      )
    }
  }
  
  # =========================================================================
  # STEP 4: Assemble combined results with replicate ID
  # =========================================================================
  
  combined <- list(
    departments = departments,
    questions   = questions,
    participation_sets = participation_sets,
    config = list(
      n_candidates = n_candidates, burn_in_years = burn_in_years,
      sim_years = sim_years, participation_rates = participation_rates,
      base_seed = base_seed, n_replicates = n_replicates,
      alpha = alpha, n_bootstrap = n_bootstrap,
      print_sim_diagnostics = print_sim_diagnostics,
      collect_rank_panel = collect_rank_panel,
      keep_diagnostics = keep_diagnostics,
      save_rep_results_dir = save_rep_results_dir,
      keep_raw_replicates = keep_raw_replicates,
      replicate_shared_burn_in = replicate_shared_burn_in,
      n_workers = n_workers,
      parallel_replicates = use_parallel,
      show_replicate_progress = isTRUE(show_replicate_progress)
    )
  )
  
  # Combine sim_results across replicates for each participation rate
  combined$sim_results <- setNames(vector("list", length(rate_keys)), rate_keys)
  for (rate_chr in rate_keys) {
    bucket <- sim_acc[[rate_chr]]
    combined$sim_results[[rate_chr]] <- list(
      results     = bind_rows(bucket$results),
      rank_panel  = bind_rows(bucket$rank_panel),
      cand_roster = bind_rows(bucket$cand_roster),
      diagnostics = bind_rows(bucket$diagnostics)
    )
  }
  
  # Combine burn-in results
  burn_in_results <- burn_in$burn_in_results
  burn_in_roster <- burn_in$cand_roster
  if (isTRUE(replicate_shared_burn_in)) {
    burn_in_results <- bind_rows(lapply(seq_len(n_replicates), function(rep) {
      burn_in$burn_in_results %>% dplyr::mutate(replicate = rep)
    }))
    burn_in_roster <- bind_rows(lapply(seq_len(n_replicates), function(rep) {
      burn_in$cand_roster %>% dplyr::mutate(replicate = rep)
    }))
  } else {
    if (!is.null(burn_in_results) && nrow(burn_in_results) > 0)
      burn_in_results <- burn_in_results %>% dplyr::mutate(replicate = 1L)
    if (!is.null(burn_in_roster) && nrow(burn_in_roster) > 0)
      burn_in_roster <- burn_in_roster %>% dplyr::mutate(replicate = 1L)
  }
  combined$burn_in <- list(
    burn_in_results = burn_in_results,
    cand_roster     = burn_in_roster
  )
  
  if (keep_raw_replicates) {
    combined$raw_replicates <- raw_replicates
  }
  if (!is.null(save_rep_results_dir)) {
    combined$replicate_checkpoint_dir <- normalizePath(save_rep_results_dir, mustWork = FALSE)
  }
  
  combined
}



# =============================================================================
#                                 FIGURES
# =============================================================================

# =============================================================================
# Publication theme and color palettes
# =============================================================================
theme_jasa <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.x       = element_text(margin = margin(t = 8)),
      axis.title.y       = element_text(margin = margin(r = 8)),
      strip.text         = element_text(face = "bold", size = rel(1)),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      plot.title          = element_text(face = "bold", size = rel(1.2)),
      plot.subtitle       = element_text(color = "grey25", size = rel(0.9))
    )
}

tier_int_to_str <- function(x) {
  dplyr::case_when(
    x == 1 ~ "Tier 1", x == 2 ~ "Tier 2",
    x == 3 ~ "Tier 3", x == 4 ~ "Tier 4",
    TRUE ~ NA_character_
  )
}

# =============================================================================
# COLOR PALETTES
# =============================================================================
# Tier palette: Acadia palette from nationalparkcolors (via paletteer)
tier_colors <- setNames(
  as.character(paletteer_d("nationalparkcolors::Acadia", n = 4)),
  c("Tier 1", "Tier 2", "Tier 3", "Tier 4")
)

# Participation palette: cavaliers_retro from nbapalettes (via paletteer)
participation_colors <- setNames(
  as.character(paletteer_d("nbapalettes::cavaliers_retro", n = 2)),
  c("Participating", "Non-participating")
)

# =============================================================================
# INTERNAL HELPERS
# =============================================================================

# Regenerate yearly_hiring_schedule_sim from config (deterministic)
.get_hiring_schedule <- function(all_sim_results) {
  cfg <- all_sim_results$config
  departments <- all_sim_results$departments
  generate_yearly_hiring_schedule(
    nrow(departments), cfg$sim_years, departments,
    seed = cfg$base_seed + 600)
}

# Extract results for a given rate string, optionally filter years
.get_results <- function(all_sim_results, rate_chr, year_filter = NULL,
                         include_scramble = FALSE) {
  sr <- all_sim_results$sim_results[[rate_chr]]
  if (is.null(sr) || is.null(sr$results) || nrow(sr$results) == 0)
    return(tibble())
  df <- sr$results %>%
    dplyr::filter(strategy == "pairwise")
  if (!is.null(year_filter))
    df <- df %>% dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  df
}

.get_roster <- function(all_sim_results, rate_chr, year_filter = NULL) {
  sr <- all_sim_results$sim_results[[rate_chr]]
  if (is.null(sr) || is.null(sr$cand_roster) || nrow(sr$cand_roster) == 0)
    return(tibble())
  df <- sr$cand_roster
  if (!is.null(year_filter))
    df <- df %>% dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  df
}
# =============================================================================
# 1. INTERVIEW HEATMAP (Baseline vs Questionnaire, averaged over replicates)
# =============================================================================
fig_interview_heatmap <- function(all_sim_results, year_filter = c(1, 10),
                                            include_scramble = FALSE) {
  
  departments <- all_sim_results$departments %>%
    dplyr::select(dept_id, prestige_tier)
  yearly_hiring_schedule <- .get_hiring_schedule(all_sim_results)
  
  get_interviews <- function(rate_chr) {
    df <- .get_results(all_sim_results, rate_chr, year_filter)
    if (nrow(df) == 0) return(tibble())
    base <- df %>% dplyr::filter(interviewed == 1)
    if (include_scramble) {
      scramble <- df %>% dplyr::filter(interviewed == 0, accepted == 1)
      if (nrow(scramble) > 0) {
        scramble$interviewed <- 1L
        base <- bind_rows(base, scramble)
      }
    }
    base
  }
  
  baseline_results <- get_interviews("0")
  full_results     <- get_interviews("1")
  
  # Candidate totals (from first replicate of baseline roster — same across reps)
  baseline_roster <- .get_roster(all_sim_results, "0", year_filter) %>%
    dplyr::filter(replicate == 1)
  cand_totals <- baseline_roster %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  # Interview budgets
  years_in_filter <- year_filter[1]:year_filter[2]
  interview_budget_totals <- tibble(dept_id = 1:nrow(departments)) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(h_j = yearly_hiring_schedule[cbind(dept_id, year)],
           k_j = 5L * h_j) %>%
    group_by(prestige_tier) %>%
    summarise(budget = sum(k_j), .groups = "drop") %>%
    mutate(prestige_tier = factor(prestige_tier,
                                  levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  # Combine and tag
  combined_interviews <- bind_rows(
    baseline_results %>%
      left_join(departments, by = "dept_id") %>%
      mutate(quality_tier = tier_int_to_str(cand_tier), scenario = "Baseline"),
    full_results %>%
      left_join(departments, by = "dept_id") %>%
      mutate(quality_tier = tier_int_to_str(cand_tier), scenario = "Questionnaire")
  )
  
  if (nrow(combined_interviews) == 0) {
    message("No interview data"); return(NULL)
  }
  
  # Per-replicate counts, then average
  per_rep <- combined_interviews %>%
    group_by(replicate, scenario, prestige_tier, quality_tier) %>%
    summarise(n_interviews = n(),
              mean_utility = mean(U_true, na.rm = TRUE),
              .groups = "drop")
  
  heatmap_data <- per_rep %>%
    group_by(scenario, prestige_tier, quality_tier) %>%
    summarise(
      mean_n = mean(n_interviews, na.rm = TRUE),
      mean_utility = mean(mean_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prestige_tier = factor(prestige_tier,
                             levels = c("Tier 4","Tier 3","Tier 2","Tier 1")),
      quality_tier = factor(quality_tier,
                            levels = c("Tier 1","Tier 2","Tier 3","Tier 4")),
      scenario = factor(scenario, levels = c("Baseline","Questionnaire"))
    ) %>%
    complete(scenario, prestige_tier, quality_tier,
             fill = list(mean_n = 0, mean_utility = NA))
  
  # Axis labels
  x_labels <- cand_totals %>%
    mutate(label = paste0(quality_tier, "\n(n=", n_cand, ")")) %>%
    arrange(quality_tier) %>% dplyr::select(quality_tier, label) %>% deframe()
  y_labels <- interview_budget_totals %>%
    mutate(prestige_tier_plot = factor(prestige_tier,
                                       levels = c("Tier 4","Tier 3","Tier 2","Tier 1")),
           label = paste0(prestige_tier, "\n(k=", budget, ")")) %>%
    arrange(desc(prestige_tier_plot)) %>%
    dplyr::select(prestige_tier_plot, label) %>% deframe()
  
  p <- ggplot(heatmap_data, aes(x = quality_tier, y = prestige_tier, fill = mean_utility)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = ifelse(mean_n > 0, sprintf("%.1f", mean_n), "")),
              fontface = "bold", size = 5, vjust = 0.3, color = "white") +
    geom_text(aes(label = ifelse(is.finite(mean_utility),
                                 sprintf("bar(U)[ji]==%.3f", mean_utility), "")),
              size = 3.5, vjust = 1.8, color = "white", parse = TRUE) +
    facet_wrap(~ scenario, ncol = 2) +
    scale_fill_viridis_c(name = expression(atop(bar(U)[ji], "Mean Utility")),
                         limits = c(0, 1), na.value = "grey70", option = "viridis",
                         breaks = seq(0, 1, 0.25)) +
    scale_x_discrete(labels = x_labels) +
    scale_y_discrete(labels = y_labels) +
    labs(x = "Candidate Quality Tier", y = "Department Prestige Tier") +
    theme_jasa(base_size = 14) +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "gray95", color = NA),
          legend.position = "right",
          legend.title = element_text(size = 12, face = "bold"),
          panel.spacing = unit(1.5, "lines"))
  
  list(plot = p, data = heatmap_data, candidate_totals = cand_totals,
       interview_budgets = interview_budget_totals)
}


# =============================================================================
# 2. HIRING HEATMAP (Baseline vs Questionnaire, averaged over replicates)
# =============================================================================
fig_hiring_heatmap <- function(all_sim_results, year_filter = c(1, 10),
                                         include_scramble = FALSE, hire_rounds = NULL) {
  
  departments <- all_sim_results$departments %>%
    dplyr::select(dept_id, prestige_tier)
  yearly_hiring_schedule <- .get_hiring_schedule(all_sim_results)
  
  get_hires <- function(rate_chr) {
    df <- .get_results(all_sim_results, rate_chr, year_filter)

    if (nrow(df) == 0) return(tibble())
    base <- df %>% dplyr::filter(accepted == 1)
    if (!is.null(hire_rounds))
      base <- base %>% dplyr::filter(is.na(offer_round) | offer_round <= hire_rounds)
    if (!include_scramble)
      base <- base %>% dplyr::filter(interviewed == 1 | is.na(interviewed))
    base
  }
  
  baseline_results <- get_hires("0")
  full_results     <- get_hires("1")
  
  baseline_roster <- .get_roster(all_sim_results, "0", year_filter) %>%
    dplyr::filter(replicate == 1)
  cand_totals <- baseline_roster %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  years_in_filter <- year_filter[1]:year_filter[2]
  quota_totals <- tibble(dept_id = 1:nrow(departments)) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(h_j = yearly_hiring_schedule[cbind(dept_id, year)]) %>%
    group_by(prestige_tier) %>%
    summarise(quota = sum(h_j), .groups = "drop") %>%
    mutate(prestige_tier = factor(prestige_tier,
                                  levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  combined_hires <- bind_rows(
    baseline_results %>%
      mutate(quality_tier = tier_int_to_str(cand_tier),
             prestige_tier = tier_int_to_str(dept_tier),
             utility_for_plot = coalesce(U_true, if ("cand_util" %in% names(.)) cand_util else NA_real_),
             scenario = "Baseline"),
    full_results %>%
      mutate(quality_tier = tier_int_to_str(cand_tier),
             prestige_tier = tier_int_to_str(dept_tier),
             utility_for_plot = coalesce(U_true, if ("cand_util" %in% names(.)) cand_util else NA_real_),
             scenario = "Questionnaire")
  ) %>% dplyr::filter(!is.na(quality_tier), !is.na(prestige_tier))
  
  if (nrow(combined_hires) == 0) {
    message("No hiring data"); return(NULL)
  }
  
  per_rep <- combined_hires %>%
    group_by(replicate, scenario, prestige_tier, quality_tier) %>%
    summarise(n_hires = n(),
              mean_utility = mean(utility_for_plot, na.rm = TRUE),
              .groups = "drop")
  
  heatmap_data <- per_rep %>%
    group_by(scenario, prestige_tier, quality_tier) %>%
    summarise(
      mean_n = mean(n_hires, na.rm = TRUE),
      mean_utility = mean(mean_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prestige_tier = factor(prestige_tier,
                             levels = c("Tier 4","Tier 3","Tier 2","Tier 1")),
      quality_tier = factor(quality_tier,
                            levels = c("Tier 1","Tier 2","Tier 3","Tier 4")),
      scenario = factor(scenario, levels = c("Baseline","Questionnaire"))
    ) %>%
    complete(scenario, prestige_tier, quality_tier,
             fill = list(mean_n = 0, mean_utility = NA))
  
  x_labels <- cand_totals %>%
    mutate(label = paste0(quality_tier, "\n(n=", n_cand, ")")) %>%
    arrange(quality_tier) %>% dplyr::select(quality_tier, label) %>% deframe()
  y_labels <- quota_totals %>%
    mutate(prestige_tier_plot = factor(prestige_tier,
                                       levels = c("Tier 4","Tier 3","Tier 2","Tier 1")),
           label = paste0(prestige_tier, "\n(h=", quota, ")")) %>%
    arrange(desc(prestige_tier_plot)) %>%
    dplyr::select(prestige_tier_plot, label) %>% deframe()
  
  p <- ggplot(heatmap_data, aes(x = quality_tier, y = prestige_tier, fill = mean_utility)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = ifelse(mean_n > 0, sprintf("%.1f", mean_n), "")),
              fontface = "bold", size = 5, vjust = 0.3, color = "white") +
    geom_text(aes(label = ifelse(is.finite(mean_utility),
                                 sprintf("bar(U)[ji]==%.3f", mean_utility), "")),
              size = 3.5, vjust = 1.8, color = "white", parse = TRUE) +
    facet_wrap(~ scenario, ncol = 2) +
    scale_fill_viridis_c(name = expression(atop(bar(U)[ji], "Mean Utility")),
                         limits = c(0, 1), na.value = "grey70", option = "viridis",
                         breaks = seq(0, 1, 0.25)) +
    scale_x_discrete(labels = x_labels) +
    scale_y_discrete(labels = y_labels) +
    labs(x = "Candidate Quality Tier", y = "Department Prestige Tier") +
    theme_jasa(base_size = 14) +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "gray95", color = NA),
          legend.position = "right",
          legend.title = element_text(size = 12, face = "bold"),
          panel.spacing = unit(1.5, "lines"))
  
  list(plot = p, data = heatmap_data, candidate_totals = cand_totals,
       hiring_quotas = quota_totals)
}


# =============================================================================
# 3. DEPARTMENT WELFARE BY TIER
# =============================================================================
fig_department_welfare <- function(all_sim_results,
                                                           year_filter = c(1, 10),
                                                           include_scramble = FALSE,
                                                           hire_rounds = NULL) {

  departments <- all_sim_results$departments %>%
    dplyr::select(dept_id, prestige_tier, s_j)
  yearly_hiring_schedule <- .get_hiring_schedule(all_sim_results)
  years_in_filter <- year_filter[1]:year_filter[2]
  n_departments <- nrow(departments)

  # Quota by tier (same across replicates)
  quota_by_tier <- tibble(dept_id = 1:n_departments) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(h_j = yearly_hiring_schedule[cbind(dept_id, year)]) %>%
    group_by(prestige_tier) %>%
    summarise(total_quota = sum(h_j), n_depts = n_distinct(dept_id), .groups = "drop")

  # Extract welfare per (rate, replicate, tier)
  rate_chrs <- names(all_sim_results$sim_results)

  welfare_by_rep <- purrr::map_dfr(rate_chrs, function(rate_chr) {
    rate <- as.numeric(rate_chr)
    df <- .get_results(all_sim_results, rate_chr, year_filter)
    if (nrow(df) == 0) return(tibble())
    hires <- df %>% dplyr::filter(accepted == 1)
    if (!include_scramble)
      hires <- hires %>% dplyr::filter(interviewed == 1 | is.na(interviewed))
    if (!is.null(hire_rounds))
      hires <- hires %>% dplyr::filter(is.na(offer_round) | offer_round <= hire_rounds)
    if (nrow(hires) == 0) return(tibble())

    hires %>%
      mutate(prestige_tier = tier_int_to_str(dept_tier),
             U_effective = coalesce(U_true, if ("cand_util" %in% names(.)) cand_util else NA_real_)) %>%
      group_by(replicate, prestige_tier) %>%
      summarise(
        n_hires = n(),
        total_utility = sum(U_effective, na.rm = TRUE),
        mean_utility_per_hire = mean(U_effective, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(quota_by_tier, by = "prestige_tier") %>%
      mutate(
        participation_rate = rate,
        mean_utility_per_slot = ifelse(total_quota > 0, total_utility / total_quota, NA_real_),
        utility_per_dept = ifelse(n_depts > 0, total_utility / n_depts, NA_real_),
        fill_rate = ifelse(total_quota > 0, n_hires / total_quota, NA_real_)
      )
  })

  if (nrow(welfare_by_rep) == 0) {
    message("No welfare data"); return(NULL)
  }

  # Average across replicates
  welfare_by_tier <- welfare_by_rep %>%
    group_by(participation_rate, prestige_tier) %>%
    summarise(
      utility_per_dept = mean(utility_per_dept, na.rm = TRUE),
      se_utility_per_dept = sd(utility_per_dept, na.rm = TRUE) / sqrt(sum(!is.na(utility_per_dept))),
      mean_utility_per_slot = mean(mean_utility_per_slot, na.rm = TRUE),
      se_utility_per_slot = sd(mean_utility_per_slot, na.rm = TRUE) / sqrt(sum(!is.na(mean_utility_per_slot))),
      mean_utility_per_hire = mean(mean_utility_per_hire, na.rm = TRUE),
      fill_rate = mean(fill_rate, na.rm = TRUE),
      n_hires = mean(n_hires, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(quota_by_tier %>% dplyr::select(prestige_tier, total_quota, n_depts),
              by = "prestige_tier") %>%
    mutate(prestige_tier = factor(prestige_tier,
                                  levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))

  # Plot: Welfare per hiring slot
  ribbon_alpha <- 0.15

  plot_theme <- theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1.5, "lines"),
          legend.key.width = unit(2.5, "lines"),
          legend.spacing.x = unit(0.5, "cm"),
          plot.margin = margin(10, 10, 10, 10))

  p <- ggplot(welfare_by_tier, aes(x = participation_rate * 100,
                                   y = mean_utility_per_slot,
                                   color = prestige_tier,
                                   linetype = prestige_tier,
                                   shape = prestige_tier,
                                   group = prestige_tier)) +
    geom_ribbon(aes(ymin = mean_utility_per_slot - 1.96 * se_utility_per_slot,
                    ymax = mean_utility_per_slot + 1.96 * se_utility_per_slot,
                    fill = prestige_tier),
                alpha = ribbon_alpha, linetype = 0, show.legend = FALSE) +
    geom_line(linewidth = 1.8) +
    geom_point(aes(size = prestige_tier)) +
    scale_color_manual(values = tier_colors) +
    scale_fill_manual(values = tier_colors) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_size_manual(values = c(4, 4, 4, 5.5)) +
    scale_x_continuous(breaks = c(0, 5, 20, 50, 90, 100),
                       labels = c("0", "5", "20", "50", "90", "100")) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    labs(x = "Market Participation Rate (%)", y = "Department Welfare per Position",
         color = "Department Tier", linetype = "Department Tier",
         shape = "Department Tier", size = "Department Tier") +
    plot_theme

  list(plot = p, aggregate_data = welfare_by_tier)
}


# =============================================================================
# 4. CANDIDATE UTILITY BY TIER (conditional on matching)
# =============================================================================
fig_candidate_utility <- function(all_sim_results,
                                                       year_filter = c(1, 10),
                                                       include_scramble = FALSE,
                                                       hire_rounds = NULL) {

  rate_chrs <- names(all_sim_results$sim_results)

  # Build welfare data: per (rate, replicate, quality_tier)
  welfare_by_rep <- purrr::map_dfr(rate_chrs, function(rate_chr) {
    rate <- as.numeric(rate_chr)
    roster <- .get_roster(all_sim_results, rate_chr, year_filter)
    df <- .get_results(all_sim_results, rate_chr, year_filter)
    if (nrow(df) == 0 || nrow(roster) == 0) return(tibble())

    matches <- df %>%
      dplyr::filter(accepted == 1)
    if (!include_scramble)
      matches <- matches %>% dplyr::filter(interviewed == 1 | is.na(interviewed))
    if (!is.null(hire_rounds))
      matches <- matches %>% dplyr::filter(is.na(offer_round) | offer_round <= hire_rounds)
    matches <- matches %>%
      mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))

    # Join by replicate, year, cand_id
    roster %>%
      left_join(matches %>%
                  group_by(replicate, year, cand_id) %>%
                  slice(1) %>%
                  summarise(welfare = first(V_ij),
                            match_f_j = first(f_j),
                            match_s_j = first(s_j),
                            .groups = "drop"),
                by = c("replicate", "year", "cand_id")) %>%
      mutate(welfare = coalesce(welfare, 0), matched = welfare > 0) %>%
      group_by(replicate, quality_tier) %>%
      summarise(
        n_cand = n(),
        n_matches = sum(matched),
        matching_rate = mean(matched),
        mean_welfare = mean(welfare, na.rm = TRUE),
        mean_welfare_if_matched = mean(welfare[matched], na.rm = TRUE),
        mean_f_j_if_matched = mean(match_f_j[matched], na.rm = TRUE),
        mean_s_j_if_matched = mean(match_s_j[matched], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(participation_rate = rate)
  })

  if (nrow(welfare_by_rep) == 0) {
    message("No candidate welfare data"); return(NULL)
  }

  # Average across replicates (unconditional utility: includes unmatched as zero)
  unconditional_welfare <- welfare_by_rep %>%
    group_by(participation_rate, quality_tier) %>%
    summarise(
      mean_welfare = mean(mean_welfare, na.rm = TRUE),
      se_mean_welfare = sd(mean_welfare, na.rm = TRUE) / sqrt(sum(!is.na(mean_welfare))),
      .groups = "drop"
    ) %>%
    mutate(quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))

  # Average across replicates (conditional: matched candidates only)
  conditional_welfare <- welfare_by_rep %>%
    group_by(participation_rate, quality_tier) %>%
    summarise(
      n_matches = mean(n_matches, na.rm = TRUE),
      mean_welfare_if_matched = mean(mean_welfare_if_matched, na.rm = TRUE),
      se_welfare = sd(mean_welfare_if_matched, na.rm = TRUE) / sqrt(sum(!is.na(mean_welfare_if_matched))),
      mean_f_j_if_matched = mean(mean_f_j_if_matched, na.rm = TRUE),
      mean_s_j_if_matched = mean(mean_s_j_if_matched, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))

  # Plot: Conditional welfare by tier
  ribbon_alpha <- 0.15

  plot_theme <- theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1.5, "lines"),
          legend.key.width = unit(2.5, "lines"),
          legend.spacing.x = unit(0.5, "cm"),
          plot.margin = margin(10, 10, 10, 10))

  p <- ggplot(conditional_welfare,
              aes(x = participation_rate * 100,
                  y = mean_welfare_if_matched,
                  color = quality_tier,
                  linetype = quality_tier,
                  shape = quality_tier,
                  size = quality_tier,
                  group = quality_tier)) +
    geom_ribbon(aes(ymin = mean_welfare_if_matched - 1.96 * se_welfare,
                    ymax = mean_welfare_if_matched + 1.96 * se_welfare,
                    fill = quality_tier),
                alpha = ribbon_alpha, linetype = 0, show.legend = FALSE) +
    geom_line(linewidth = 1.8) +
    geom_point() +
    scale_color_manual(values = tier_colors) +
    scale_fill_manual(values = tier_colors) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_size_manual(values = c(4, 4, 4, 5.5)) +
    scale_x_continuous(breaks = c(0, 5, 20, 50, 90, 100),
                       labels = c("0", "5", "20", "50", "90", "100")) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    labs(x = "Market Participation Rate (%)", y = "Mean Candidate Utility",
         color = "Candidate Tier", linetype = "Candidate Tier",
         shape = "Candidate Tier", size = "Candidate Tier") +
    plot_theme

  list(plot = p,
       unconditional_welfare = unconditional_welfare,
       conditional_welfare = conditional_welfare)
}


# =============================================================================
# 4b. CANDIDATE WELFARE BY TIER (unconditional: unmatched candidate utility = 0)
# =============================================================================
fig_candidate_welfare_unconditional <- function(all_sim_results,
                                                year_filter = c(1, 10),
                                                include_scramble = FALSE,
                                                hire_rounds = NULL) {

  rate_chrs <- names(all_sim_results$sim_results)

  welfare_by_rep <- purrr::map_dfr(rate_chrs, function(rate_chr) {
    rate <- as.numeric(rate_chr)
    roster <- .get_roster(all_sim_results, rate_chr, year_filter)
    df <- .get_results(all_sim_results, rate_chr, year_filter)
    if (nrow(df) == 0 || nrow(roster) == 0) return(tibble())

    matches <- df %>% dplyr::filter(accepted == 1)
    if (!include_scramble)
      matches <- matches %>% dplyr::filter(interviewed == 1 | is.na(interviewed))
    if (!is.null(hire_rounds))
      matches <- matches %>% dplyr::filter(is.na(offer_round) | offer_round <= hire_rounds)
    matches <- matches %>%
      mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))

    roster %>%
      left_join(matches %>%
                  group_by(replicate, year, cand_id) %>%
                  slice(1) %>%
                  summarise(welfare = first(V_ij), .groups = "drop"),
                by = c("replicate", "year", "cand_id")) %>%
      mutate(welfare = coalesce(welfare, 0)) %>%
      group_by(replicate, quality_tier) %>%
      summarise(
        n_cand = n(),
        mean_welfare = mean(welfare, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(participation_rate = rate)
  })

  if (nrow(welfare_by_rep) == 0) {
    message("No candidate welfare data"); return(NULL)
  }

  unconditional_welfare <- welfare_by_rep %>%
    group_by(participation_rate, quality_tier) %>%
    summarise(
      mean_welfare = mean(mean_welfare, na.rm = TRUE),
      se_mean_welfare = sd(mean_welfare, na.rm = TRUE) / sqrt(sum(!is.na(mean_welfare))),
      .groups = "drop"
    ) %>%
    mutate(quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))

  ribbon_alpha <- 0.15
  plot_theme <- theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1.5, "lines"),
          legend.key.width = unit(2.5, "lines"),
          legend.spacing.x = unit(0.5, "cm"),
          plot.margin = margin(10, 10, 10, 10))

  p <- ggplot(unconditional_welfare,
              aes(x = participation_rate * 100,
                  y = mean_welfare,
                  color = quality_tier,
                  linetype = quality_tier,
                  shape = quality_tier,
                  size = quality_tier,
                  group = quality_tier)) +
    geom_ribbon(aes(ymin = mean_welfare - 1.96 * se_mean_welfare,
                    ymax = mean_welfare + 1.96 * se_mean_welfare,
                    fill = quality_tier),
                alpha = ribbon_alpha, linetype = 0, show.legend = FALSE) +
    geom_line(linewidth = 1.8) +
    geom_point() +
    scale_color_manual(values = tier_colors) +
    scale_fill_manual(values = tier_colors) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_size_manual(values = c(4, 4, 4, 5.5)) +
    scale_x_continuous(breaks = c(0, 5, 20, 50, 90, 100),
                       labels = c("0", "5", "20", "50", "90", "100")) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    labs(x = "Market Participation Rate (%)", y = "Mean Candidate Welfare",
         color = "Candidate Tier", linetype = "Candidate Tier",
         shape = "Candidate Tier", size = "Candidate Tier") +
    plot_theme

  list(plot = p, unconditional_welfare = unconditional_welfare)
}

# =============================================================================
# 4c. AGGREGATE CANDIDATE WELFARE (unconditional, all tiers pooled)
# =============================================================================
fig_candidate_welfare <- function(all_sim_results,
                                            year_filter = c(1, 10),
                                            include_scramble = FALSE,
                                            hire_rounds = NULL) {

  rate_chrs <- names(all_sim_results$sim_results)

  welfare_by_rep <- purrr::map_dfr(rate_chrs, function(rate_chr) {
    rate <- as.numeric(rate_chr)
    roster <- .get_roster(all_sim_results, rate_chr, year_filter)
    df <- .get_results(all_sim_results, rate_chr, year_filter)
    if (nrow(df) == 0 || nrow(roster) == 0) return(tibble())

    matches <- df %>% dplyr::filter(accepted == 1)
    if (!include_scramble)
      matches <- matches %>% dplyr::filter(interviewed == 1 | is.na(interviewed))
    if (!is.null(hire_rounds))
      matches <- matches %>% dplyr::filter(is.na(offer_round) | offer_round <= hire_rounds)
    matches <- matches %>%
      mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))

    roster %>%
      left_join(matches %>%
                  group_by(replicate, year, cand_id) %>%
                  slice(1) %>%
                  summarise(welfare = first(V_ij), .groups = "drop"),
                by = c("replicate", "year", "cand_id")) %>%
      mutate(welfare = coalesce(welfare, 0)) %>%
      group_by(replicate) %>%
      summarise(
        n_cand = n(),
        mean_welfare = mean(welfare, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(participation_rate = rate)
  })

  if (nrow(welfare_by_rep) == 0) {
    message("No candidate welfare data"); return(NULL)
  }

  agg_welfare <- welfare_by_rep %>%
    group_by(participation_rate) %>%
    summarise(
      mean_welfare = mean(mean_welfare, na.rm = TRUE),
      se_mean_welfare = sd(mean_welfare, na.rm = TRUE) / sqrt(sum(!is.na(mean_welfare))),
      .groups = "drop"
    )

  ribbon_alpha <- 0.15
  plot_theme <- theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          plot.margin = margin(10, 10, 10, 10))

  p <- ggplot(agg_welfare,
              aes(x = participation_rate * 100, y = mean_welfare)) +
    geom_ribbon(aes(ymin = mean_welfare - 1.96 * se_mean_welfare,
                    ymax = mean_welfare + 1.96 * se_mean_welfare),
                alpha = ribbon_alpha, fill = "steelblue") +
    geom_line(linewidth = 1.8, color = "steelblue") +
    geom_point(size = 4, color = "steelblue") +
    scale_x_continuous(breaks = c(0, 5, 20, 50, 90, 100),
                       labels = c("0", "5", "20", "50", "90", "100")) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    labs(x = "Market Participation Rate (%)", y = "Mean Candidate Welfare") +
    plot_theme

  list(plot = p, aggregate_welfare = agg_welfare)
}


# =============================================================================
# 5. CANDIDATE BY PARTICIPATION (Participating vs Non-participating)
# =============================================================================
fig_participation_welfare <- function(all_sim_results,
                                                year_filter = c(1, 10),
                                                include_scramble = FALSE,
                                                hire_rounds = NULL) {
  rate_chrs <- names(all_sim_results$sim_results)
  # Interior rates: exclude baseline (0) and full (1)
  interior_rates <- setdiff(rate_chrs, c("0", "1"))
  
  if (length(interior_rates) == 0) {
    message("No interior rates for participation comparison")
    return(NULL)
  }
  
  # --- Baseline stats (averaged over replicates) ---
  bl_roster <- .get_roster(all_sim_results, "0", year_filter)
  bl_results <- .get_results(all_sim_results, "0", year_filter) %>%
    dplyr::filter(accepted == 1)
  if (!include_scramble)
    bl_results <- bl_results %>% dplyr::filter(interviewed == 1 | is.na(interviewed))
  if (!is.null(hire_rounds))
    bl_results <- bl_results %>% dplyr::filter(is.na(offer_round) | offer_round <= hire_rounds)
  bl_results <- bl_results %>%
    mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))
  
  bl_per_rep <- bl_roster %>%
    left_join(bl_results %>%
                group_by(replicate, year, cand_id) %>%
                slice(1) %>%
                summarise(welfare = first(V_ij), .groups = "drop"),
              by = c("replicate", "year", "cand_id")) %>%
    mutate(welfare = coalesce(welfare, 0), matched = welfare > 0) %>%
    group_by(replicate) %>%
    summarise(mean_welfare = mean(welfare),
              matching_rate = mean(matched),
              mean_welfare_if_matched = mean(welfare[matched], na.rm = TRUE),
              .groups = "drop")
  
  baseline_stats <- bl_per_rep %>%
    summarise(
      baseline_mean_welfare = mean(mean_welfare, na.rm = TRUE),
      baseline_matching_rate = mean(matching_rate, na.rm = TRUE),
      baseline_mean_welfare_if_matched = mean(mean_welfare_if_matched, na.rm = TRUE)
    )
  
  cat("\n=== BASELINE STATISTICS (averaged over replicates) ===\n")
  if (!include_scramble) cat("  (scramble hires EXCLUDED)\n")
  cat(sprintf("Mean welfare: %.4f | Matching rate: %.2f%% | Mean welfare if matched: %.4f\n",
              baseline_stats$baseline_mean_welfare,
              baseline_stats$baseline_matching_rate * 100,
              baseline_stats$baseline_mean_welfare_if_matched))
  
  # --- Interior rates: per (replicate, rate, participates) ---
  comparison_by_rep <- purrr::map_dfr(interior_rates, function(rate_chr) {
    rate <- as.numeric(rate_chr)
    roster <- .get_roster(all_sim_results, rate_chr, year_filter)
    if (!"participates" %in% names(roster)) return(tibble())
    
    df <- .get_results(all_sim_results, rate_chr, year_filter) %>%
      dplyr::filter(accepted == 1)
    if (!include_scramble)
      df <- df %>% dplyr::filter(interviewed == 1 | is.na(interviewed))
    if (!is.null(hire_rounds))
      df <- df %>% dplyr::filter(is.na(offer_round) | offer_round <= hire_rounds)
    df <- df %>% mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))
    
    matched_agg <- df %>%
      group_by(replicate, year, cand_id) %>%
      slice(1) %>%
      summarise(welfare = first(V_ij), .groups = "drop")
    
    roster %>%
      left_join(matched_agg, by = c("replicate", "year", "cand_id")) %>%
      mutate(welfare = coalesce(welfare, 0), matched = welfare > 0) %>%
      group_by(replicate, participates) %>%
      summarise(
        participation_rate = rate,
        n_candidates = n(),
        n_matched = sum(matched),
        matching_rate = mean(matched),
        mean_welfare = mean(welfare),
        mean_welfare_if_matched = mean(welfare[matched], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(participation_status = ifelse(participates, "Participating", "Non-participating"))
  })
  
  if (nrow(comparison_by_rep) == 0) {
    message("No comparison data"); return(NULL)
  }
  
  # Average across replicates
  comparison_data <- comparison_by_rep %>%
    group_by(participation_rate, participates, participation_status) %>%
    summarise(
      n_candidates = mean(n_candidates, na.rm = TRUE),
      n_matched = mean(n_matched, na.rm = TRUE),
      matching_rate = mean(matching_rate, na.rm = TRUE),
      se_matching_rate = sd(matching_rate, na.rm = TRUE) / sqrt(sum(!is.na(matching_rate))),
      mean_welfare = mean(mean_welfare, na.rm = TRUE),
      se_welfare = sd(mean_welfare, na.rm = TRUE) / sqrt(sum(!is.na(mean_welfare))),
      mean_welfare_if_matched = mean(mean_welfare_if_matched, na.rm = TRUE),
      se_welfare_if_matched = sd(mean_welfare_if_matched, na.rm = TRUE) / sqrt(sum(!is.na(mean_welfare_if_matched))),
      .groups = "drop"pair analysis
    )
  
  # Diagnostics
  cat("\n=== PARTICIPATING vs NON-PARTICIPATING (averaged over replicates) ===\n")
  for (rate in unique(comparison_data$participation_rate)) {
    pd <- comparison_data %>% dplyr::filter(participation_rate == rate, participates == TRUE)
    nd <- comparison_data %>% dplyr::filter(participation_rate == rate, participates == FALSE)
    if (nrow(pd) > 0 && nrow(nd) > 0) {
      cat(sprintf("Rate=%.0f%%: Part=%.4f, NonPart=%.4f, Diff=%.4f\n",
                  rate * 100, pd$mean_welfare, nd$mean_welfare,
                  pd$mean_welfare - nd$mean_welfare))
    }
  }
  
  #  Plots
  ribbon_alpha <- 0.15
  
  fig5_theme <- theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          legend.position = "bottom",
          legend.text = element_text(size = 13),
          legend.key.size = unit(1.5, "lines"),
          legend.key.width = unit(2.5, "lines"),
          legend.spacing.x = unit(0.5, "cm"),
          plot.margin = margin(10, 10, 10, 10))
  
  p1 <- ggplot(comparison_data,
               aes(x = participation_rate * 100, y = mean_welfare,
                   color = participation_status,
                   linetype = participation_status, shape = participation_status)) +
    geom_hline(aes(yintercept = baseline_stats$baseline_mean_welfare,
                   linetype = "Baseline (\u03C1 = 0%)",
                   color = "Baseline (\u03C1 = 0%)"),
               linewidth = 0.9, inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = mean_welfare - 1.96 * se_welfare,
                    ymax = mean_welfare + 1.96 * se_welfare,
                    fill = participation_status,
                    group = participation_status),
                alpha = ribbon_alpha, linetype = 0, show.legend = FALSE) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 3.5) +
    scale_color_manual(
      values = c(participation_colors, "Baseline (\u03C1 = 0%)" = "gray45"),
      breaks = c("Participating", "Non-participating", "Baseline (\u03C1 = 0%)")) +
    scale_fill_manual(values = participation_colors) +
    scale_linetype_manual(
      values = c("Participating" = "solid", "Non-participating" = "11",
                 "Baseline (\u03C1 = 0%)" = "dashed"),
      breaks = c("Participating", "Non-participating", "Baseline (\u03C1 = 0%)")) +
    scale_shape_manual(
      values = c("Participating" = 16, "Non-participating" = 17, "Baseline (\u03C1 = 0%)" = NA),
      breaks = c("Participating", "Non-participating", "Baseline (\u03C1 = 0%)")) +
    scale_x_continuous(breaks = c(5, 20, 50, 90), labels = c("5", "20", "50", "90")) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Market Participation Rate (%)", y = "Mean Candidate Welfare",
         color = NULL, linetype = NULL, shape = NULL) +
    guides(linetype = "none", shape = "none",
           color = guide_legend(
             title = NULL,
             override.aes = list(
               linetype = c("solid", "11", "dashed"),
               shape = c(16, 17, NA),
               linewidth = c(1.4, 1.4, 0.9)))) +
    fig5_theme
  
  p2 <- ggplot(comparison_data,
               aes(x = participation_rate * 100, y = matching_rate,
                   color = participation_status,
                   linetype = participation_status, shape = participation_status)) +
    geom_hline(aes(yintercept = baseline_stats$baseline_matching_rate,
                   linetype = "Baseline (\u03C1 = 0%)",
                   color = "Baseline (\u03C1 = 0%)"),
               linewidth = 0.9, inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = matching_rate - 1.96 * se_matching_rate,
                    ymax = matching_rate + 1.96 * se_matching_rate,
                    fill = participation_status,
                    group = participation_status),
                alpha = ribbon_alpha, linetype = 0, show.legend = FALSE) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 3.5) +
    scale_color_manual(
      values = c(participation_colors, "Baseline (\u03C1 = 0%)" = "gray45"),
      breaks = c("Participating", "Non-participating", "Baseline (\u03C1 = 0%)")) +
    scale_fill_manual(values = participation_colors) +
    scale_linetype_manual(
      values = c("Participating" = "solid", "Non-participating" = "11",
                 "Baseline (\u03C1 = 0%)" = "dashed"),
      breaks = c("Participating", "Non-participating", "Baseline (\u03C1 = 0%)")) +
    scale_shape_manual(
      values = c("Participating" = 16, "Non-participating" = 17, "Baseline (\u03C1 = 0%)" = NA),
      breaks = c("Participating", "Non-participating", "Baseline (\u03C1 = 0%)")) +
    scale_x_continuous(breaks = c(5, 20, 50, 90), labels = c("5", "20", "50", "90")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                       limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Market Participation Rate (%)", y = "Candidate Matching Rate",
         color = NULL, linetype = NULL, shape = NULL) +
    guides(linetype = "none", shape = "none",
           color = guide_legend(
             title = NULL,
             override.aes = list(
               linetype = c("solid", "11", "dashed"),
               shape = c(16, 17, NA),
               linewidth = c(1.4, 1.4, 0.9)))) +
    fig5_theme
  
  combined <- (p1 | p2) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")

  list(plot = combined, data = comparison_data, baseline_stats = baseline_stats)
}


# =============================================================================
# count_blocking_pairs — Stability analysis
#
# Standard blocking pair definition for a one-to-one matching with unfilled
# positions and shortlisting.  A pair (d, c) blocks matching mu when:
#
#   1. d has an open position in year y  (h_j = 1)
#   2. c is eligible for d under the shortlist rule (tier-based)
#   3. (d, c) is not in mu
#   4. d strictly prefers c to mu(d):
#        - if d is filled:   U(d,c) > U(d, mu(d))
#        - if d is unfilled: always satisfied (d prefers any hire to vacancy)
#   5. c strictly prefers d to mu(c):
#        - if c is matched:  V(c,d) > V(c, mu(c))
#        - if c is unmatched: always satisfied (c prefers any job to none)
#
# Metric: average number of blocking pairs per hiring position.
#
# Returns list(plot, data, summary).
# =============================================================================
count_blocking_pairs <- function(all_sim_results,
                                 hiring_schedule,
                                 rates = c(0, 1),
                                 year_filter = NULL,
                                 max_replicates = NULL) {

  departments   <- all_sim_results$departments
  cfg           <- all_sim_results$config
  n_candidates  <- cfg$n_candidates
  burn_in_years <- cfg$burn_in_years
  base_seed     <- cfg$base_seed
  sim_years     <- cfg$sim_years
  n_departments <- nrow(departments)

  if (is.null(year_filter)) year_filter <- 1:sim_years

  rep_ids <- sort(unique(
    all_sim_results$sim_results[[as.character(rates[1])]]$results$replicate))
  if (!is.null(max_replicates)) rep_ids <- head(rep_ids, max_replicates)

  out <- vector("list", length(rep_ids) * length(year_filter) * length(rates))
  k <- 0L

  for (rep in rep_ids) {
    cat(sprintf("\r  Blocking pairs: replicate %d / %d", rep, max(rep_ids)))
    rep_seed <- base_seed + rep * 10000L

    # Regenerate candidate cohorts 
    yearly_cohorts <- vector("list", sim_years)
    for (yr in 1:sim_years)
      yearly_cohorts[[yr]] <- generate_candidates(
        n_candidates, questions, seed = rep_seed + burn_in_years + yr)

    for (yr in year_filter) {
      cohort       <- yearly_cohorts[[yr]]
      hiring_depts <- which(hiring_schedule[, yr] == 1L)
      n_hiring     <- length(hiring_depts)
      if (n_hiring == 0L) next

      # Assign candidate quality tiers 
      cand_tiers <- assign_tiers_from_quantiles(
        cohort$v_i_bar, cutpoints = c(0.10, 0.25, 0.50))

      # Shortlist eligibility: which candidates each dept considers
      # Tier 1 -> Tier 1 only; Tier 2 -> 1-2; Tier 3 -> 1-3; Tier 4 -> all
      allow_map <- list(
        "Tier 1" = "Tier 1",
        "Tier 2" = c("Tier 1", "Tier 2"),
        "Tier 3" = c("Tier 1", "Tier 2", "Tier 3"),
        "Tier 4" = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
      # eligible[i, d] = TRUE if dept d would consider candidate i
      eligible <- matrix(FALSE, n_candidates, n_hiring)
      for (idx in seq_along(hiring_depts)) {
        pt <- as.character(
          departments$prestige_tier[hiring_depts[idx]] %||% "Tier 4")
        allowed <- allow_map[[pt]] %||% allow_map[["Tier 4"]]
        eligible[, idx] <- as.character(cand_tiers) %in% allowed
      }

      # f_j, U, V matrices for all (candidate, hiring-dept) pairs
      s_j_vec <- departments$s_j[hiring_depts]
      v_i_vec <- cohort$v_i_bar
      fj_mat  <- matrix(NA_real_, n_candidates, n_hiring)
      U_mat   <- matrix(NA_real_, n_candidates, n_hiring)
      V_mat   <- matrix(NA_real_, n_candidates, n_hiring)
      for (idx in seq_along(hiring_depts)) {
        fj_mat[, idx] <- compute_f_j(
          cohort, departments[hiring_depts[idx], , drop = FALSE], questions)
        U_mat[, idx] <- department_utility(
          s_j_vec[idx], v_i_vec, fj_mat[, idx])
        V_mat[, idx] <- candidate_utility(
          v_i_vec, s_j_vec[idx], fj_mat[, idx])
      }

      for (rate in rates) {
        # Extract matching mu for specific (replicate, year, rate)
        matches <- all_sim_results$sim_results[[as.character(rate)]]$results %>%
          dplyr::filter(replicate == rep, year == yr, accepted == 1L)
        n_matches <- nrow(matches)

        dept_hire  <- rep(NA_integer_, n_hiring)
        cand_match <- rep(NA_integer_, n_candidates)
        for (r in seq_len(n_matches)) {
          d_idx <- match(matches$dept_id[r],
                         departments$dept_id[hiring_depts])
          if (!is.na(d_idx)) {
            dept_hire[d_idx]              <- matches$cand_id[r]
            cand_match[matches$cand_id[r]] <- d_idx
          }
        }

        # Thresholds: utility of current partner (0 if unfilled/unmatched)
        dept_threshold <- numeric(n_hiring)
        for (d in seq_len(n_hiring))
          if (!is.na(dept_hire[d]))
            dept_threshold[d] <- U_mat[dept_hire[d], d]

        cand_threshold <- numeric(n_candidates)
        for (i in seq_len(n_candidates))
          if (!is.na(cand_match[i]))
            cand_threshold[i] <- V_mat[i, cand_match[i]]

        #  Count blocking pairs per position, under shortlist rule 
        dept_bp_count <- integer(n_hiring)

        for (d in seq_len(n_hiring)) {
          bp <- eligible[, d] &
            (U_mat[, d] > dept_threshold[d]) &
            (V_mat[, d] > cand_threshold)
          if (!is.na(dept_hire[d])) bp[dept_hire[d]] <- FALSE
          dept_bp_count[d] <- sum(bp)
        }

        total_bp         <- sum(dept_bp_count)
        n_eligible_pairs <- sum(eligible)

        # Per-tier aggregation
        dept_tier_vec <- as.character(
          departments$prestige_tier[hiring_depts])
        tier_labels <- c("Tier 1","Tier 2","Tier 3","Tier 4")
        tier_bp    <- setNames(numeric(4), tier_labels)
        tier_elig  <- setNames(numeric(4), tier_labels)
        tier_ndept <- setNames(numeric(4), tier_labels)
        for (tl in tier_labels) {
          idx <- which(dept_tier_vec == tl)
          tier_bp[[tl]]    <- sum(dept_bp_count[idx])
          tier_elig[[tl]]  <- sum(eligible[, idx])
          tier_ndept[[tl]] <- length(idx)
        }

        k <- k + 1L
        out[[k]] <- tibble::tibble(
          participation_rate = rate,
          replicate          = rep,
          year               = yr,
          n_blocking_pairs   = total_bp,
          n_eligible_pairs   = n_eligible_pairs,
          n_hiring_depts     = n_hiring,
          n_filled           = sum(!is.na(dept_hire)),
          n_matches          = n_matches,
          bp_t1 = tier_bp[1], elig_t1 = tier_elig[1], nd_t1 = tier_ndept[1],
          bp_t2 = tier_bp[2], elig_t2 = tier_elig[2], nd_t2 = tier_ndept[2],
          bp_t3 = tier_bp[3], elig_t3 = tier_elig[3], nd_t3 = tier_ndept[3],
          bp_t4 = tier_bp[4], elig_t4 = tier_elig[4], nd_t4 = tier_ndept[4]
        )
      }
    }
  }
  cat("\n")
  data <- dplyr::bind_rows(out[1:k])

  # Blocking pair rate
  data <- data %>%
    dplyr::mutate(
      bp_rate = n_blocking_pairs / n_eligible_pairs)

  # Summary: average across years, grouped by participation rate
  rep_avg <- data %>%
    dplyr::group_by(participation_rate, replicate) %>%
    dplyr::summarise(
      bp_rate_avg = mean(bp_rate), .groups = "drop")

  summary_tbl <- rep_avg %>%
    dplyr::group_by(participation_rate) %>%
    dplyr::summarise(
      mean_bp_rate = mean(bp_rate_avg),
      se_bp_rate   = sd(bp_rate_avg) / sqrt(dplyr::n()),
      .groups = "drop")

  # Tier summary
  tier_long <- data %>%
    dplyr::transmute(
      participation_rate, replicate, year,
      `Tier 1` = bp_t1 / (nd_t1 * n_candidates),
      `Tier 2` = bp_t2 / (nd_t2 * n_candidates),
      `Tier 3` = bp_t3 / (nd_t3 * n_candidates),
      `Tier 4` = bp_t4 / (nd_t4 * n_candidates)) %>%
    tidyr::pivot_longer(
      cols = starts_with("Tier"),
      names_to = "dept_tier",
      values_to = "bp_rate_tier") %>%
    dplyr::group_by(participation_rate, replicate, dept_tier) %>%
    dplyr::summarise(
      bp_rate_avg = mean(bp_rate_tier, na.rm = TRUE),
      .groups = "drop")

  tier_summary <- tier_long %>%
    dplyr::group_by(participation_rate, dept_tier) %>%
    dplyr::summarise(
      mean_bp_rate = mean(bp_rate_avg),
      se_bp_rate   = sd(bp_rate_avg) / sqrt(dplyr::n()),
      .groups = "drop") %>%
    dplyr::mutate(
      dept_tier = factor(dept_tier,
                         levels = c("Tier 1","Tier 2",
                                    "Tier 3","Tier 4")))

  list(data = data, summary = summary_tbl, tier_summary = tier_summary)
}

# =============================================================================
# 6. Plot blocking pair results from count_blocking_pairs
# =============================================================================
fig_blocking_pairs <- function(bp_result) {

  summary_tbl  <- bp_result$summary
  tier_summary <- bp_result$tier_summary

  tier_colors <- c("Tier 1" = "#E41A1C", "Tier 2" = "#377EB8",
                    "Tier 3" = "#4DAF4A", "Tier 4" = "#984EA3")

  plot_theme <- theme_minimal(base_size = 14) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major   = element_line(color = "gray90", linewidth = 0.5),
          axis.title         = element_text(size = 14, face = "bold"),
          axis.text          = element_text(size = 12),
          plot.margin        = margin(10, 10, 10, 10))

  p <- ggplot(summary_tbl,
              aes(x = participation_rate * 100, y = mean_bp_rate)) +
    geom_line(linewidth = 1.8, color = "#2C6FAC") +
    geom_point(size = 4, color = "#2C6FAC") +
    scale_x_continuous(
      breaks = c(0, 5, 20, 50, 90, 100),
      labels = c("0", "5", "20", "50", "90", "100")) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      expand = expansion(mult = c(0.05, 0.05))) +
    labs(x = "Market Participation Rate (%)",
         y = "Blocking Pair Rate") +
    plot_theme

  p_tier <- ggplot(tier_summary,
                   aes(x = participation_rate * 100,
                       y = mean_bp_rate,
                       color = dept_tier, linetype = dept_tier,
                       shape = dept_tier, group = dept_tier)) +
    geom_line(linewidth = 1.8) +
    geom_point(aes(size = dept_tier)) +
    scale_color_manual(values = tier_colors) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_size_manual(values = c(4, 4, 4, 5.5)) +
    scale_x_continuous(
      breaks = c(0, 5, 20, 50, 90, 100),
      labels = c("0", "5", "20", "50", "90", "100")) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      expand = expansion(mult = c(0.05, 0.05))) +
    labs(x = "Market Participation Rate (%)",
         y = "Blocking Pair Rate",
         color = "Department Tier", linetype = "Department Tier",
         shape = "Department Tier", size = "Department Tier") +
    plot_theme +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1.5, "lines"),
          legend.key.width = unit(2.5, "lines"),
          legend.spacing.x = unit(0.5, "cm"))

  list(plot = p, plot_tier = p_tier,
       summary = summary_tbl, tier_summary = tier_summary)
}