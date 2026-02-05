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
  if (is.null(prestige_sensitivity)) {
    beta <- 0.15 + 0.25 * v_i_bar^1.5
  } else {
    beta <- prestige_sensitivity
  }
  V_ij <- (s_j + 1e-8)^beta * (f_j + 1e-8)^(1 - beta)
  pmin(pmax(V_ij, 1e-6), 1 - 1e-6)
}

true_utility <- function(s_j, v_i_bar, f_j, dept_tier, cand_tier,
                         quality_weight_base = 0.85, fit_weight_max = 0.40) {
  alpha <- quality_weight_base - (4 - dept_tier) * 0.05
  alpha <- pmax(alpha, 1 - fit_weight_max)
  (v_i_bar + 1e-8)^alpha * (f_j + 1e-8)^(1 - alpha)
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
        paste(head(unique(values_char[unmatched_mask]), 5), collapse = ", "), "\n")
    indices[unmatched_mask] <- 1L
  }
  indices <- as.integer(indices)
  out_of_range <- indices < 1 | indices > n_levels
  if (any(out_of_range, na.rm = TRUE)) indices <- pmax(1L, pmin(indices, n_levels))
  indices
}


# =============================================================================
# ORIGINAL calculate_f_j - kept for single-row calls (e.g. scramble fallback)
# =============================================================================
calculate_f_j <- function(candidate_row, department_row, questions, gamma = 2.5) {
  if (is.data.frame(candidate_row))  candidate_row  <- as.list(candidate_row[1, ])
  if (is.data.frame(department_row)) department_row <- as.list(department_row[1, ])
  num_scores <- numeric(0)
  cand_col <- candidate_row[["q_q4_cost_of_living"]]
  dept_col <- department_row[["q4_cost_of_living"]]
  if (!is.null(cand_col) && !is.na(cand_col) && !is.null(dept_col) && !is.na(dept_col)) {
    cand_norm <- pmin(pmax((as.numeric(cand_col) - 60000) / 80000, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_col) - 60000) / 80000, 0), 1)
    s_col <- if (dept_norm <= cand_norm + 0.1) 1.0 else pmax(0, 1 - 1.5 * (dept_norm - cand_norm - 0.1))
    num_scores <- c(num_scores, s_col)
  }
  cand_sal <- candidate_row[["q_q6_typical_salary_range"]]
  dept_sal <- department_row[["q6_typical_salary_range"]]
  if (!is.null(cand_sal) && !is.na(cand_sal) && !is.null(dept_sal) && !is.na(dept_sal)) {
    cand_norm <- pmin(pmax((as.numeric(cand_sal) - 80000) / 120000, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_sal) - 80000) / 120000, 0), 1)
    s_sal <- if (dept_norm >= cand_norm - 0.1) 1.0 else pmax(0, 1 - 1.5 * (cand_norm - dept_norm - 0.1))
    num_scores <- c(num_scores, s_sal)
  }
  cand_phd <- candidate_row[["q_q14_phd_student_ratio"]]
  dept_phd <- department_row[["q14_phd_student_ratio"]]
  if (!is.null(cand_phd) && !is.na(cand_phd) && !is.null(dept_phd) && !is.na(dept_phd)) {
    cand_norm <- pmin(pmax((as.numeric(cand_phd) - 0.5) / 4.5, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_phd) - 0.5) / 4.5, 0), 1)
    num_scores <- c(num_scores, 1 - abs(cand_norm - dept_norm))
  }
  cat_scores <- numeric(0); cat_weights <- numeric(0)
  for (q_name in names(questions$categorical)) {
    cand_val <- candidate_row[[paste0("q_", q_name)]]
    dept_val <- department_row[[q_name]]
    if (is.null(cand_val) || is.na(cand_val) || is.null(dept_val) || is.na(dept_val)) next
    if (q_name == "q2_region") {
      cand_regions <- if(is.character(cand_val)) trimws(strsplit(cand_val, ",")[[1]]) else as.character(cand_val)
      s_k <- as.numeric(as.character(dept_val) %in% cand_regions)
      cat_scores <- c(cat_scores, s_k); cat_weights <- c(cat_weights, 3.0)
    } else {
      cand_char <- as.character(cand_val); dept_char <- as.character(dept_val)
      if (cand_char == dept_char) { s_k <- 1.0
      } else {
        lvls <- questions$categorical[[q_name]]
        cand_pos <- match(cand_char, lvls); dept_pos <- match(dept_char, lvls)
        s_k <- if (!is.na(cand_pos) && !is.na(dept_pos)) pmax(0, 1 - abs(cand_pos - dept_pos) / (length(lvls) - 1)) else 0.5
      }
      weight <- switch(q_name, q1_geographic_setting = 2.0, q9_typical_teaching_load = 2.5,
                       q7_typical_startup = 1.5, q8_guaranteed_summer = 1.5,
                       q5_dual_career = 2.0, q12_research_culture = 1.5, 1.0)
      cat_scores <- c(cat_scores, s_k); cat_weights <- c(cat_weights, weight)
    }
  }
  num_avg <- if (length(num_scores) > 0) mean(num_scores) else 0.5
  cat_avg <- if (length(cat_scores) > 0) sum(cat_scores * cat_weights) / sum(cat_weights) else 0.5
  S_ij <- 0.4 * num_avg + 0.6 * cat_avg
  f <- (exp(gamma * S_ij) - 1) / (exp(gamma) - 1)
  as.numeric(pmin(pmax(f, 1e-6), 1 - 1e-6))
}


# =============================================================================
# *** OPTIMIZATION #1: VECTORIZED calculate_f_j_batch ***
# Computes f_j for ALL candidates vs ONE department in a single call.
# Replaces the vapply(..., calculate_f_j, ...) pattern that was the #1 bottleneck.
# Expected speedup: 50-100x for f_j computation.
# =============================================================================
calculate_f_j_batch <- function(candidates_df, dept_row, questions, gamma = 2.5) {
  n <- nrow(candidates_df)
  if (n == 0) return(numeric(0))
  if (is.data.frame(dept_row)) dept_row <- as.list(dept_row[1, ])
  eps <- 1e-6
  
  # --- NUMERICAL SCORES (fully vectorized) ---
  num_scores <- list()
  dept_col_val <- dept_row[["q4_cost_of_living"]]
  if (!is.null(dept_col_val) && !is.na(dept_col_val)) {
    cand_raw <- as.numeric(candidates_df[["q_q4_cost_of_living"]])
    cand_norm <- pmin(pmax((cand_raw - 60000) / 80000, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_col_val) - 60000) / 80000, 0), 1)
    diff_val <- dept_norm - cand_norm - 0.1
    num_scores[[length(num_scores) + 1]] <- ifelse(is.na(cand_raw), NA_real_,
                                                   ifelse(diff_val <= 0, 1.0, pmax(0, 1 - 1.5 * diff_val)))
  }
  dept_sal_val <- dept_row[["q6_typical_salary_range"]]
  if (!is.null(dept_sal_val) && !is.na(dept_sal_val)) {
    cand_raw <- as.numeric(candidates_df[["q_q6_typical_salary_range"]])
    cand_norm <- pmin(pmax((cand_raw - 80000) / 120000, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_sal_val) - 80000) / 120000, 0), 1)
    diff_val <- cand_norm - dept_norm - 0.1
    num_scores[[length(num_scores) + 1]] <- ifelse(is.na(cand_raw), NA_real_,
                                                   ifelse(diff_val <= 0, 1.0, pmax(0, 1 - 1.5 * diff_val)))
  }
  dept_phd_val <- dept_row[["q14_phd_student_ratio"]]
  if (!is.null(dept_phd_val) && !is.na(dept_phd_val)) {
    cand_raw <- as.numeric(candidates_df[["q_q14_phd_student_ratio"]])
    cand_norm <- pmin(pmax((cand_raw - 0.5) / 4.5, 0), 1)
    dept_norm <- pmin(pmax((as.numeric(dept_phd_val) - 0.5) / 4.5, 0), 1)
    num_scores[[length(num_scores) + 1]] <- ifelse(is.na(cand_raw), NA_real_,
                                                   1 - abs(cand_norm - dept_norm))
  }
  if (length(num_scores) > 0) {
    num_mat <- do.call(cbind, num_scores)
    num_avg <- rowMeans(num_mat, na.rm = TRUE)
    num_avg[is.na(num_avg)] <- 0.5
  } else { num_avg <- rep(0.5, n) }
  
  # --- CATEGORICAL SCORES (vectorized per question) ---
  cat_weight_map <- c(q1_geographic_setting=2.0, q2_region=3.0, q3_airport_proximity=1.0,
                      q5_dual_career=2.0, q7_typical_startup=1.5, q8_guaranteed_summer=1.5,
                      q9_typical_teaching_load=2.5, q10_course_types=1.0, q11_mentoring_program=1.0,
                      q12_research_culture=1.5, q13_publication_venues=1.0, q15_medical_school_proximity=1.0)
  cat_score_sum <- rep(0, n); cat_weight_sum <- rep(0, n)
  
  for (q_name in names(questions$categorical)) {
    dept_val <- dept_row[[q_name]]
    if (is.null(dept_val) || is.na(dept_val)) next
    dept_char <- as.character(dept_val)
    cand_col_name <- paste0("q_", q_name)
    if (!cand_col_name %in% names(candidates_df)) next
    cand_vals <- candidates_df[[cand_col_name]]
    valid <- !is.na(cand_vals)
    if (!any(valid)) next
    w <- cat_weight_map[[q_name]] %||% 1.0
    
    if (q_name == "q2_region") {
      s_k <- as.numeric(grepl(dept_char, as.character(cand_vals), fixed = TRUE))
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
      } else { s_k <- rep(0.5, n) }
      s_k[!valid] <- 0
    }
    cat_score_sum <- cat_score_sum + s_k * w
    cat_weight_sum <- cat_weight_sum + valid * w
  }
  cat_avg <- ifelse(cat_weight_sum > 0, cat_score_sum / cat_weight_sum, 0.5)
  
  # --- COMBINE ---
  S_ij <- 0.4 * num_avg + 0.6 * cat_avg
  f <- (exp(gamma * S_ij) - 1) / (exp(gamma) - 1)
  pmin(pmax(f, eps), 1 - eps)
}

# =============================================================================
# OPTIMIZED generate_candidates_new - Vectorized categorical sampling
# =============================================================================
generate_candidates_new <- function(n_candidates, questions, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  candidates <- tibble(
    cand_id = 1:n_candidates,
    v_i1 = rbeta(n_candidates, 17, 8) * 0.9,
    v_i2 = rbeta(n_candidates, 17, 8) * 0.9,
    v_i3 = rbeta(n_candidates, 17, 8) * 0.9
  ) %>% mutate(v_i_bar = (v_i1 * v_i2 * v_i3)^(1/3))
  
  candidates <- candidates %>%
    mutate(quality_pctl = percent_rank(v_i_bar),
           within_tier_rank = case_when(
             quality_pctl >= 0.90 ~ (1 - quality_pctl) / 0.10,
             quality_pctl >= 0.75 ~ (0.90 - quality_pctl) / 0.15,
             quality_pctl >= 0.50 ~ (0.75 - quality_pctl) / 0.25,
             TRUE ~ (0.50 - quality_pctl) / 0.50))
  
  n <- n_candidates; v_pctl <- candidates$quality_pctl
  within_rank <- candidates$within_tier_rank
  flexibility <- 0.3 + 0.7 * within_rank
  
  # Helper: vectorized weighted sampling (cumulative-probability trick)
  vsample <- function(choices, prob_matrix) {
    pm <- prob_matrix / rowSums(prob_matrix)
    u <- runif(nrow(pm))
    cum <- t(apply(pm, 1, cumsum))
    idx <- rowSums(u > cum) + 1L
    choices[pmin(idx, length(choices))]
  }
  
  # Numerical questions (already vectorized)
  candidates$q_q6_typical_salary_range <- pmax(80000, pmin(180000,
                                                           80000 + 60000 * v_pctl^1.2 - flexibility * 15000 + rnorm(n, 0, 12000)))
  candidates$q_q4_cost_of_living <- pmax(60000, pmin(140000,
                                                     70000 + 40000 * v_pctl + flexibility * 20000 + rnorm(n, 0, 15000)))
  candidates$q_q14_phd_student_ratio <- pmax(0.5, pmin(5.0,
                                                       1.0 + 2.5 * runif(n) + 0.5 * v_pctl + rnorm(n, 0, 0.5)))
  
  # Geographic setting (vectorized)
  pm <- cbind((1-flexibility)*0.50 + flexibility*0.25, (1-flexibility)*0.30 + flexibility*0.30,
              (1-flexibility)*0.15 + flexibility*0.30, (1-flexibility)*0.05 + flexibility*0.15)
  candidates$q_q1_geographic_setting <- vsample(c("A","B","C","D"), pm)
  
  # Region (multi-select, must loop but optimized)
  n_reg <- pmin(pmax(round(1 + 3 * flexibility + runif(n)), 1), 5)
  reg_names <- c("Northeast","West Coast","Midwest","Southeast","Southwest")
  reg_probs <- c(0.22, 0.22, 0.20, 0.20, 0.16)
  candidates$q_q2_region <- vapply(n_reg, function(nr)
    paste(sample(reg_names, nr, prob = reg_probs, replace = FALSE), collapse = ","), character(1))
  
  # Teaching load (vectorized)
  pm <- cbind(pmax(0.50-0.3*flexibility,0.02), 0.30, pmax(0.15+0.2*flexibility,0.02), pmax(0.05+0.1*flexibility,0.02))
  candidates$q_q9_typical_teaching_load <- vsample(c("A","B","C","D"), pm)
  
  # Startup (vectorized)
  pm <- cbind((0.05+0.1*flexibility)*(1-0.3*v_pctl)+0.05*0.3*v_pctl,
              (0.15+0.1*flexibility)*(1-0.3*v_pctl)+0.10*0.3*v_pctl,
              0.30*(1-0.3*v_pctl)+0.25*0.3*v_pctl,
              (0.30-0.1*flexibility)*(1-0.3*v_pctl)+0.30*0.3*v_pctl,
              (0.20-0.1*flexibility)*(1-0.3*v_pctl)+0.30*0.3*v_pctl)
  pm <- pmax(pm, 0.02)
  candidates$q_q7_typical_startup <- vsample(c("A","B","C","D","E"), pm)
  
  # Simple categoricals (already vectorized via sample())
  candidates$q_q3_airport_proximity <- sample(c("A","B","C","D"), n, replace=TRUE, prob=c(0.35,0.35,0.20,0.10))
  candidates$q_q5_dual_career <- sample(c("Y","N"), n, replace=TRUE, prob=c(0.35,0.65))
  candidates$q_q8_guaranteed_summer <- sample(c("A","B","C","D"), n, replace=TRUE, prob=c(0.35,0.35,0.20,0.10))
  
  # Course types (vectorized)
  pm <- cbind(pmax(0.35-0.15*flexibility,0.02), 0.35, pmax(0.20+0.1*flexibility,0.02), pmax(0.10+0.05*flexibility,0.02))
  candidates$q_q10_course_types <- vsample(c("A","B","C","D"), pm)
  
  candidates$q_q11_mentoring_program <- sample(c("A","B","C","D"), n, replace=TRUE, prob=c(0.25,0.35,0.25,0.15))
  candidates$q_q12_research_culture <- sample(c("A","B","C","D","E"), n, replace=TRUE, prob=c(0.15,0.20,0.30,0.25,0.10))
  
  # Publication venues (vectorized)
  pm <- cbind(pmax(0.05-0.04*v_pctl,0.02), pmax(0.15-0.05*v_pctl,0.02), pmax(0.30,0.02),
              pmax(0.30+0.05*v_pctl,0.02), pmax(0.20+0.04*v_pctl,0.02))
  candidates$q_q13_publication_venues <- vsample(c("A","B","C","D","E"), pm)
  
  candidates$q_q15_medical_school_proximity <- sample(c("0","1"), n, replace=TRUE, prob=c(0.60,0.40))
  candidates$prestige_sensitivity <- 0.15 + 0.25 * (1 - flexibility)
  candidates %>% dplyr::select(-quality_pctl, -within_tier_rank)
}


# =============================================================================
# prepare_departments (unchanged - runs once, not a bottleneck)
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
  n_depts <- nrow(result); weight_list <- vector("list", n_depts)
  q_names <- c(questions$numerical, names(questions$categorical))
  for (j in 1:n_depts) {
    dept_tier <- result$tier[j]; base_weights <- rep(1, n_total_q)
    dept_type <- sample(1:4, 1, prob = c(0.25, 0.25, 0.30, 0.20))
    if (dept_type == 1) { base_weights <- runif(n_total_q, 0.7, 1.3)
    } else if (dept_type == 2) {
      for (i in seq_along(q_names)) {
        if (q_names[i] %in% c("q1_geographic_setting","q2_region","q3_airport_proximity")) base_weights[i] <- runif(1,2.0,4.0)
        else if (q_names[i] == "q4_cost_of_living") base_weights[i] <- runif(1,1.5,2.5)
      }
    } else if (dept_type == 3) {
      for (i in seq_along(q_names)) {
        if (q_names[i] %in% c("q14_phd_student_ratio","q12_research_culture","q13_publication_venues")) base_weights[i] <- runif(1,2.5,4.5)
        else if (q_names[i] %in% c("q9_typical_teaching_load","q10_course_types")) base_weights[i] <- runif(1,1.5,2.5)
      }
    } else if (dept_type == 4) {
      for (i in seq_along(q_names)) {
        if (q_names[i] %in% c("q6_typical_salary_range","q7_typical_startup","q8_guaranteed_summer")) base_weights[i] <- runif(1,2.0,4.0)
      }
    }
    if (dept_tier == "Tier 1") for (i in seq_along(q_names)) if (q_names[i] %in% c("q13_publication_venues","q12_research_culture")) base_weights[i] <- base_weights[i] * runif(1,1.3,1.8)
    if (dept_tier == "Tier 4") for (i in seq_along(q_names)) if (q_names[i] %in% c("q1_geographic_setting","q4_cost_of_living")) base_weights[i] <- base_weights[i] * runif(1,1.2,1.6)
    weight_list[[j]] <- base_weights / sum(base_weights)
  }
  result$weight_vector <- weight_list
  cat("\n=== DEPARTMENT WEIGHT SUMMARY ===\n")
  weight_matrix <- do.call(rbind, weight_list); colnames(weight_matrix) <- q_names
  cat("Mean weights by question:\n"); print(round(colMeans(weight_matrix), 3))
  cat("\nWeight range (min-max) by question:\n")
  for (i in 1:ncol(weight_matrix)) cat(sprintf("  %s: [%.3f, %.3f]\n", q_names[i], min(weight_matrix[,i]), max(weight_matrix[,i])))
  result
}


generate_yearly_hiring_schedule <- function(n_departments, n_years, departments, seed = 123) {
  set.seed(seed)
  hire_prob_by_tier <- c("Tier 1"=0.6,"Tier 2"=0.6,"Tier 3"=0.6,"Tier 4"=0.6)
  hiring_schedule <- matrix(0L, nrow=n_departments, ncol=n_years)
  for (j in 1:n_departments) {
    tier <- as.character(departments$prestige_tier[j])
    prob <- hire_prob_by_tier[[tier]]; if (is.null(prob)) prob <- 0.5
    hiring_schedule[j, ] <- rbinom(n_years, size=1, prob=prob)
  }
  cat("\n=== YEARLY HIRING SCHEDULE SUMMARY ===\n")
  cat("Total department-years:", n_departments * n_years, "\n")
  cat("Total hiring events:", sum(hiring_schedule), "\n")
  cat("Overall hiring rate:", round(mean(hiring_schedule), 3), "\n\n")
  for (tier in c("Tier 1","Tier 2","Tier 3","Tier 4")) {
    tier_idx <- which(departments$prestige_tier == tier)
    if (length(tier_idx) > 0) cat(tier, ": ", sum(hiring_schedule[tier_idx,]), " hires over ",
                                  length(tier_idx)*n_years, " dept-years (rate = ", round(mean(hiring_schedule[tier_idx,]),3), ")\n", sep="")
  }
  hiring_schedule
}


# =============================================================================
# Neural network components (unchanged - torch is already optimized)
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
    self$layers <- nn_module_list(); self$batch_norms <- nn_module_list(); self$dropouts <- nn_module_list()
    prev_dim <- input_dim
    for (hidden_dim in hidden_dims) {
      self$layers$append(nn_linear(prev_dim, hidden_dim))
      self$batch_norms$append(nn_batch_norm1d(hidden_dim, momentum = 0.005))
      self$dropouts$append(nn_dropout(0.4)); prev_dim <- hidden_dim
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
      h <- self$layers[[i]](h); h <- self$batch_norms[[i]](h); h <- torch_relu(h); h <- self$dropouts[[i]](h)
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
          values <- vapply(raw_values, function(x) {
            if (is.na(x) || !is.character(x) || x == "") return(levels[1])
            parts <- strsplit(as.character(x), ",")[[1]]
            if (length(parts)==0) levels[1] else trimws(parts[1])
          }, character(1))
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
  for (col in 1:ncol(cont_matrix)) { na_mask <- is.na(cont_matrix[,col]); if (any(na_mask)) { m <- mean(cont_matrix[,col],na.rm=TRUE); if(is.na(m)) m <- 0; cont_matrix[na_mask,col] <- m } }
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
                                   include_questions = dept_model$include_questions, seed = NULL) {
  train_data <- dplyr::filter(train_data, offered == 1L)
  if (nrow(train_data) < 1) return(dept_model)
  if (!is.null(seed)) { set.seed(seed); torch::torch_manual_seed(seed) }
  nn_data <- prepare_nn_data(train_data, dept_model$questions, include_fit=include_fit, include_questions=include_questions)
  y_tensor <- torch::torch_tensor(train_data$accepted, dtype=torch::torch_float())$unsqueeze(2)
  n_train <- nrow(train_data); use_validation <- n_train >= 40
  if (use_validation) {
    val_idx <- sample.int(n_train, size=ceiling(0.2*n_train)); train_idx <- setdiff(seq_len(n_train), val_idx)
    x_train <- nn_data$x_cont[train_idx,]; y_train <- y_tensor[train_idx,]
    x_val <- nn_data$x_cont[val_idx,]; y_val <- y_tensor[val_idx,]
    x_cat_train <- lapply(nn_data$x_cat, function(t) t[train_idx]); x_cat_val <- lapply(nn_data$x_cat, function(t) t[val_idx])
  } else { x_train <- nn_data$x_cont; y_train <- y_tensor; x_cat_train <- nn_data$x_cat }
  optimizer <- optim_adam(dept_model$model$parameters, lr=0.002)
  scheduler <- lr_multiplicative(optimizer, lr_lambda=function(epoch) 0.98)
  best_val_loss <- Inf; patience <- 20; patience_counter <- 0; best_state <- NULL
  dept_model$model$train()
  for (epoch in 1:n_epochs) {
    log_odds <- dept_model$model(x_train, x_cat_train)
    bce_loss <- nn_bce_with_logits_loss(reduction="none")(log_odds, y_train)
    probs <- torch_sigmoid(log_odds)
    focal_weight <- torch_pow(1-probs*y_train-(1-probs)*(1-y_train), 2)
    loss <- torch_mean(focal_weight*bce_loss)
    l2_loss <- 0; for (param in dept_model$model$parameters) l2_loss <- l2_loss+torch_sum(param^2)
    total_loss <- loss + 0.01*exp(-epoch/100)*l2_loss
    optimizer$zero_grad(); total_loss$backward()
    nn_utils_clip_grad_norm_(dept_model$model$parameters, max_norm=1.0)
    optimizer$step(); scheduler$step()
    if (use_validation && epoch%%5==0) {
      dept_model$model$eval()
      with_no_grad({ val_log_odds <- dept_model$model(x_val, x_cat_val); val_loss <- as.numeric(nn_bce_with_logits_loss()(val_log_odds, y_val)) })
      dept_model$model$train()
      if (val_loss < best_val_loss) { best_val_loss <- val_loss; patience_counter <- 0; best_state <- lapply(dept_model$model$parameters, function(p) p$clone()$detach())
      } else { patience_counter <- patience_counter+1
      if (patience_counter >= patience) { if (!is.null(best_state)) { pl <- dept_model$model$parameters; for (i in seq_along(pl)) pl[[i]]$data <- best_state[[i]]$data }; break } }
    }
  }
  dept_model$is_trained <- TRUE; dept_model
}

# =============================================================================
# predict_acceptance_probability (unchanged logic, already efficient)
# =============================================================================
predict_acceptance_probability <- function(dept_model, applicant_data, n_bootstrap = 200,
                                           include_fit = dept_model$include_fit,
                                           include_questions = dept_model$include_questions,
                                           seed = NULL, participation_rate = NULL) {
  if (!is.null(seed)) { set.seed(seed); torch::torch_manual_seed(seed) }
  is_baseline <- is.null(participation_rate) || participation_rate == 0 || !include_fit
  if (!dept_model$is_trained || nrow(applicant_data) == 0) {
    base <- applicant_data %>% mutate(
      tier_gap = pmax(0, dept_tier - cand_tier),
      baseline_accept = case_when(tier_gap==0~0.55,tier_gap==1~0.45,tier_gap==2~0.35,tier_gap>=3~0.25),
      questionnaire_base = baseline_accept,
      fit_adjustment = case_when(f_j>=0.80~0.30,f_j>=0.65~0.20,f_j>=0.50~0.10,f_j>=0.35~0.00,TRUE~-0.05),
      reach_fit_bonus = case_when(tier_gap>=2&f_j>=0.70~0.15,tier_gap>=3&f_j>=0.75~0.20,TRUE~0.0),
      questionnaire_accept = pmin(0.90, questionnaire_base + fit_adjustment + reach_fit_bonus),
      pi_pred = if (is_baseline) pmax(0.10,pmin(0.90,baseline_accept+rnorm(n(),0,0.02)))
      else pmax(0.10,pmin(0.90,questionnaire_accept+rnorm(n(),0,0.02))))
    mu <- pmin(pmax(base$pi_pred, 1e-5), 1-1e-5)
    res <- base %>% mutate(pi_var=0) %>% dplyr::select(-tier_gap,-baseline_accept,-questionnaire_base,-fit_adjustment,-reach_fit_bonus,-questionnaire_accept)
    attr(res, "pi_draws") <- matrix(mu, nrow=1); return(res)
  }
  hist_data <- dept_model$historical_data %>% filter(offered == 1L)
  if (nrow(hist_data) < 10) {
    nn_data <- tryCatch(prepare_nn_data(applicant_data, dept_model$questions, include_fit=include_fit, include_questions=include_questions), error=function(e) NULL)
    if (is.null(nn_data)) {
      base <- applicant_data %>% mutate(tier_gap=pmax(0,dept_tier-cand_tier),
                                        pi_pred=pmax(0.10,pmin(0.90,case_when(tier_gap==0~0.55,tier_gap==1~0.45,tier_gap==2~0.35,TRUE~0.25)+rnorm(n(),0,0.02))),pi_var=0) %>% dplyr::select(-tier_gap)
      attr(base,"pi_draws") <- matrix(base$pi_pred, nrow=1); return(base)
    }
    dept_model$model$eval()
    with_no_grad({ log_odds <- dept_model$model(nn_data$x_cont, nn_data$x_cat); pi_mean <- as.numeric(torch_sigmoid(log_odds)$squeeze()$to(device="cpu")) })
    pi_mean <- pmin(pmax(pi_mean,1e-5),1-1e-5)
    res <- applicant_data %>% mutate(pi_pred=pi_mean, pi_var=0)
    attr(res,"pi_draws") <- matrix(pi_mean, nrow=1); return(res)
  }
  n_hist <- nrow(hist_data); pi_draws_matrix <- matrix(NA_real_, nrow=n_bootstrap, ncol=nrow(applicant_data))
  for (b in 1:n_bootstrap) {
    boot_data <- hist_data[sample.int(n_hist,n_hist,replace=TRUE),]
    temp_model <- initialize_department_model(dept_model$questions, include_fit=include_fit, include_questions=include_questions)
    temp_model$historical_data <- boot_data
    temp_model <- tryCatch(train_department_model(temp_model, boot_data, n_epochs=30, include_fit=include_fit, include_questions=include_questions, seed=seed+b), error=function(e) temp_model)
    if (!temp_model$is_trained) {
      tg <- pmax(0, applicant_data$dept_tier-applicant_data$cand_tier)
      pi_draws_matrix[b,] <- pmin(pmax(case_when(tg==0~0.55,tg==1~0.45,tg==2~0.35,TRUE~0.25)+rnorm(nrow(applicant_data),0,0.05),1e-5),1-1e-5); next
    }
    nn_data <- tryCatch(prepare_nn_data(applicant_data, temp_model$questions, include_fit=include_fit, include_questions=include_questions), error=function(e) NULL)
    if (is.null(nn_data)) {
      tg <- pmax(0, applicant_data$dept_tier-applicant_data$cand_tier)
      pi_draws_matrix[b,] <- pmin(pmax(case_when(tg==0~0.55,tg==1~0.45,tg==2~0.35,TRUE~0.25)+rnorm(nrow(applicant_data),0,0.05),1e-5),1-1e-5); next
    }
    temp_model$model$eval()
    with_no_grad({ log_odds <- temp_model$model(nn_data$x_cont, nn_data$x_cat); pi_b <- as.numeric(torch_sigmoid(log_odds)$squeeze()$to(device="cpu")) })
    pi_draws_matrix[b,] <- pmin(pmax(pi_b,1e-5),1-1e-5)
  }
  res <- applicant_data %>% mutate(pi_pred=colMeans(pi_draws_matrix,na.rm=TRUE), pi_var=apply(pi_draws_matrix,2,stats::var,na.rm=TRUE))
  attr(res,"pi_draws") <- pi_draws_matrix; res
}


# =============================================================================
# *** OPTIMIZATION #2: VECTORIZED make_repeated_rank_draws ***
# Bootstrap resampling via matrix indexing instead of per-draw loop
# =============================================================================
make_repeated_rank_draws <- function(applicant_data, L = 200, tuple_size = NULL,
                                     method = c("bootstrap","gumbel","gaussian"),
                                     noise_scale = 0.15, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  method <- match.arg(method); n <- nrow(applicant_data)
  if (!"U_det" %in% names(applicant_data))
    applicant_data <- applicant_data %>% mutate(U_det = exp(s_j*log(v_i_bar+1e-8)+(1-s_j)*log(f_j+1e-8)))
  mu <- pmin(pmax(applicant_data$pi_pred, 1e-5), 1-1e-5)
  Uhat_full <- applicant_data$U_det * mu
  if (is.null(tuple_size)) idx <- seq_len(n) else { M_use <- min(tuple_size, n); idx <- order(Uhat_full, decreasing=TRUE)[seq_len(M_use)] }
  M <- length(idx); U_hat <- Uhat_full[idx]
  
  if (method == "bootstrap") {
    if (!"U_true" %in% names(applicant_data))
      applicant_data <- applicant_data %>% mutate(dept_tier=dept_tier%||%4L, cand_tier=cand_tier%||%4L,
                                                  U_true=true_utility(s_j,v_i_bar,f_j,dept_tier,cand_tier))
    U_true_sub <- applicant_data$U_true[idx]; resid <- U_true_sub - U_hat
    if (all(!is.finite(resid)) || sd(resid,na.rm=TRUE)==0)
      return(list(U_draws=matrix(rep(U_hat,each=L),nrow=L,ncol=M), idx=idx, U_hat=U_hat))
    # VECTORIZED: sample all at once via matrix indexing
    boot_idx <- sample.int(M, size=M*L, replace=TRUE)
    eps_mat <- matrix(resid[boot_idx], nrow=L, ncol=M)
    U_draws <- sweep(eps_mat, 2, U_hat, "+"); U_draws[U_draws<0] <- 0
    return(list(U_draws=U_draws, idx=idx, U_hat=U_hat))
  }
  base <- matrix(rep(U_hat, each=L), nrow=L)
  if (method == "gumbel") { U_draws <- base + noise_scale*(-log(-log(matrix(runif(L*M),nrow=L,ncol=M))))
  } else { U_draws <- base + noise_scale*matrix(rnorm(L*M),nrow=L,ncol=M); U_draws[U_draws<0] <- 0 }
  list(U_draws=U_draws, idx=idx, U_hat=U_hat)
}


# =============================================================================
# *** OPTIMIZATION #3: VECTORIZED compute_c_alpha_from_draws ***
# Uses covariance matrix instead of O(n^2) loop with per-pair sd()
# =============================================================================
compute_c_alpha_from_draws <- function(U_hat, U_draws, alpha = 0.05, ridge = 0.01) {
  stopifnot(is.numeric(U_hat), is.matrix(U_draws))
  n <- length(U_hat); B <- nrow(U_draws)
  if (n <= 1L || B <= 1L) return(list(c_alpha=0, sigma_hat=matrix(0,n,n)))
  
  # Compute sigma_hat via Var(X_i - X_l) = Var(X_i) + Var(X_l) - 2*Cov(X_i,X_l)
  col_vars <- apply(U_draws, 2, var, na.rm=TRUE); col_vars[!is.finite(col_vars)] <- 0
  cov_mat <- cov(U_draws, use="pairwise.complete.obs"); cov_mat[!is.finite(cov_mat)] <- 0
  var_diff <- outer(col_vars, col_vars, "+") - 2*cov_mat
  var_diff[var_diff < 0] <- 0
  sigma_hat <- pmax(sqrt(var_diff), ridge)
  
  # delta_hat matrix
  delta_hat_mat <- outer(U_hat, U_hat, "-")
  
  # Compute max Z per bootstrap draw
  if (n <= 200) {
    # Full vectorized: extract all upper-triangle pairs
    pairs <- which(upper.tri(sigma_hat), arr.ind=TRUE); n_pairs <- nrow(pairs)
    delta_b_pairs <- U_draws[, pairs[,1], drop=FALSE] - U_draws[, pairs[,2], drop=FALSE]
    delta_hat_pairs <- delta_hat_mat[pairs]; sigma_pairs <- sigma_hat[pairs]
    z_pairs <- sweep(sweep(delta_b_pairs, 2, delta_hat_pairs, "-"), 2, sigma_pairs, "/")
    z_pairs[!is.finite(z_pairs)] <- -Inf
    Zb <- apply(z_pairs, 1, max, na.rm=TRUE)
  } else {
    # Row-chunked for larger n
    Zb <- rep(-Inf, B)
    for (i in 1:(n-1)) {
      l_idx <- (i+1):n
      z_batch <- sweep(U_draws[,i]-U_draws[,l_idx,drop=FALSE], 2, delta_hat_mat[i,l_idx], "-")
      z_batch <- sweep(z_batch, 2, sigma_hat[i,l_idx], "/")
      z_batch[!is.finite(z_batch)] <- -Inf
      Zb <- pmax(Zb, apply(z_batch, 1, max, na.rm=TRUE))
    }
  }
  Zb[!is.finite(Zb)] <- -Inf
  c_alpha <- stats::quantile(Zb, probs=1-alpha, na.rm=TRUE, names=FALSE)
  list(c_alpha=as.numeric(c_alpha), sigma_hat=sigma_hat)
}


# =============================================================================
# *** OPTIMIZATION #4: VECTORIZED compute_pairwise_lower_ranks ***
# Matrix comparison instead of O(n^2) loop
# =============================================================================
compute_pairwise_lower_ranks <- function(applicant_data, L_repeats=200, tuple_size=NULL,
                                         noise_method=c("bootstrap","gumbel","gaussian"),
                                         noise_scale=0.15, alpha=0.05, seed=NULL) {
  noise_method <- match.arg(noise_method)
  if (!"U_det" %in% names(applicant_data) || !"U_hat" %in% names(applicant_data))
    applicant_data <- applicant_data %>% mutate(U_det=exp(s_j*log(v_i_bar+1e-8)+(1-s_j)*log(f_j+1e-8)),
                                                U_hat=U_det*pmin(pmax(pi_pred,1e-5),1-1e-5))
  repack <- make_repeated_rank_draws(applicant_data, L=L_repeats, tuple_size=tuple_size,
                                     method=noise_method, noise_scale=noise_scale, seed=seed)
  idx <- repack$idx; U_draws <- repack$U_draws; U_hat <- repack$U_hat
  cand_ids <- applicant_data$cand_id[idx]; M <- length(U_hat)
  if (M <= 1L) return(tibble(cand_id=cand_ids, rank_lower=1L, pair_score=0L, mean_rank=1) %>%
                        right_join(applicant_data %>% dplyr::select(cand_id), by="cand_id") %>%
                        mutate(rank_lower=ifelse(is.na(rank_lower),Inf,rank_lower), mean_rank=ifelse(is.na(mean_rank),Inf,mean_rank)))
  
  cal <- compute_c_alpha_from_draws(U_hat, U_draws, alpha=alpha, ridge=0.0001)
  c_alpha <- cal$c_alpha; sigma_hat <- cal$sigma_hat
  if (is.na(c_alpha) || !is.finite(c_alpha)) c_alpha <- 1.96
  
  # VECTORIZED: z_mat[l,i] = (U_hat[l]-U_hat[i])/sigma[l,i]
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


select_interviews_sure_screening <- function(applicant_data, k_j, h_j, alpha=0.05, L_repeats=200,
                                             tuple_size=NULL, noise_method=c("bootstrap","gumbel","gaussian"),
                                             noise_scale=0.15, seed=NULL, tier_width=0.1) {
  noise_method <- match.arg(noise_method); n <- nrow(applicant_data)
  if (n == 0L) return(integer()); if (n <= k_j) return(applicant_data$cand_id)
  if (!"U_det" %in% names(applicant_data) || !"U_hat" %in% names(applicant_data))
    applicant_data <- applicant_data %>% dplyr::mutate(U_det=exp(s_j*log(v_i_bar+1e-8)+(1-s_j)*log(f_j+1e-8)),
                                                       U_hat=U_det*pmin(pmax(pi_pred,1e-5),1-1e-5))
  rank_tbl <- compute_pairwise_lower_ranks(applicant_data, L_repeats, tuple_size, noise_method, noise_scale, alpha, seed)
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
          weights <- exp((tier_candidates$U_hat - mean(tier_candidates$U_hat,na.rm=TRUE))/0.1); weights <- weights/sum(weights)
          additional_ids <- sample(tier_candidates$cand_id, size=min(remaining_slots,nrow(tier_candidates)), prob=weights, replace=FALSE)
        } else additional_ids <- remaining_pool %>% dplyr::arrange(dplyr::desc(U_hat)) %>% dplyr::slice_head(n=remaining_slots) %>% dplyr::pull(cand_id)
      } else additional_ids <- remaining_pool %>% dplyr::arrange(dplyr::desc(U_hat)) %>% dplyr::slice_head(n=remaining_slots) %>% dplyr::pull(cand_id)
      selected_ids <- c(sure_ids, additional_ids)
    }
  }
  attr(selected_ids, "sure_ids") <- applicant_data$cand_id[sure_idx]; selected_ids
}


generate_applications <- function(candidates, departments, questions, ...)
  tibble(cand_id=candidates$cand_id) %>% tidyr::crossing(tibble(dept_id=departments$dept_id))

generate_nested_participation_assignments <- function(n_candidates, participation_rates=c(0.05,0.20,0.50,0.90), seed=123) {
  set.seed(seed); propensities <- runif(n_candidates); cand_ids <- 1:n_candidates
  participation_sets <- list(); participation_sets[["0"]] <- integer(0)
  sorted_idx <- order(propensities)
  for (rate in participation_rates) participation_sets[[as.character(rate)]] <- cand_ids[sorted_idx[1:floor(n_candidates*rate)]]
  participation_sets[["1"]] <- cand_ids; participation_sets
}

generate_department_strategies_adaptive <- function(n_departments, n_years, participation_rate, seed=456) {
  set.seed(seed+round(participation_rate*1000))
  prob_S1 <- pmin(0.9, pmax(0.0, participation_rate))
  matrix(sample(1:2, n_departments*n_years, replace=TRUE, prob=c(prob_S1,1-prob_S1)), nrow=n_departments, ncol=n_years)
}

print_hiring_allocation <- function(all_results, year, participation_rate) {
  if (nrow(all_results)==0) { cat("\n  No results in year", year, "\n"); return(invisible(NULL)) }
  hires <- all_results %>% filter(accepted==1)
  if (nrow(hires)==0) { cat("\n  No hires in year", year, "\n"); return(invisible(NULL)) }
  hire_matrix <- hires %>%
    count(cand_tier, dept_tier) %>% complete(cand_tier=1:4, dept_tier=1:4, fill=list(n=0)) %>%
    pivot_wider(names_from=dept_tier, values_from=n, names_prefix="Dept_T", values_fill=0) %>%
    arrange(cand_tier) %>% mutate(Total=Dept_T1+Dept_T2+Dept_T3+Dept_T4, Cand_Tier=paste0("Tier ",cand_tier)) %>%
    dplyr::select(Cand_Tier, Dept_T1, Dept_T2, Dept_T3, Dept_T4, Total)
  col_totals <- tibble(Cand_Tier="Total", Dept_T1=sum(hire_matrix$Dept_T1), Dept_T2=sum(hire_matrix$Dept_T2),
                       Dept_T3=sum(hire_matrix$Dept_T3), Dept_T4=sum(hire_matrix$Dept_T4), Total=sum(hire_matrix$Total))
  hire_matrix <- bind_rows(hire_matrix, col_totals)
  cat(sprintf("\n  ╔══════════════════════════════════════════════════════════╗\n"))
  cat(sprintf("  ║  HIRING ALLOCATION MATRIX - Year %d (ρ = %.0f%%)            ║\n", year, participation_rate*100))
  cat(sprintf("  ╠══════════════════════════════════════════════════════════╣\n"))
  cat(sprintf("  ║ %-10s │ %7s %7s %7s %7s │ %7s ║\n","","Dept T1","Dept T2","Dept T3","Dept T4","Total"))
  cat(sprintf("  ╠══════════════════════════════════════════════════════════╣\n"))
  for (i in 1:(nrow(hire_matrix)-1)) { row <- hire_matrix[i,]
  cat(sprintf("  ║ %-10s │ %7d %7d %7d %7d │ %7d ║\n", row$Cand_Tier, row$Dept_T1, row$Dept_T2, row$Dept_T3, row$Dept_T4, row$Total)) }
  cat(sprintf("  ╠══════════════════════════════════════════════════════════╣\n"))
  totals <- hire_matrix[nrow(hire_matrix),]
  cat(sprintf("  ║ %-10s │ %7d %7d %7d %7d │ %7d ║\n", totals$Cand_Tier, totals$Dept_T1, totals$Dept_T2, totals$Dept_T3, totals$Dept_T4, totals$Total))
  cat(sprintf("  ╚══════════════════════════════════════════════════════════╝\n"))
  cat("\n  Mean f_j by allocation:\n")
  fit_summary <- hires %>% group_by(cand_tier,dept_tier) %>% summarise(n=n(),mean_f_j=mean(f_j,na.rm=TRUE),.groups="drop") %>% filter(n>0) %>% arrange(cand_tier,dept_tier)
  for (i in 1:nrow(fit_summary)) { row <- fit_summary[i,]; cat(sprintf("    Cand T%d → Dept T%d: n=%3d, mean_f_j=%.3f\n", row$cand_tier, row$dept_tier, row$n, row$mean_f_j)) }
  invisible(hire_matrix)
}

add_dept_info_to_learning_data <- function(ld_selected, dept_info) {
  ld_selected$q4_cost_of_living <- dept_info$q4_cost_of_living
  ld_selected$q6_typical_salary_range <- dept_info$q6_typical_salary_range
  ld_selected$q14_phd_student_ratio <- dept_info$q14_phd_student_ratio
  for (q in c("q1_geographic_setting","q2_region","q3_airport_proximity","q5_dual_career","q7_typical_startup",
              "q8_guaranteed_summer","q9_typical_teaching_load","q10_course_types","q11_mentoring_program",
              "q12_research_culture","q13_publication_venues","q15_medical_school_proximity"))
    ld_selected[[q]] <- as.character(dept_info[[q]])
  ld_selected
}

# =============================================================================
# OPTIMIZED simulate_market_year_adaptive_sequential
# KEY CHANGE: Uses calculate_f_j_batch() instead of vapply row-by-row
# =============================================================================
simulate_market_year_adaptive_sequential <- function(candidates, departments, questions, year,
                                                     dept_models_pairwise, participants_this_year, dept_strategies, yearly_hiring_schedule,
                                                     participation_rate=0, alpha=0.05, L_repeats=200, tuple_size=NULL,
                                                     noise_method="bootstrap", noise_scale=0.15, shortlist_enabled=TRUE,
                                                     collect_ranking_panel=TRUE, cand_tier_col="quality_tier", max_offer_rounds=5, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  candidates <- candidates %>% mutate(participates = cand_id %in% participants_this_year)
  tier_to_int <- function(x) as.integer(factor(as.character(x), levels=c("Tier 1","Tier 2","Tier 3","Tier 4")))
  applications <- generate_applications(candidates, departments, questions)
  all_interviewed <- tibble(); learning_data_pairwise <- vector("list", nrow(departments))
  rank_panel <- tibble(); diag_list <- vector("list", nrow(departments))
  cat(sprintf("\n=== YEAR %d: Interview Selection Phase ===\n", year))
  for (j in 1:nrow(departments)) {
    dept <- departments[j, ]; h_j_this_year <- yearly_hiring_schedule[j, year]; k_j_this_year <- 5L*h_j_this_year
    if (h_j_this_year == 0L) {
      da <- applications %>% filter(dept_id==dept$dept_id)
      if (nrow(da) > 0) { aa <- da %>% left_join(candidates, by="cand_id") %>% mutate(s_j=dept$s_j)
      diag_list[[j]] <- aa %>% mutate(year=year, dept_id=dept$dept_id, strategy="pairwise", pi_pred=NA_real_, U_true=NA_real_, U_hat=NA_real_, r_true=NA_real_, r_hat=NA_real_, interviewed=0L, considered=0L, h_j=0L, k_j=0L) }
      next
    }
    da <- applications %>% filter(dept_id==dept$dept_id); if (nrow(da)==0) next
    dept_strategy <- if (participation_rate==0) "S2" else ifelse(dept_strategies[j,year]==1,"S1","S2")
    apps_all <- da %>% left_join(candidates, by="cand_id") %>% mutate(s_j=dept$s_j)
    if (!cand_tier_col %in% names(apps_all)) apps_all[[cand_tier_col]] <- factor("Tier 4", levels=c("Tier 1","Tier 2","Tier 3","Tier 4"))
    else apps_all[[cand_tier_col]] <- factor(as.character(apps_all[[cand_tier_col]]), levels=c("Tier 1","Tier 2","Tier 3","Tier 4"))
    apps_all <- apps_all %>% mutate(dept_tier=tier_to_int(dept$prestige_tier %||% "Tier 4"), cand_tier=tier_to_int(.data[[cand_tier_col]]))
    # *** KEY OPTIMIZATION: batch f_j ***
    apps_all$f_j <- calculate_f_j_batch(apps_all, dept, questions)
    if (shortlist_enabled) {
      allow_map <- list("Tier 1"=c("Tier 1"),"Tier 2"=c("Tier 1","Tier 2"),"Tier 3"=c("Tier 1","Tier 2","Tier 3"),"Tier 4"=c("Tier 1","Tier 2","Tier 3","Tier 4"))
      pt <- as.character(dept$prestige_tier %||% "Tier 4"); allowed <- allow_map[[pt]] %||% allow_map[["Tier 4"]]
      considered_mask <- as.character(apps_all[[cand_tier_col]]) %in% allowed
    } else considered_mask <- rep(TRUE, nrow(apps_all))
    applicant_data <- apps_all[considered_mask, , drop=FALSE]
    if (nrow(applicant_data)==0) { diag_list[[j]] <- apps_all %>% mutate(year=year,dept_id=dept$dept_id,strategy="pairwise",pi_pred=NA_real_,U_true=NA_real_,U_hat=NA_real_,r_true=NA_real_,r_hat=NA_real_,interviewed=0L,considered=0L,h_j=h_j_this_year,k_j=k_j_this_year); next }
    applicant_data <- applicant_data %>% mutate(row_id=row_number())
    n_part <- sum(applicant_data$participates)
    if (participation_rate==0) { use_gmi <- FALSE; mfp <- NA_real_
    } else if (n_part>0) { use_gmi <- TRUE; mfp <- mean(applicant_data$f_j[applicant_data$participates], na.rm=TRUE)
    } else { use_gmi <- FALSE; mfp <- NA_real_ }
    applicant_data_pw <- applicant_data %>% mutate(f_j_used=case_when(!participates&dept_strategy=="S1"~0.0, !participates&dept_strategy=="S2"&use_gmi~mfp, !participates&dept_strategy=="S2"&!use_gmi~v_i_bar, TRUE~f_j))
    actual_f_j <- applicant_data_pw$f_j; ad_tmp <- applicant_data_pw; ad_tmp$f_j <- applicant_data_pw$f_j_used
    for (q in questions$numerical) ad_tmp[[q]] <- dept[[q]]
    for (qn in names(questions$categorical)) ad_tmp[[qn]] <- as.character(dept[[qn]])
    applicant_data_pw <- predict_acceptance_probability(dept_models_pairwise[[j]], ad_tmp, n_bootstrap=L_repeats, seed=j*1000+year, participation_rate=participation_rate)
    applicant_data_pw$f_j <- actual_f_j
    U_det_pw <- true_utility(applicant_data_pw$s_j, applicant_data_pw$v_i_bar, applicant_data_pw$f_j_used, applicant_data_pw$dept_tier, applicant_data_pw$cand_tier)
    applicant_data_pw <- applicant_data_pw %>% dplyr::mutate(f_j_used=applicant_data_pw$f_j_used, U_det=U_det_pw, U_hat=U_det*pmin(pmax(pi_pred,1e-5),1-1e-5))
    U_true_pw <- true_utility(applicant_data_pw$s_j, applicant_data_pw$v_i_bar, applicant_data_pw$f_j, applicant_data_pw$dept_tier, applicant_data_pw$cand_tier)
    applicant_data_pw$U_true <- U_true_pw; applicant_data_pw$r_true <- rank(-U_true_pw, ties.method="average"); applicant_data_pw$r_point <- rank(-applicant_data_pw$U_hat, ties.method="average")
    if (collect_ranking_panel) { rpair <- compute_pairwise_lower_ranks(applicant_data_pw, L_repeats, tuple_size, noise_method, noise_scale, alpha)
    applicant_data_pw <- applicant_data_pw %>% dplyr::left_join(rpair, by="cand_id") %>% dplyr::mutate(r_pair_lb=rank_lower) %>% dplyr::select(-rank_lower) }
    interviewed_ids_pw <- select_interviews_sure_screening(applicant_data_pw, k_j=k_j_this_year, h_j=h_j_this_year, alpha=alpha, L_repeats=L_repeats, tuple_size=tuple_size, noise_method=noise_method, noise_scale=noise_scale)
    applicant_data_pw$interviewed_flag <- as.integer(applicant_data_pw$cand_id %in% interviewed_ids_pw)
    if (collect_ranking_panel) rank_panel <- dplyr::bind_rows(rank_panel, applicant_data_pw %>% dplyr::transmute(year, dept_id=dept$dept_id, strategy="pairwise", cand_id, s_j, v_i_bar, f_j, participates, U_true, U_hat, r_true, r_point, r_pair_lb=dplyr::if_else(is.finite(r_pair_lb),r_pair_lb,NA_real_), interviewed=interviewed_flag, k_j=k_j_this_year, h_j=h_j_this_year))
    idpw <- applicant_data_pw %>% dplyr::filter(cand_id %in% interviewed_ids_pw) %>% dplyr::mutate(interviewed=1L, year=year, dept_id=dept$dept_id, strategy="pairwise", h_j=h_j_this_year, k_j=k_j_this_year)
    if (nrow(idpw)>0) all_interviewed <- dplyr::bind_rows(all_interviewed, idpw)
    diag_list[[j]] <- apps_all %>% dplyr::mutate(considered=as.integer(cand_id %in% applicant_data$cand_id), interviewed=as.integer(cand_id %in% interviewed_ids_pw), h_j=h_j_this_year, k_j=k_j_this_year) %>%
      dplyr::left_join(applicant_data_pw %>% dplyr::select(cand_id,pi_pred,U_true,U_hat,r_true), by="cand_id") %>%
      dplyr::transmute(year=year,dept_id=dept$dept_id,strategy="pairwise",cand_id,s_j,v_i_bar,f_j,pi_pred,U_true,U_hat,r_true,interviewed,considered,participates,h_j,k_j)
  }
  cat(sprintf("\n=== YEAR %d: Sequential Offer Resolution ===\n", year))
  if (nrow(all_interviewed)>0) {
    all_results <- resolve_offers_sequential(interviewed_data=all_interviewed, departments=departments, questions=questions, max_rounds=max_offer_rounds, seed=year, temperature=0.05, noise_sd=0.01, shortlist_enabled=shortlist_enabled, year=year)
    print_hiring_allocation(all_results, year, participation_rate)
  } else all_results <- tibble()
  for (j in 1:nrow(departments)) {
    ld_pw <- all_results %>% dplyr::filter(dept_id==departments$dept_id[j], strategy=="pairwise")
    if (nrow(ld_pw)>0) learning_data_pairwise[[j]] <- ld_pw %>% dplyr::select(year,dept_id,cand_id,s_j,v_i_bar,f_j,offered,accepted,dplyr::starts_with("q_")) %>% add_dept_info_to_learning_data(departments[j,])
  }
  diag_list <- diag_list[!sapply(diag_list, is.null)]
  list(results=all_results, learning_data_pairwise=learning_data_pairwise, rank_panel=rank_panel, diagnostics=list(applicant_level=bind_rows(diag_list)))
}


# =============================================================================
# resolve_offers_sequential - WITH SCRAMBLE (batch f_j for scramble round)
# =============================================================================
resolve_offers_sequential <- function(interviewed_data, departments, questions,
                                      all_candidates=NULL, max_rounds=3, seed=NULL,
                                      temperature=0.3, noise_sd=0.4,
                                      shortlist_enabled=TRUE, year=NA_integer_) {
  if (!is.null(seed)) set.seed(seed)
  if (nrow(interviewed_data)==0) return(interviewed_data %>% dplyr::mutate(offered=0L,accepted=0L,offer_round=NA_integer_))
  interviewed_data <- interviewed_data %>% dplyr::mutate(offered=0L, accepted=0L, offer_round=NA_integer_)
  candidates_with_accepted_offer <- integer(0)
  dept_positions_filled <- interviewed_data %>% dplyr::distinct(dept_id,h_j) %>% dplyr::mutate(filled=0L)
  dept_offered_candidates <- list()
  for (d in unique(interviewed_data$dept_id)) dept_offered_candidates[[as.character(d)]] <- integer(0)
  softmax_safe <- function(x, temp=1) {
    x <- as.numeric(x); if (length(x)==0L) return(numeric())
    if (all(!is.finite(x))) return(rep(1/length(x),length(x)))
    x[!is.finite(x)] <- min(x[is.finite(x)],na.rm=TRUE)-10
    if (!is.finite(temp)||temp<=0) temp <- 1
    z <- pmax(pmin((x-max(x,na.rm=TRUE))/temp, 700), -700)
    ez <- exp(z); s <- sum(ez); if (!is.finite(s)||s<=0) return(rep(1/length(x),length(x)))
    p <- ez/s; if (any(!is.finite(p))|any(p<0)) p <- rep(1/length(x),length(p)); p
  }
  cat("  Calculating candidate utilities with unified f_j...\n")
  if ("prestige_sensitivity" %in% names(interviewed_data))
    interviewed_data <- interviewed_data %>% dplyr::mutate(cand_util=candidate_utility(v_i_bar,s_j,f_j,cand_tier=cand_tier,prestige_sensitivity=prestige_sensitivity))
  else interviewed_data <- interviewed_data %>% dplyr::mutate(cand_util=candidate_utility(v_i_bar,s_j,f_j,cand_tier=cand_tier))
  n_regular_rounds <- max(1, max_rounds-1)
  for (round in 1:n_regular_rounds) {
    cat(sprintf("  Offer Round %d...\n", round)); offers_this_round <- tibble()
    for (d in unique(interviewed_data$dept_id)) {
      dept_data <- interviewed_data %>% dplyr::filter(dept_id==d); if (nrow(dept_data)==0) next
      h_j <- dept_data$h_j[1]; pf <- dept_positions_filled$filled[dept_positions_filled$dept_id==d]; pr <- h_j-pf; if (pr<=0) next
      ao <- dept_offered_candidates[[as.character(d)]]
      ec <- dept_data %>% dplyr::filter(!(cand_id%in%ao), !(cand_id%in%candidates_with_accepted_offer)) %>% dplyr::arrange(dplyr::desc(U_hat)); if (nrow(ec)==0) next
      noc <- ec %>% dplyr::slice_head(n=min(pr,nrow(ec)))
      offers_this_round <- dplyr::bind_rows(offers_this_round, noc %>% dplyr::select(dept_id,cand_id) %>% dplyr::mutate(round=round))
      dept_offered_candidates[[as.character(d)]] <- c(dept_offered_candidates[[as.character(d)]], noc$cand_id)
    }
    if (nrow(offers_this_round)==0) { cat(sprintf("  No new offers in round %d, moving to scramble.\n",round)); break }
    cat(sprintf("  %d new offers extended in round %d\n", nrow(offers_this_round), round))
    for (i in 1:nrow(offers_this_round)) {
      idx <- which(interviewed_data$dept_id==offers_this_round$dept_id[i] & interviewed_data$cand_id==offers_this_round$cand_id[i])
      if (length(idx)==1) { interviewed_data$offered[idx] <- 1L; interviewed_data$offer_round[idx] <- round }
    }
    for (cand in unique(offers_this_round$cand_id)) {
      if (cand %in% candidates_with_accepted_offer) next
      co <- interviewed_data %>% dplyr::filter(cand_id==cand, offered==1L, accepted==0L); if (nrow(co)==0) next
      if (nrow(co)==1) { chosen_dept <- co$dept_id[1]
      } else { wu <- co$cand_util; lu <- log(pmax(wu,1e-12))+rnorm(length(wu),0,noise_sd); probs <- softmax_safe(lu,temp=temperature); chosen_dept <- co$dept_id[sample(1:nrow(co),1,prob=probs)] }
      idx <- which(interviewed_data$dept_id==chosen_dept & interviewed_data$cand_id==cand)
      interviewed_data$accepted[idx] <- 1L; candidates_with_accepted_offer <- c(candidates_with_accepted_offer, cand)
      di <- which(dept_positions_filled$dept_id==chosen_dept); dept_positions_filled$filled[di] <- dept_positions_filled$filled[di]+1L
    }
    total_remaining <- sum(dept_positions_filled$h_j-dept_positions_filled$filled)
    cat(sprintf("  After round %d: %d positions still unfilled\n", round, total_remaining)); if (total_remaining==0) { cat("  All positions filled!\n"); break }
  }
  # === SCRAMBLE ROUND with batch f_j + SHORTLIST ENFORCEMENT ===
  total_remaining <- sum(dept_positions_filled$h_j-dept_positions_filled$filled)
  if (total_remaining>0 && !is.null(all_candidates)) {
    cat(sprintf("\n  === SCRAMBLE ROUND === (%d positions still unfilled)\n", total_remaining))
    unmatched_candidates <- all_candidates %>% dplyr::filter(!(cand_id %in% candidates_with_accepted_offer))
    if (nrow(unmatched_candidates)==0) { cat("  No unmatched candidates available for scramble.\n")
    } else {
      cat(sprintf("  %d unmatched candidates entering scramble\n", nrow(unmatched_candidates)))
      unfilled_depts <- dept_positions_filled %>% dplyr::filter(filled<h_j) %>% dplyr::mutate(positions_needed=h_j-filled) %>%
        dplyr::left_join(departments %>% dplyr::select(dept_id,s_j,prestige_tier), by="dept_id") %>% dplyr::arrange(dplyr::desc(s_j))
      scramble_round <- max_rounds; scramble_matches <- tibble()
      
      # Shortlist tier mapping (same as interview selection phase)
      allow_map <- list("Tier 1"=c("Tier 1"),"Tier 2"=c("Tier 1","Tier 2"),
                        "Tier 3"=c("Tier 1","Tier 2","Tier 3"),
                        "Tier 4"=c("Tier 1","Tier 2","Tier 3","Tier 4"))
      tti <- function(x) as.integer(factor(as.character(x),levels=c("Tier 1","Tier 2","Tier 3","Tier 4")))
      
      for (row_idx in 1:nrow(unfilled_depts)) {
        d <- unfilled_depts$dept_id[row_idx]; pn <- unfilled_depts$positions_needed[row_idx]
        dept_info <- departments %>% dplyr::filter(dept_id==d); if (nrow(dept_info)==0) next
        still_available <- unmatched_candidates %>% dplyr::filter(!(cand_id %in% candidates_with_accepted_offer)); if (nrow(still_available)==0) next
        
        # === SHORTLIST ENFORCEMENT IN SCRAMBLE ===
        if (shortlist_enabled && "quality_tier" %in% names(still_available)) {
          pt <- as.character(dept_info$prestige_tier[1] %||% "Tier 4")
          allowed_tiers <- allow_map[[pt]] %||% allow_map[["Tier 4"]]
          still_available <- still_available %>% dplyr::filter(as.character(quality_tier) %in% allowed_tiers)
          if (nrow(still_available) == 0) next
        }
        
        # *** BATCH f_j for scramble ***
        f_j_values <- calculate_f_j_batch(still_available, dept_info, questions)
        vbn <- if (diff(range(still_available$v_i_bar,na.rm=TRUE))>0) (still_available$v_i_bar-min(still_available$v_i_bar,na.rm=TRUE))/diff(range(still_available$v_i_bar,na.rm=TRUE)) else rep(0.5,nrow(still_available))
        scramble_priority <- 0.85*vbn+0.15*f_j_values
        top_indices <- order(scramble_priority, decreasing=TRUE)[1:min(pn,nrow(still_available))]
        for (ti in top_indices) {
          cand <- still_available$cand_id[ti]; if (cand %in% candidates_with_accepted_offer) next
          cr <- still_available[ti,]; sjv <- dept_info$s_j[1]; fjv <- f_j_values[ti]
          cuv <- if ("prestige_sensitivity" %in% names(cr)) candidate_utility(cr$v_i_bar,sjv,fjv,prestige_sensitivity=cr$prestige_sensitivity) else candidate_utility(cr$v_i_bar,sjv,fjv)
          ap <- if (fjv<0.15) 0.3 else 0.95
          if (runif(1)<ap) {
            # Build scramble match row WITH all metadata
            sm <- tibble(
              dept_id=d, cand_id=cand, s_j=sjv, v_i_bar=cr$v_i_bar, f_j=fjv,
              cand_tier=tti(cr$quality_tier), dept_tier=tti(dept_info$prestige_tier),
              offered=1L, accepted=1L, offer_round=scramble_round, cand_util=cuv,
              h_j=dept_info$h_j_this_year %||% 1L, strategy="pairwise", interviewed=0L,
              year=year,
              participates=if ("participates" %in% names(cr)) cr$participates else NA
            )
            # Carry over quality_tier if present
            if ("quality_tier" %in% names(cr)) sm$quality_tier <- as.character(cr$quality_tier)
            
            scramble_matches <- dplyr::bind_rows(scramble_matches, sm)
            candidates_with_accepted_offer <- c(candidates_with_accepted_offer, cand)
            di <- which(dept_positions_filled$dept_id==d); dept_positions_filled$filled[di] <- dept_positions_filled$filled[di]+1L
            if (dept_positions_filled$filled[di]>=dept_positions_filled$h_j[di]) break
          }
        }
      }
      if (nrow(scramble_matches)>0) {
        cat(sprintf("  Scramble produced %d new matches\n", nrow(scramble_matches)))
        for (col in setdiff(names(interviewed_data),names(scramble_matches))) scramble_matches[[col]] <- NA
        scramble_matches <- scramble_matches[, names(interviewed_data), drop=FALSE]
        interviewed_data <- dplyr::bind_rows(interviewed_data, scramble_matches)
      } else cat("  Scramble produced no new matches\n")
    }
  } else if (total_remaining>0) cat(sprintf("\n  WARNING: %d positions unfilled but no candidate pool for scramble.\n", total_remaining))
  fs <- dept_positions_filled %>% dplyr::mutate(unfilled=h_j-filled)
  cat("\n  Final hiring summary:\n"); cat(sprintf("    Total positions: %d\n",sum(fs$h_j))); cat(sprintf("    Positions filled: %d\n",sum(fs$filled))); cat(sprintf("    Positions unfilled: %d\n",sum(fs$unfilled)))
  interviewed_data
}

# =============================================================================
# BURN-IN PHASE
# =============================================================================
run_burn_in_phase <- function(departments, questions, n_candidates=500, burn_in_years=10,
                              yearly_candidate_cohorts=NULL, yearly_hiring_schedule=NULL, seed=123,
                              alpha=0.05, L_repeats=200, tuple_size=NULL, noise_method="bootstrap", noise_scale=0.15,
                              cand_tier_cutpoints=c(0.10,0.25,0.50), max_offer_rounds=5, print_diagnostics=TRUE) {
  set.seed(seed); torch::torch_manual_seed(seed); n_departments <- nrow(departments)
  cat("\n",strrep("=",70),"\nBURN-IN PHASE: Learning Priors\n",strrep("=",70),"\n")
  cat(sprintf("Running %d years of baseline simulation...\n", burn_in_years))
  if (is.null(yearly_hiring_schedule))
    yearly_hiring_schedule <- generate_yearly_hiring_schedule(n_departments, burn_in_years, departments, seed+500)
  if (is.null(yearly_candidate_cohorts)) {
    yearly_candidate_cohorts <- vector("list", burn_in_years)
    for (year in 1:burn_in_years)
      yearly_candidate_cohorts[[year]] <- generate_candidates_new(n_candidates, questions, seed=seed+year)
  }
  dept_strategies <- matrix(2L, nrow=n_departments, ncol=burn_in_years)
  mdl <- purrr::map(1:n_departments, ~initialize_department_model(questions, include_fit=FALSE, include_questions=FALSE))
  res_all <- list(); cand_roster_all <- list()
  for (year in 1:burn_in_years) {
    cat(sprintf("\n--- Burn-in Year %d/%d ---\n", year, burn_in_years))
    candidates <- yearly_candidate_cohorts[[year]] %>%
      mutate(quality_tier=assign_tiers_from_quantiles(v_i_bar, cutpoints=cand_tier_cutpoints))
    cand_roster_all[[year]] <- candidates %>%
      mutate(participates=FALSE) %>%
      transmute(year=!!year, cand_id, quality_tier, v_i_bar, participates)
    out <- simulate_market_year_adaptive_sequential(candidates, departments, questions, year,
                                                    dept_models_pairwise=mdl, participants_this_year=integer(0), dept_strategies=dept_strategies,
                                                    yearly_hiring_schedule=yearly_hiring_schedule, participation_rate=0, alpha=alpha,
                                                    L_repeats=L_repeats, tuple_size=tuple_size, noise_method=noise_method,
                                                    noise_scale=noise_scale, shortlist_enabled=TRUE, collect_ranking_panel=FALSE,
                                                    cand_tier_col="quality_tier", max_offer_rounds=max_offer_rounds, seed=year)
    res_all[[year]] <- out$results
    for (j in 1:n_departments) {
      if (!is.null(out$learning_data_pairwise[[j]]) && nrow(out$learning_data_pairwise[[j]])>0) {
        ld <- out$learning_data_pairwise[[j]]; ld$f_j <- ld$v_i_bar
        mdl[[j]]$historical_data <- bind_rows(mdl[[j]]$historical_data, ld)
      }
    }
  }
  cat("\n",strrep("=",70),"\nTraining models on burn-in data...\n",strrep("=",70),"\n")
  for (j in 1:n_departments) {
    n_obs <- nrow(mdl[[j]]$historical_data)
    if (n_obs>=10) {
      mdl[[j]] <- train_department_model(mdl[[j]], mdl[[j]]$historical_data,
                                         n_epochs=100, include_fit=FALSE, include_questions=FALSE, seed=j*1000)
      cat(sprintf("  Dept %d: Trained on %d observations\n", j, n_obs))
    } else cat(sprintf("  Dept %d: Insufficient data (%d obs)\n", j, n_obs))
  }
  total_hires <- sum(sapply(res_all, function(r) sum(r$accepted==1, na.rm=TRUE)))
  total_offers <- sum(sapply(res_all, function(r) sum(r$offered==1, na.rm=TRUE)))
  cat(sprintf("\nBurn-in complete. Offers: %d | Hires: %d | Yield: %.1f%%\n",
              total_offers, total_hires, 100*total_hires/max(total_offers,1)))
  list(trained_models=mdl, burn_in_results=bind_rows(res_all),
       yearly_candidate_cohorts=yearly_candidate_cohorts,
       yearly_hiring_schedule=yearly_hiring_schedule,
       cand_roster=bind_rows(cand_roster_all))
}


# =============================================================================
# predict_acceptance_probability_with_learned_prior
# =============================================================================
predict_acceptance_probability_with_learned_prior <- function(dept_model, applicant_data,
                                                              learned_prior_model=NULL, n_bootstrap=200, include_fit=dept_model$include_fit,
                                                              include_questions=dept_model$include_questions, seed=NULL, participation_rate=NULL) {
  if (!is.null(seed)) { set.seed(seed); torch::torch_manual_seed(seed) }
  is_baseline <- is.null(participation_rate) || participation_rate==0 || !include_fit
  # Get learned prior
  if (!is.null(learned_prior_model) && learned_prior_model$is_trained) {
    prior_data <- applicant_data %>% mutate(f_j=v_i_bar)
    nn_data_prior <- tryCatch(
      prepare_nn_data(prior_data, learned_prior_model$questions, include_fit=FALSE, include_questions=FALSE),
      error=function(e) NULL)
    if (!is.null(nn_data_prior)) {
      learned_prior_model$model$eval()
      with_no_grad({
        lo <- learned_prior_model$model(nn_data_prior$x_cont, nn_data_prior$x_cat)
        pi_prior <- as.numeric(torch_sigmoid(lo)$squeeze()$to(device="cpu"))
      })
      pi_prior <- pmin(pmax(pi_prior, 0.05), 0.95)
    } else {
      tg <- pmax(0, applicant_data$dept_tier - applicant_data$cand_tier)
      pi_prior <- case_when(tg==0~0.55, tg==1~0.45, tg==2~0.35, TRUE~0.25)
    }
  } else {
    tg <- pmax(0, applicant_data$dept_tier - applicant_data$cand_tier)
    pi_prior <- case_when(tg==0~0.55, tg==1~0.45, tg==2~0.35, TRUE~0.25)
  }
  if (is_baseline) {
    res <- applicant_data %>% mutate(
      pi_pred=pmax(0.10, pmin(0.90, pi_prior + rnorm(n(), 0, 0.02))), pi_var=0)
    attr(res, "pi_draws") <- matrix(res$pi_pred, nrow=1); return(res)
  }
  fit_adj <- case_when(
    applicant_data$f_j>=0.80~0.30, applicant_data$f_j>=0.65~0.20,
    applicant_data$f_j>=0.50~0.10, applicant_data$f_j>=0.35~0.00, TRUE~-0.05)
  tg <- pmax(0, applicant_data$dept_tier - applicant_data$cand_tier)
  reach_bonus <- case_when(
    tg>=2 & applicant_data$f_j>=0.70~0.15,
    tg>=3 & applicant_data$f_j>=0.75~0.20, TRUE~0.0)
  pi_adjusted <- pmax(0.10, pmin(0.90, pi_prior + fit_adj + reach_bonus))
  if (dept_model$is_trained && nrow(dept_model$historical_data)>=10) {
    nn_data <- tryCatch(
      prepare_nn_data(applicant_data, dept_model$questions, include_fit=include_fit, include_questions=include_questions),
      error=function(e) NULL)
    if (!is.null(nn_data)) {
      dept_model$model$eval()
      with_no_grad({
        lo <- dept_model$model(nn_data$x_cont, nn_data$x_cat)
        pi_model <- as.numeric(torch_sigmoid(lo)$squeeze()$to(device="cpu"))
      })
      pi_model <- pmin(pmax(pi_model, 1e-5), 1-1e-5)
      mw <- pmin(1, nrow(dept_model$historical_data)/100)
      pi_blended <- mw * pi_model + (1-mw) * pi_adjusted
      res <- applicant_data %>% mutate(pi_pred=pi_blended, pi_var=0)
      attr(res, "pi_draws") <- matrix(pi_blended, nrow=1); return(res)
    }
  }
  res <- applicant_data %>% mutate(
    pi_pred=pmax(0.10, pmin(0.90, pi_adjusted + rnorm(n(), 0, 0.02))), pi_var=0)
  attr(res, "pi_draws") <- matrix(res$pi_pred, nrow=1); res
}


# =============================================================================
# simulate_market_year_with_learned_prior
# =============================================================================
simulate_market_year_with_learned_prior <- function(candidates, departments, questions, year,
                                                    dept_models_pairwise, learned_prior_models, participants_this_year, dept_strategies,
                                                    yearly_hiring_schedule, participation_rate=0, alpha=0.05, L_repeats=200, tuple_size=NULL,
                                                    noise_method="bootstrap", noise_scale=0.15, shortlist_enabled=TRUE, collect_ranking_panel=TRUE,
                                                    cand_tier_col="quality_tier", max_offer_rounds=5, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  candidates <- candidates %>% mutate(participates=cand_id %in% participants_this_year)
  tier_to_int <- function(x) as.integer(factor(as.character(x), levels=c("Tier 1","Tier 2","Tier 3","Tier 4")))
  applications <- generate_applications(candidates, departments, questions)
  all_interviewed <- tibble(); learning_data_pairwise <- vector("list", nrow(departments))
  rank_panel <- tibble(); diag_list <- vector("list", nrow(departments))
  cat(sprintf("\n=== YEAR %d: Interview Selection Phase ===\n", year))
  for (j in 1:nrow(departments)) {
    dept <- departments[j,]; h_j <- yearly_hiring_schedule[j,year]; k_j <- 5L*h_j
    if (h_j==0L) {
      da <- applications %>% filter(dept_id==dept$dept_id)
      if (nrow(da)>0) {
        aa <- da %>% left_join(candidates,by="cand_id") %>% mutate(s_j=dept$s_j)
        diag_list[[j]] <- aa %>% mutate(year=year,dept_id=dept$dept_id,strategy="pairwise",
                                        pi_pred=NA_real_,U_true=NA_real_,U_hat=NA_real_,r_true=NA_real_,r_hat=NA_real_,
                                        interviewed=0L,considered=0L,h_j=0L,k_j=0L)
      }
      next
    }
    da <- applications %>% filter(dept_id==dept$dept_id); if (nrow(da)==0) next
    dept_strategy <- if (participation_rate==0) "S2" else ifelse(dept_strategies[j,year]==1,"S1","S2")
    apps_all <- da %>% left_join(candidates, by="cand_id") %>% mutate(s_j=dept$s_j)
    if (!cand_tier_col %in% names(apps_all))
      apps_all[[cand_tier_col]] <- factor("Tier 4", levels=c("Tier 1","Tier 2","Tier 3","Tier 4"))
    else
      apps_all[[cand_tier_col]] <- factor(as.character(apps_all[[cand_tier_col]]),
                                          levels=c("Tier 1","Tier 2","Tier 3","Tier 4"))
    apps_all <- apps_all %>% mutate(
      dept_tier=tier_to_int(dept$prestige_tier%||%"Tier 4"),
      cand_tier=tier_to_int(.data[[cand_tier_col]]))
    apps_all$f_j <- calculate_f_j_batch(apps_all, dept, questions)
    if (shortlist_enabled) {
      allow_map <- list("Tier 1"=c("Tier 1"),"Tier 2"=c("Tier 1","Tier 2"),
                        "Tier 3"=c("Tier 1","Tier 2","Tier 3"),"Tier 4"=c("Tier 1","Tier 2","Tier 3","Tier 4"))
      pt <- as.character(dept$prestige_tier%||%"Tier 4")
      allowed <- allow_map[[pt]]%||%allow_map[["Tier 4"]]
      considered_mask <- as.character(apps_all[[cand_tier_col]]) %in% allowed
    } else considered_mask <- rep(TRUE, nrow(apps_all))
    applicant_data <- apps_all[considered_mask,,drop=FALSE]
    if (nrow(applicant_data)==0) {
      diag_list[[j]] <- apps_all %>% mutate(year=year,dept_id=dept$dept_id,strategy="pairwise",
                                            pi_pred=NA_real_,U_true=NA_real_,U_hat=NA_real_,r_true=NA_real_,r_hat=NA_real_,
                                            interviewed=0L,considered=0L,h_j=h_j,k_j=k_j); next
    }
    applicant_data <- applicant_data %>% mutate(row_id=row_number())
    n_part <- sum(applicant_data$participates)
    if (participation_rate==0) { use_gmi <- FALSE; mfp <- NA_real_
    } else if (n_part>0) { use_gmi <- TRUE; mfp <- mean(applicant_data$f_j[applicant_data$participates],na.rm=TRUE)
    } else { use_gmi <- FALSE; mfp <- NA_real_ }
    applicant_data_pw <- applicant_data %>% mutate(
      f_j_used=case_when(
        !participates & dept_strategy=="S1" ~ 0.0,
        !participates & dept_strategy=="S2" & use_gmi ~ mfp,
        !participates & dept_strategy=="S2" & !use_gmi ~ v_i_bar,
        TRUE ~ f_j))
    actual_f_j <- applicant_data_pw$f_j
    ad_tmp <- applicant_data_pw; ad_tmp$f_j <- applicant_data_pw$f_j_used
    for (q in questions$numerical) ad_tmp[[q]] <- dept[[q]]
    for (qn in names(questions$categorical)) ad_tmp[[qn]] <- as.character(dept[[qn]])
    applicant_data_pw <- predict_acceptance_probability_with_learned_prior(
      dept_models_pairwise[[j]], ad_tmp, learned_prior_model=learned_prior_models[[j]],
      n_bootstrap=L_repeats, seed=j*1000+year, participation_rate=participation_rate)
    applicant_data_pw$f_j <- actual_f_j
    U_det <- true_utility(applicant_data_pw$s_j, applicant_data_pw$v_i_bar,
                          applicant_data_pw$f_j_used, applicant_data_pw$dept_tier, applicant_data_pw$cand_tier)
    applicant_data_pw <- applicant_data_pw %>% mutate(
      f_j_used=applicant_data_pw$f_j_used,
      U_det=U_det,
      U_hat=U_det*pmin(pmax(pi_pred,1e-5),1-1e-5))
    U_true <- true_utility(applicant_data_pw$s_j, applicant_data_pw$v_i_bar,
                           applicant_data_pw$f_j, applicant_data_pw$dept_tier, applicant_data_pw$cand_tier)
    applicant_data_pw$U_true <- U_true
    applicant_data_pw$r_true <- rank(-U_true, ties.method="average")
    applicant_data_pw$r_point <- rank(-applicant_data_pw$U_hat, ties.method="average")
    if (collect_ranking_panel) {
      rpair <- compute_pairwise_lower_ranks(applicant_data_pw, L_repeats, tuple_size,
                                            noise_method, noise_scale, alpha)
      applicant_data_pw <- applicant_data_pw %>%
        left_join(rpair, by="cand_id") %>%
        mutate(r_pair_lb=rank_lower) %>% dplyr::select(-rank_lower)
    }
    interviewed_ids <- select_interviews_sure_screening(applicant_data_pw,
                                                        k_j=k_j, h_j=h_j, alpha=alpha, L_repeats=L_repeats, tuple_size=tuple_size,
                                                        noise_method=noise_method, noise_scale=noise_scale)
    applicant_data_pw$interviewed_flag <- as.integer(applicant_data_pw$cand_id %in% interviewed_ids)
    if (collect_ranking_panel) {
      rank_panel <- bind_rows(rank_panel, applicant_data_pw %>% transmute(
        year, dept_id=dept$dept_id, strategy="pairwise", cand_id, s_j, v_i_bar, f_j,
        participates, U_true, U_hat, r_true, r_point,
        r_pair_lb=if_else(is.finite(r_pair_lb), r_pair_lb, NA_real_),
        interviewed=interviewed_flag, k_j=k_j, h_j=h_j))
    }
    idpw <- applicant_data_pw %>% filter(cand_id %in% interviewed_ids) %>%
      mutate(interviewed=1L, year=year, dept_id=dept$dept_id, strategy="pairwise", h_j=h_j, k_j=k_j)
    if (nrow(idpw)>0) all_interviewed <- bind_rows(all_interviewed, idpw)
    diag_list[[j]] <- apps_all %>% mutate(
      considered=as.integer(cand_id %in% applicant_data$cand_id),
      interviewed=as.integer(cand_id %in% interviewed_ids), h_j=h_j, k_j=k_j) %>%
      left_join(applicant_data_pw %>% dplyr::select(cand_id,pi_pred,U_true,U_hat,r_true), by="cand_id") %>%
      transmute(year=year,dept_id=dept$dept_id,strategy="pairwise",cand_id,s_j,v_i_bar,f_j,
                pi_pred,U_true,U_hat,r_true,interviewed,considered,participates,h_j,k_j)
  }
  cat(sprintf("\n=== YEAR %d: Sequential Offer Resolution ===\n", year))
  if (nrow(all_interviewed)>0) {
    all_results <- resolve_offers_sequential(interviewed_data=all_interviewed,
                                             departments=departments, questions=questions, all_candidates=candidates,
                                             max_rounds=max_offer_rounds, seed=year, temperature=0.05, noise_sd=0.01,
                                             shortlist_enabled=shortlist_enabled, year=year)
    print_hiring_allocation(all_results, year, participation_rate)
  } else all_results <- tibble()
  for (j in 1:nrow(departments)) {
    ld <- all_results %>% filter(dept_id==departments$dept_id[j], strategy=="pairwise")
    if (nrow(ld)>0)
      learning_data_pairwise[[j]] <- ld %>%
        dplyr::select(year,dept_id,cand_id,s_j,v_i_bar,f_j,offered,accepted,starts_with("q_")) %>%
        add_dept_info_to_learning_data(departments[j,])
  }
  diag_list <- diag_list[!sapply(diag_list, is.null)]
  list(results=all_results, learning_data_pairwise=learning_data_pairwise,
       rank_panel=rank_panel, diagnostics=list(applicant_level=bind_rows(diag_list)))
}

# =============================================================================
# run_job_market_sim_with_learned_prior
# =============================================================================
run_job_market_sim_with_learned_prior <- function(departments, questions, n_candidates=500,
                                                  burn_in_years=10, sim_years=10, participation_rate, yearly_candidate_cohorts_burn_in=NULL,
                                                  yearly_candidate_cohorts_sim=NULL, participation_sets=NULL,
                                                  yearly_hiring_schedule_burn_in=NULL, yearly_hiring_schedule_sim=NULL,
                                                  learned_prior_models=NULL, seed=123, alpha=0.05, L_repeats=200, tuple_size=NULL,
                                                  noise_method="bootstrap", noise_scale=0.15, cand_tier_cutpoints=c(0.10,0.25,0.50),
                                                  max_offer_rounds=5, print_diagnostics=TRUE) {
  
  set.seed(seed); torch::torch_manual_seed(seed); n_departments <- nrow(departments)
  is_baseline <- (participation_rate==0)
  if (is.null(yearly_hiring_schedule_sim))
    yearly_hiring_schedule_sim <- generate_yearly_hiring_schedule(n_departments, sim_years, departments, seed+600)
  dept_strategies <- generate_department_strategies_adaptive(n_departments, sim_years, participation_rate, seed+1000)
  if (is.null(yearly_candidate_cohorts_sim)) {
    yearly_candidate_cohorts_sim <- vector("list", sim_years)
    for (year in 1:sim_years)
      yearly_candidate_cohorts_sim[[year]] <- generate_candidates_new(n_candidates, questions, seed=seed+burn_in_years+year)
  }
  cand_roster_all <- list(); rank_all <- list(); diag_all <- list()
  mdl <- purrr::map(1:n_departments, ~initialize_department_model(questions, include_fit=!is_baseline, include_questions=!is_baseline))
  res_all <- list(); dept_tier_info <- departments %>% dplyr::select(dept_id, tier)
  
  for (year in 1:sim_years) {
    cat("\n",strrep("=",70),"\n")
    cat(sprintf("SIMULATING YEAR %d with participation rate %.0f%% [%s]\n",
                year, participation_rate*100, if(is_baseline) "BASELINE" else "QUESTIONNAIRE"))
    cat(strrep("=",70),"\n")
    candidates <- yearly_candidate_cohorts_sim[[year]]
    rate_key <- as.character(participation_rate)
    participants <- if (!is.null(participation_sets) && rate_key %in% names(participation_sets))
      participation_sets[[rate_key]] else integer(0)
    candidates <- candidates %>% mutate(
      quality_tier=assign_tiers_from_quantiles(v_i_bar, cutpoints=cand_tier_cutpoints))
    cand_roster_all[[year]] <- candidates %>%
      mutate(participates=cand_id %in% participants) %>%
      transmute(year=!!year, cand_id, quality_tier, v_i_bar, participates)
    
    out <- simulate_market_year_with_learned_prior(candidates, departments, questions, year,
                                                   dept_models_pairwise=mdl, learned_prior_models=learned_prior_models,
                                                   participants_this_year=participants, dept_strategies=dept_strategies,
                                                   yearly_hiring_schedule=yearly_hiring_schedule_sim,
                                                   participation_rate=participation_rate, alpha=alpha, L_repeats=L_repeats,
                                                   tuple_size=tuple_size, noise_method=noise_method, noise_scale=noise_scale,
                                                   shortlist_enabled=TRUE, collect_ranking_panel=TRUE, cand_tier_col="quality_tier",
                                                   max_offer_rounds=max_offer_rounds, seed=year)
    
    diag_all[[year]] <- out$diagnostics$applicant_level
    res_all[[year]] <- out$results
    rank_all[[year]] <- out$rank_panel
    
    # Print cumulative diagnostics
    if (print_diagnostics && nrow(out$results) > 0) {
      cumulative_results <- bind_rows(res_all[1:year])
      if (nrow(cumulative_results) > 0) {
        cumulative_with_tiers <- cumulative_results %>%
          left_join(dept_tier_info, by = "dept_id")
        cumulative_quota <- tibble(dept_id = 1:n_departments) %>%
          left_join(dept_tier_info, by = "dept_id") %>%
          crossing(yr = 1:year) %>%
          mutate(h_j = purrr::map2_int(dept_id, yr, ~yearly_hiring_schedule_sim[.x, .y])) %>%
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
        cat(strrep("\u2500", 70), "\n")
        cat(sprintf("  CUMULATIVE RESULTS (Years 1-%d) | rho = %.0f%%\n", year, participation_rate * 100))
        cat(strrep("\u2500", 70), "\n")
        cat(sprintf("  Overall: Offers=%d | Accepts=%d | Yield=%.1f%% | Mean U=%.4f | Mean f=%.4f\n",
                    overall_offers, overall_accepts, overall_yield * 100,
                    overall_mean_U, overall_mean_f))
        cat(strrep("\u2500", 70), "\n")
        cat(sprintf("  %-8s %7s %7s %7s %7s %9s %9s %9s\n",
                    "Tier", "Quota", "Offers", "Accepts", "Yield", "FillRate", "Mean_U", "Mean_f"))
        cat(strrep("\u2500", 70), "\n")
        for (i in 1:nrow(tier_metrics)) {
          row <- tier_metrics[i, ]
          cat(sprintf("  %-8s %7d %7d %7d %6.1f%% %8.1f%% %9.4f %9.4f\n",
                      as.character(row$tier),
                      row$total_quota,
                      row$n_offers,
                      row$n_accepts,
                      row$yield * 100,
                      row$fill_rate * 100,
                      row$mean_U_true,
                      row$mean_f_j))
        }
        cat(strrep("\u2500", 70), "\n\n")
      }
    }
    
    # Update models with new data
    for (j in 1:n_departments) {
      if (!is.null(out$learning_data_pairwise[[j]]) && nrow(out$learning_data_pairwise[[j]])>0) {
        mdl[[j]]$historical_data <- bind_rows(mdl[[j]]$historical_data, out$learning_data_pairwise[[j]])
        if (nrow(mdl[[j]]$historical_data)>=10 && year%%2==0)
          mdl[[j]] <- tryCatch(
            train_department_model(mdl[[j]], mdl[[j]]$historical_data, n_epochs=60,
                                   include_fit=!is_baseline, include_questions=!is_baseline, seed=j*1000+year),
            error=function(e) mdl[[j]])
      }
    }
  }
  
  list(
    results = bind_rows(res_all),
    rank_panel = bind_rows(rank_all),
    cand_roster = bind_rows(cand_roster_all),
    diagnostics = bind_rows(diag_all),
    dept_models = mdl,
    yearly_results = res_all
  )
}


# =============================================================================
# run_complete_simulation_with_burn_in (MAIN ENTRY POINT)
# =============================================================================
run_complete_simulation_with_burn_in <- function(sampled_depts, questions, n_candidates=300,
                                                 burn_in_years=10, sim_years=20, participation_rates=c(0, 0.20),
                                                 seed=42, alpha=0.05, L_repeats=200, tuple_size=NULL,
                                                 noise_method="bootstrap", noise_scale=0.15,
                                                 cand_tier_cutpoints=c(0.10,0.25,0.50), max_offer_rounds=5) {
  
  set.seed(seed); torch::torch_manual_seed(seed)
  departments <- prepare_departments(sampled_depts, questions, seed=seed)
  n_departments <- nrow(departments)
  
  # Generate shared candidate cohorts
  yearly_candidate_cohorts_burn_in <- vector("list", burn_in_years)
  for (year in 1:burn_in_years)
    yearly_candidate_cohorts_burn_in[[year]] <- generate_candidates_new(n_candidates, questions, seed=seed+year)
  yearly_candidate_cohorts_sim <- vector("list", sim_years)
  for (year in 1:sim_years)
    yearly_candidate_cohorts_sim[[year]] <- generate_candidates_new(n_candidates, questions, seed=seed+burn_in_years+year)
  
  yearly_hiring_schedule_burn_in <- generate_yearly_hiring_schedule(n_departments, burn_in_years, departments, seed+500)
  yearly_hiring_schedule_sim <- generate_yearly_hiring_schedule(n_departments, sim_years, departments, seed+600)
  
  participation_sets <- generate_nested_participation_assignments(n_candidates, participation_rates[participation_rates>0], seed=seed+100)
  
  # === BURN-IN ===
  burn_in <- run_burn_in_phase(departments, questions, n_candidates=n_candidates,
                               burn_in_years=burn_in_years, yearly_candidate_cohorts=yearly_candidate_cohorts_burn_in,
                               yearly_hiring_schedule=yearly_hiring_schedule_burn_in, seed=seed, alpha=alpha,
                               L_repeats=L_repeats, tuple_size=tuple_size, noise_method=noise_method,
                               noise_scale=noise_scale, cand_tier_cutpoints=cand_tier_cutpoints,
                               max_offer_rounds=max_offer_rounds)
  
  learned_prior_models <- burn_in$trained_models
  
  # === SIMULATION RUNS ===
  sim_results <- list()
  for (rate in participation_rates) {
    cat("\n\n", strrep("#",80), "\n")
    cat(sprintf("### SIMULATION RUN: participation_rate = %.0f%% ###\n", rate*100))
    cat(strrep("#",80), "\n\n")
    
    sim_results[[as.character(rate)]] <- run_job_market_sim_with_learned_prior(
      departments=departments, questions=questions, n_candidates=n_candidates,
      burn_in_years=burn_in_years, sim_years=sim_years, participation_rate=rate,
      yearly_candidate_cohorts_sim=yearly_candidate_cohorts_sim,
      participation_sets=participation_sets,
      yearly_hiring_schedule_sim=yearly_hiring_schedule_sim,
      learned_prior_models=learned_prior_models, seed=seed, alpha=alpha,
      L_repeats=L_repeats, tuple_size=tuple_size, noise_method=noise_method,
      noise_scale=noise_scale, cand_tier_cutpoints=cand_tier_cutpoints,
      max_offer_rounds=max_offer_rounds)
  }
  
  list(
    burn_in = burn_in,
    sim_results = sim_results,
    departments = departments,
    questions = questions,
    participation_sets = participation_sets,
    config = list(n_candidates=n_candidates, burn_in_years=burn_in_years, sim_years=sim_years,
                  participation_rates=participation_rates, seed=seed, alpha=alpha, L_repeats=L_repeats)
  )
}


# =============================================================================
# EXECUTION BLOCK
# =============================================================================
# Load department data and prepare it
setwd("/Users/alikmofrad/UCLA PhD/SCALE Group/Research/Job Market /Codes/Academic Job Market/department_generator")
departments_final <- read_csv("departments_dataset.csv")

departments <- departments_final %>%
  mutate(dept_id = row_number()) %>%
  prepare_departments(questions, seed = 42)



start.time <- Sys.time()

full_results <- run_complete_simulation_with_burn_in(
  sampled_depts = departments,   # Your department data
  questions = questions,
  n_candidates = 300,
  burn_in_years = 10,
  sim_years = 10,
  participation_rates = c(0, 0.05, 0.2, 0.5, 0.9, 1.00),
  seed = 42,
  alpha = 0.05,
  L_repeats = 5,
  max_offer_rounds = 2
)

end.time <- Sys.time()
(time.taken <- round(end.time - start.time,2))


# setwd("/Users/alikmofrad/UCLA PhD/SCALE Group/Research/Job Market /Codes/Academic Job Market")
# saveRDS(full_results, "full_results_opt.rds")



#######################################################
################## ANALYZING RESULTS ##################
#######################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(viridis)
})

# =============================================================================
# THEME + PALETTE
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

tier_colors <- c(
  "Tier 1" = "black",
  "Tier 2" = "black",
  "Tier 3" = "black",
  "Tier 4" = "black"
)

tier_linetypes <- c(
  "Tier 1" = "solid",
  "Tier 2" = "dashed",
  "Tier 3" = "dotted",
  "Tier 4" = "dotdash"
)

tier_shapes <- c("Tier 1" = 16, "Tier 2" = 17, "Tier 3" = 15, "Tier 4" = 18)


# =============================================================================
# ADAPTER: Convert full_results -> all_sim_results format for viz functions
# =============================================================================
convert_to_viz_format <- function(full_results) {
  departments <- full_results$departments
  config <- full_results$config
  n_departments <- nrow(departments)
  yearly_hiring_schedule_sim <- generate_yearly_hiring_schedule(
    n_departments, config$sim_years, departments, seed = config$seed + 600)
  
  all_sim_results <- list()
  for (rate_str in names(full_results$sim_results)) {
    sim <- full_results$sim_results[[rate_str]]
    rate_num <- as.numeric(rate_str)
    key <- if (rate_num == 0) "baseline" else rate_str
    
    all_sim_results[[key]] <- list(
      results              = sim$results,
      cand_roster          = sim$cand_roster,
      rank_panel           = sim$rank_panel,
      diagnostics          = sim$diagnostics,
      departments          = departments,
      yearly_hiring_schedule = yearly_hiring_schedule_sim
    )
  }
  all_sim_results
}


# =============================================================================
# HELPERS
# =============================================================================
tier_int_to_str <- function(x) {
  dplyr::case_when(
    x == 1 ~ "Tier 1", x == 2 ~ "Tier 2",
    x == 3 ~ "Tier 3", x == 4 ~ "Tier 4",
    TRUE ~ NA_character_
  )
}

# ---------------------------------------------------------------------------
# is_scramble_hire: The definitive test for scramble-round rows.
#
# In resolve_offers_sequential(), scramble matches are created with
#   interviewed = 0L   (they skip the interview phase)
#   offer_round = max_rounds   (the last round is the scramble round)
#   accepted = 1L, offered = 1L
#   year = <valid integer>   (NOT NA — that was the old bug)
#
# We use interviewed == 0 as the primary marker because it is always set
# and does not depend on knowing max_rounds at analysis time.
# ---------------------------------------------------------------------------
is_scramble_hire <- function(df) {
  # Must be an accepted hire that was never interviewed
  (df$accepted == 1L) & (df$interviewed == 0L)
}


# =============================================================================
# 1. INTERVIEW HEATMAP
# =============================================================================
make_fig_dept_interview_heatmap <- function(all_sim_results, year_filter = c(1, 10),
                                            include_scramble = FALSE) {
  
  # Interview heatmap: we look at interviewed == 1 rows.
  # Scramble hires have interviewed == 0, so they are never "interviews".
  # When include_scramble = TRUE, we add scramble hires as additional
  # interview-equivalent rows so they appear in the heatmap counts.
  get_interviews <- function(results_df) {
    base <- results_df %>%
      dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                    strategy == "pairwise", interviewed == 1)
    if (include_scramble) {
      scramble <- results_df %>%
        dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                      strategy == "pairwise", interviewed == 0, accepted == 1)
      if (nrow(scramble) > 0) {
        cat(sprintf("  [interview heatmap] Including %d scramble hires\n", nrow(scramble)))
        # Add interviewed=1 temporarily so downstream logic treats them the same
        scramble$interviewed <- 1L
        base <- bind_rows(base, scramble)
      }
    }
    base
  }
  
  baseline_results <- get_interviews(all_sim_results[["baseline"]]$results)
  full_results     <- get_interviews(all_sim_results[["1"]]$results)
  
  departments <- all_sim_results[["baseline"]]$departments %>%
    dplyr::select(dept_id, prestige_tier)
  yearly_hiring_schedule <- all_sim_results[["baseline"]]$yearly_hiring_schedule
  
  baseline_roster <- all_sim_results[["baseline"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  full_roster <- all_sim_results[["1"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  cand_totals <- baseline_roster %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  years_in_filter <- year_filter[1]:year_filter[2]
  interview_budget_totals <- tibble(dept_id = 1:nrow(departments)) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y]),
           k_j = 5L * h_j) %>%
    group_by(prestige_tier) %>%
    summarise(budget = sum(k_j), n_interviewing_events = sum(k_j > 0), .groups = "drop") %>%
    mutate(prestige_tier = factor(prestige_tier,
                                  levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  combined_interviews <- bind_rows(
    baseline_results %>%
      left_join(departments, by = "dept_id") %>%
      left_join(baseline_roster %>% dplyr::select(year, cand_id, quality_tier),
                by = c("year","cand_id")) %>%
      mutate(scenario = "Baseline"),
    full_results %>%
      left_join(departments, by = "dept_id") %>%
      left_join(full_roster %>% dplyr::select(year, cand_id, quality_tier),
                by = c("year","cand_id")) %>%
      mutate(scenario = "Questionnaire")
  )
  if ("quality_tier.y" %in% names(combined_interviews))
    combined_interviews$quality_tier <- combined_interviews$quality_tier.y
  if ("prestige_tier.y" %in% names(combined_interviews))
    combined_interviews$prestige_tier <- combined_interviews$prestige_tier.y
  
  heatmap_data <- combined_interviews %>%
    group_by(scenario, prestige_tier, quality_tier) %>%
    summarise(n_interviews = n(), mean_utility = mean(U_true, na.rm = TRUE), .groups = "drop") %>%
    mutate(prestige_tier = factor(prestige_tier,
                                  levels = c("Tier 4","Tier 3","Tier 2","Tier 1")),
           quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")),
           scenario = factor(scenario, levels = c("Baseline","Questionnaire"))) %>%
    complete(scenario, prestige_tier, quality_tier,
             fill = list(n_interviews = 0, mean_utility = NA))
  
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
    geom_text(aes(label = ifelse(n_interviews > 0, n_interviews, "")),
              fontface = "bold", size = 5, vjust = 0.3, color = "white") +
    geom_text(aes(label = ifelse(is.finite(mean_utility),
                                 sprintf("bar(U)[ji]==%.3f", mean_utility), "")),
              size = 3.5, vjust = 1.8, color = "white", parse = TRUE) +
    facet_wrap(~ scenario, ncol = 2) +
    scale_fill_viridis_c(name = expression(atop(bar(U)[ji], "Mean Utility")),
                         limits = c(0,1), na.value = "grey70", option = "viridis", breaks = seq(0,1,0.25)) +
    scale_x_discrete(labels = x_labels) +
    scale_y_discrete(labels = y_labels) +
    labs(x = "Candidate Quality Tier", y = "Department Prestige Tier") +
    theme_jasa(base_size = 14) +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "gray95", color = NA),
          legend.position = "right",
          legend.title = element_text(size = 12, face = "bold"),
          panel.spacing = unit(1.5, "lines"))
  
  interview_fill_summary <- heatmap_data %>%
    mutate(tier_for_join = as.character(prestige_tier)) %>%
    group_by(scenario, tier_for_join) %>%
    summarise(total_interviews = sum(n_interviews), .groups = "drop") %>%
    left_join(interview_budget_totals %>% mutate(tier_for_join = as.character(prestige_tier)),
              by = "tier_for_join") %>%
    mutate(utilization_rate = ifelse(budget > 0, total_interviews / budget, NA_real_)) %>%
    dplyr::select(scenario, prestige_tier, total_interviews, budget, utilization_rate)
  
  list(plot = p, data = heatmap_data, candidate_totals = cand_totals,
       interview_budgets = interview_budget_totals,
       utilization_rates = interview_fill_summary)
}


# =============================================================================
# 2. HIRING HEATMAP
# =============================================================================
make_fig_dept_hiring_heatmap <- function(all_sim_results, year_filter = c(1, 10),
                                         include_scramble = FALSE) {
  
  get_hires <- function(results_df) {
    base <- results_df %>%
      dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                    strategy == "pairwise", accepted == 1)
    if (!include_scramble) {
      # Exclude scramble hires (interviewed == 0)
      base <- base %>% dplyr::filter(interviewed == 1 | is.na(interviewed))
    }
    base
  }
  
  baseline_results <- get_hires(all_sim_results[["baseline"]]$results)
  full_results     <- get_hires(all_sim_results[["1"]]$results)
  
  n_bl_scramble <- sum(all_sim_results[["baseline"]]$results$accepted == 1 &
                         all_sim_results[["baseline"]]$results$interviewed == 0 &
                         all_sim_results[["baseline"]]$results$year >= year_filter[1] &
                         all_sim_results[["baseline"]]$results$year <= year_filter[2], na.rm = TRUE)
  n_fu_scramble <- sum(all_sim_results[["1"]]$results$accepted == 1 &
                         all_sim_results[["1"]]$results$interviewed == 0 &
                         all_sim_results[["1"]]$results$year >= year_filter[1] &
                         all_sim_results[["1"]]$results$year <= year_filter[2], na.rm = TRUE)
  if (include_scramble) {
    cat(sprintf("  [hiring heatmap] Including scramble: %d baseline + %d questionnaire\n",
                n_bl_scramble, n_fu_scramble))
  } else {
    cat(sprintf("  [hiring heatmap] Excluding scramble: %d baseline + %d questionnaire removed\n",
                n_bl_scramble, n_fu_scramble))
  }
  
  departments <- all_sim_results[["baseline"]]$departments %>%
    dplyr::select(dept_id, prestige_tier)
  yearly_hiring_schedule <- all_sim_results[["baseline"]]$yearly_hiring_schedule
  
  baseline_roster <- all_sim_results[["baseline"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  cand_totals <- baseline_roster %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  years_in_filter <- year_filter[1]:year_filter[2]
  quota_totals <- tibble(dept_id = 1:nrow(departments)) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y])) %>%
    group_by(prestige_tier) %>%
    summarise(quota = sum(h_j), n_hiring_events = sum(h_j > 0), .groups = "drop") %>%
    mutate(prestige_tier = factor(prestige_tier,
                                  levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  process_hires <- function(df) {
    df %>% mutate(
      quality_tier = tier_int_to_str(cand_tier),
      prestige_tier = tier_int_to_str(dept_tier),
      utility_for_plot = dplyr::coalesce(
        if ("U_true" %in% names(.)) U_true else NA_real_,
        if ("cand_util" %in% names(.)) cand_util else NA_real_))
  }
  baseline_processed <- process_hires(baseline_results) %>% mutate(scenario = "Baseline")
  full_processed <- process_hires(full_results) %>% mutate(scenario = "Questionnaire")
  
  combined_hires <- bind_rows(baseline_processed, full_processed) %>%
    dplyr::filter(!is.na(quality_tier), !is.na(prestige_tier))
  
  heatmap_data <- combined_hires %>%
    group_by(scenario, prestige_tier, quality_tier) %>%
    summarise(n_hires = n(), mean_utility = mean(utility_for_plot, na.rm = TRUE), .groups = "drop") %>%
    mutate(prestige_tier = factor(prestige_tier,
                                  levels = c("Tier 4","Tier 3","Tier 2","Tier 1")),
           quality_tier = factor(quality_tier,
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")),
           scenario = factor(scenario, levels = c("Baseline","Questionnaire"))) %>%
    complete(scenario, prestige_tier, quality_tier,
             fill = list(n_hires = 0, mean_utility = NA))
  
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
    geom_text(aes(label = ifelse(n_hires > 0, n_hires, "")),
              fontface = "bold", size = 5, vjust = 0.3, color = "white") +
    geom_text(aes(label = ifelse(is.finite(mean_utility),
                                 sprintf("bar(U)[ji]==%.3f", mean_utility), "")),
              size = 3.5, vjust = 1.8, color = "white", parse = TRUE) +
    facet_wrap(~ scenario, ncol = 2) +
    scale_fill_viridis_c(name = expression(atop(bar(U)[ji], "Mean Utility")),
                         limits = c(0,1), na.value = "grey70", option = "viridis", breaks = seq(0,1,0.25)) +
    scale_x_discrete(labels = x_labels) +
    scale_y_discrete(labels = y_labels) +
    labs(x = "Candidate Quality Tier", y = "Department Prestige Tier") +
    theme_jasa(base_size = 14) +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "gray95", color = NA),
          legend.position = "right",
          legend.title = element_text(size = 12, face = "bold"),
          panel.spacing = unit(1.5, "lines"))
  
  fill_rate_summary <- heatmap_data %>%
    mutate(tier_for_join = as.character(prestige_tier)) %>%
    group_by(scenario, tier_for_join) %>%
    summarise(total_hires = sum(n_hires), .groups = "drop") %>%
    left_join(quota_totals %>% mutate(tier_for_join = as.character(prestige_tier)),
              by = "tier_for_join") %>%
    mutate(fill_rate = ifelse(quota > 0, total_hires / quota, NA_real_)) %>%
    dplyr::select(scenario, prestige_tier, total_hires, quota, fill_rate)
  
  list(plot = p, data = heatmap_data, candidate_totals = cand_totals,
       hiring_quotas = quota_totals, fill_rates = fill_rate_summary)
}


# =============================================================================
# 3. DEPARTMENT WELFARE BY TIER (NORMALIZED)
# =============================================================================
make_fig_department_welfare_by_tier_normalized <- function(all_sim_results, year_filter = c(1, 10),
                                                           include_scramble = FALSE) {
  
  # Get department info
  departments <- all_sim_results[["baseline"]]$departments %>%
    dplyr::select(dept_id, prestige_tier, s_j)
  
  yearly_hiring_schedule <- all_sim_results[["baseline"]]$yearly_hiring_schedule
  years_in_filter <- year_filter[1]:year_filter[2]
  
  # Calculate total quota by tier
  quota_by_tier <- tibble(dept_id = 1:nrow(departments)) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y])) %>%
    group_by(prestige_tier) %>%
    summarise(total_quota = sum(h_j), n_depts = n_distinct(dept_id),
              n_hiring_events = sum(h_j > 0), .groups = "drop")
  
  # Department-year level data
  dept_year_quotas <- tibble(dept_id = 1:nrow(departments)) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y]))
  
  # Extract hired candidates
  dept_welfare_data <- purrr::map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    full_results <- all_sim_results[[rate_name]]$results
    filtered <- full_results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1)
    if (!include_scramble) {
      filtered <- filtered %>% filter(interviewed == 1 | is.na(interviewed))
    }
    filtered %>%
      mutate(prestige_tier = tier_int_to_str(dept_tier),
             U_effective = dplyr::coalesce(U_true, cand_util),
             participation_rate = rate)
  })
  
  # Calculate welfare metrics by tier
  welfare_by_tier <- dept_welfare_data %>%
    group_by(participation_rate, prestige_tier) %>%
    summarise(n_hires = n(), total_utility = sum(U_effective, na.rm = TRUE),
              mean_utility_per_hire = mean(U_effective, na.rm = TRUE),
              se_per_hire = sd(U_effective, na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
    left_join(quota_by_tier, by = "prestige_tier") %>%
    mutate(mean_utility_per_slot = ifelse(total_quota > 0, total_utility / total_quota, NA_real_),
           fill_rate = ifelse(total_quota > 0, n_hires / total_quota, NA_real_),
           utility_per_dept = ifelse(n_depts > 0, total_utility / n_depts, NA_real_),
           prestige_tier = factor(prestige_tier,
                                  levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  # Department-year level welfare for statistical testing
  dept_level_welfare <- purrr::map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    full_results <- all_sim_results[[rate_name]]$results
    all_hires <- full_results %>%
      filter(strategy == "pairwise", accepted == 1) %>%
      mutate(U_effective = dplyr::coalesce(U_true, cand_util))
    if (!include_scramble) {
      all_hires <- all_hires %>% filter(interviewed == 1 | is.na(interviewed))
    }
    hires_with_year <- all_hires %>%
      filter(!is.na(year), year >= year_filter[1], year <= year_filter[2]) %>%
      group_by(dept_id, year) %>%
      summarise(total_utility = sum(U_effective, na.rm = TRUE), n_hires = n(), .groups = "drop")
    dept_year_quotas %>%
      left_join(hires_with_year, by = c("dept_id","year")) %>%
      mutate(total_utility = dplyr::coalesce(total_utility, 0),
             n_hires = dplyr::coalesce(n_hires, 0L),
             welfare_per_slot = ifelse(h_j > 0, total_utility / h_j, NA_real_),
             participation_rate = rate)
  })
  
  # Statistical tests by tier
  cat("\n=== DEPARTMENT WELFARE GAINS BY TIER ===\n")
  if (!include_scramble) cat("  (scramble hires EXCLUDED)\n")
  else cat("  (scramble hires INCLUDED)\n")
  
  tier_tests <- list()
  tier_summaries <- list()
  
  for (tier in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
    baseline_dept <- dept_level_welfare %>%
      filter(participation_rate == 0, prestige_tier == tier, h_j > 0) %>%
      pull(welfare_per_slot)
    
    full_dept <- dept_level_welfare %>%
      filter(participation_rate == 1, prestige_tier == tier, h_j > 0) %>%
      pull(welfare_per_slot)
    
    if (length(baseline_dept) > 1 && length(full_dept) > 1) {
      test <- t.test(full_dept, baseline_dept, paired = TRUE)
      tier_tests[[tier]] <- test
      
      baseline_fill <- dept_level_welfare %>%
        filter(participation_rate == 0, prestige_tier == tier, h_j > 0) %>%
        summarise(fill_rate = sum(n_hires) / sum(h_j)) %>%
        pull(fill_rate)
      
      full_fill <- dept_level_welfare %>%
        filter(participation_rate == 1, prestige_tier == tier, h_j > 0) %>%
        summarise(fill_rate = sum(n_hires) / sum(h_j)) %>%
        pull(fill_rate)
      
      tier_summaries[[tier]] <- list(
        baseline_mean = mean(baseline_dept, na.rm = TRUE),
        full_mean = mean(full_dept, na.rm = TRUE),
        gain = mean(full_dept, na.rm = TRUE) - mean(baseline_dept, na.rm = TRUE),
        baseline_fill = baseline_fill,
        full_fill = full_fill,
        p_value = test$p.value,
        n_hiring_events = sum(dept_level_welfare$prestige_tier == tier &
                                dept_level_welfare$h_j > 0 &
                                dept_level_welfare$participation_rate == 0)
      )
      
      cat(tier, ":\n")
      cat("  N hiring dept-years:", tier_summaries[[tier]]$n_hiring_events, "\n")
      cat("  Baseline: Welfare/slot = %.4f, Fill rate = %.1f%%\n" %>%
            sprintf(mean(baseline_dept, na.rm = TRUE), baseline_fill * 100))
      cat("  Full:     Welfare/slot = %.4f, Fill rate = %.1f%%\n" %>%
            sprintf(mean(full_dept, na.rm = TRUE), full_fill * 100))
      cat("  Gain:     %.4f (p = %.4f) %s\n" %>%
            sprintf(mean(full_dept, na.rm = TRUE) - mean(baseline_dept, na.rm = TRUE),
                    test$p.value,
                    ifelse(test$p.value < 0.05, "✓", "")))
      cat("\n")
    } else {
      cat(tier, ": Insufficient data for test\n\n")
    }
  }
  
  # Correlation test
  gain_corr <- NULL
  if (length(tier_summaries) >= 3) {
    gains_by_tier <- tibble(tier = names(tier_summaries), 
                            gain = purrr::map_dbl(tier_summaries, "gain")) %>%
      left_join(departments %>% group_by(prestige_tier) %>% 
                  summarise(s_j_mean = mean(s_j), .groups = "drop") %>%
                  rename(tier = prestige_tier), by = "tier") %>%
      arrange(s_j_mean)
    gain_corr <- cor.test(gains_by_tier$s_j_mean, gains_by_tier$gain, method = "spearman")
    cat(sprintf("Correlation(s_j, gain): %.3f (p=%.4f)\n", gain_corr$estimate, gain_corr$p.value))
  }
  
  # ============================================================================
  # Plots
  # ============================================================================
  
  # Plot 1: Welfare per department
  p1 <- ggplot(welfare_by_tier, aes(x = participation_rate * 100,
                                    y = utility_per_dept,
                                    linetype = prestige_tier,
                                    shape = prestige_tier,
                                    group = prestige_tier)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_x_continuous(
      breaks = c(0, 5, 20, 50, 90, 100),
      labels = c("0", "5", "20", "50", "90", "100")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = "Mean Welfare per Department",
      linetype = "Department Tier",
      shape = "Department Tier"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Plot 2: Welfare per slot
  p2 <- ggplot(welfare_by_tier, aes(x = participation_rate * 100,
                                    y = mean_utility_per_slot,
                                    linetype = prestige_tier,
                                    shape = prestige_tier,
                                    group = prestige_tier)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_x_continuous(
      breaks = c(0, 5, 20, 50, 90, 100),
      labels = c("0", "5", "20", "50", "90", "100")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = "Mean Welfare per Position",
      linetype = "Department Tier",
      shape = "Department Tier"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Plot 3: Welfare gains
  welfare_gains <- welfare_by_tier %>%
    group_by(prestige_tier) %>%
    mutate(baseline_utility_per_dept = utility_per_dept[participation_rate == 0],
           welfare_gain_per_dept = utility_per_dept - baseline_utility_per_dept) %>%
    ungroup() %>%
    filter(participation_rate > 0)
  
  p3 <- ggplot(welfare_gains, aes(x = participation_rate * 100,
                                  y = welfare_gain_per_dept,
                                  linetype = prestige_tier,
                                  shape = prestige_tier,
                                  group = prestige_tier)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray50", linewidth = 0.8) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_x_continuous(
      breaks = c(5, 20, 50, 90, 100),
      labels = c("5", "20", "50", "90", "100")
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = "Welfare Gain per Department vs. Baseline",
      linetype = "Department Tier",
      shape = "Department Tier"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  combined_plot_2 <- p1 + p2 +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
  
  combined_plot_3 <- p1 + p2 + p3 +
    plot_layout(ncol = 3, guides = "collect") &
    theme(legend.position = "bottom")
  
  list(
    plot = combined_plot_2,
    plot_3panel = combined_plot_3,
    plot_total_welfare = p1,
    plot_welfare_per_slot = p2,
    plot_gains = p3,
    tests = tier_tests,
    tier_summaries = tier_summaries,
    gain_correlation = gain_corr,
    aggregate_data = welfare_by_tier,
    dept_level_data = dept_level_welfare,
    quota_by_tier = quota_by_tier
  )
}


# =============================================================================
# 4. CANDIDATE WELFARE BY TIER (REVISED)
# =============================================================================
make_fig_candidate_welfare_by_tier_revised <- function(all_sim_results, year_filter = c(1, 10),
                                                       include_scramble = FALSE) {
  
  # ============================================================================
  # Data Extraction
  # ============================================================================
  
  welfare_data_by_tier <- purrr::map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    roster <- all_sim_results[[rate_name]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2])
    full_results <- all_sim_results[[rate_name]]$results
    
    # Regular-round matches within year_filter
    matches <- full_results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1)
    if (!include_scramble) {
      matches <- matches %>% filter(interviewed == 1 | is.na(interviewed))
    }
    matches <- matches %>% mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))
    
    # Join by BOTH year and cand_id to avoid inflation
    roster %>%
      left_join(matches %>%
                  group_by(year, cand_id) %>%
                  slice(1) %>%
                  summarise(welfare = first(V_ij),
                            match_s_j = first(s_j),
                            match_f_j = first(f_j),
                            .groups = "drop"),
                by = c("year", "cand_id")) %>%
      mutate(welfare = dplyr::coalesce(welfare, 0),
             matched = welfare > 0,
             participation_rate = rate)
  })
  
  if (!include_scramble) cat("  [candidate welfare] Scramble hires EXCLUDED\n")
  else cat("  [candidate welfare] Scramble hires INCLUDED\n")
  
  # ============================================================================
  # Compute Metrics
  # ============================================================================
  
  unconditional_welfare <- welfare_data_by_tier %>%
    group_by(participation_rate, quality_tier) %>%
    summarise(
      n_cand = n(),
      n_matches = sum(matched),
      matching_rate = mean(matched),
      mean_welfare_per_candidate = mean(welfare),
      .groups = "drop"
    ) %>%
    mutate(quality_tier = factor(quality_tier, 
                                 levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  
  conditional_welfare <- welfare_data_by_tier %>%
    filter(matched) %>%
    group_by(participation_rate, quality_tier) %>%
    summarise(
      n_matches = n(),
      mean_welfare_if_matched = mean(welfare),
      mean_f_j_if_matched = mean(match_f_j, na.rm = TRUE),
      mean_s_j_if_matched = mean(match_s_j, na.rm = TRUE),
      se_welfare = sd(welfare) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(quality_tier = factor(quality_tier, 
                                 levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  
  # Compute gains relative to baseline
  baseline_values <- conditional_welfare %>%
    filter(participation_rate == 0) %>%
    dplyr::select(quality_tier, baseline_welfare = mean_welfare_if_matched)
  
  conditional_gains <- conditional_welfare %>%
    left_join(baseline_values, by = "quality_tier") %>%
    mutate(
      welfare_gain = if_else(!is.na(baseline_welfare), 
                             mean_welfare_if_matched - baseline_welfare, 
                             NA_real_),
      pct_gain = if_else(!is.na(baseline_welfare) & baseline_welfare > 0,
                         (mean_welfare_if_matched / baseline_welfare - 1) * 100,
                         NA_real_)
    )
  
  # ============================================================================
  # Statistical Tests
  # ============================================================================
  
  cat("\n=== CONDITIONAL WELFARE (E[Welfare | Matched]) BY TIER ===\n")
  cat("This measures MATCH QUALITY, independent of matching rate\n\n")
  
  conditional_tests <- list()
  conditional_summaries <- list()
  
  for (tier in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
    baseline_matched <- welfare_data_by_tier %>%
      filter(participation_rate == 0, quality_tier == tier, matched) %>%
      pull(welfare)
    
    full_matched <- welfare_data_by_tier %>%
      filter(participation_rate == 1, quality_tier == tier, matched) %>%
      pull(welfare)
    
    if (length(baseline_matched) >= 5 && length(full_matched) >= 5) {
      test <- t.test(full_matched, baseline_matched)
      conditional_tests[[tier]] <- test
      
      conditional_summaries[[tier]] <- list(
        baseline_n = length(baseline_matched),
        full_n = length(full_matched),
        baseline_mean = mean(baseline_matched),
        full_mean = mean(full_matched),
        gain = mean(full_matched) - mean(baseline_matched),
        pct_gain = (mean(full_matched) / mean(baseline_matched) - 1) * 100,
        p_value = test$p.value
      )
      
      cat(tier, ":\n")
      cat("  Baseline: n=%d matches, E[W|match] = %.4f\n" %>% 
            sprintf(length(baseline_matched), mean(baseline_matched)))
      cat("  Full:     n=%d matches, E[W|match] = %.4f\n" %>%
            sprintf(length(full_matched), mean(full_matched)))
      cat("  Gain:     %.4f (%+.1f%%) (p = %.4f) %s\n\n" %>%
            sprintf(mean(full_matched) - mean(baseline_matched),
                    (mean(full_matched) / mean(baseline_matched) - 1) * 100,
                    test$p.value,
                    ifelse(test$p.value < 0.05, "✓", "")))
    } else {
      cat(tier, ":\n")
      if (length(baseline_matched) < 5) {
        cat("  Baseline: n=%d matches (insufficient for test)\n" %>% 
              sprintf(length(baseline_matched)))
      } else {
        cat("  Baseline: n=%d matches, E[W|match] = %.4f\n" %>% 
              sprintf(length(baseline_matched), mean(baseline_matched)))
      }
      if (length(full_matched) < 5) {
        cat("  Full:     n=%d matches (insufficient for test)\n" %>%
              sprintf(length(full_matched)))
      } else {
        cat("  Full:     n=%d matches, E[W|match] = %.4f\n" %>%
              sprintf(length(full_matched), mean(full_matched)))
      }
      cat("  Test:     Cannot perform comparison (insufficient baseline data)\n\n")
      
      conditional_summaries[[tier]] <- list(
        baseline_n = length(baseline_matched),
        full_n = length(full_matched),
        baseline_mean = if(length(baseline_matched) > 0) mean(baseline_matched) else NA,
        full_mean = if(length(full_matched) > 0) mean(full_matched) else NA,
        gain = NA, pct_gain = NA, p_value = NA
      )
    }
  }
  
  # ============================================================================
  # Plots
  # ============================================================================
  
  p1_conditional <- ggplot(conditional_welfare, 
                           aes(x = participation_rate * 100, 
                               y = mean_welfare_if_matched,
                               linetype = quality_tier,
                               shape = quality_tier,
                               size = quality_tier,
                               group = quality_tier)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(color = "black") +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_size_manual(values = c(3, 3, 3, 4.5)) +
    scale_x_continuous(
      breaks = c(0, 5, 20, 50, 90, 100),
      labels = c("0", "5", "20", "50", "90", "100")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = "Mean Candidate Utility",
      linetype = "Candidate Tier",
      shape = "Candidate Tier",
      size = "Candidate Tier"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  p2_gains <- conditional_gains %>%
    filter(participation_rate > 0, !is.na(welfare_gain)) %>%
    ggplot(aes(x = participation_rate * 100, 
               y = welfare_gain,
               linetype = quality_tier,
               shape = quality_tier,
               group = quality_tier)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray50", linewidth = 0.8) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_x_continuous(
      breaks = c(5, 20, 50, 90, 100),
      labels = c("5", "20", "50", "90", "100")
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = expression(Delta*bar(V)[ij]),
      linetype = "Candidate Tier",
      shape = "Candidate Tier"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  p3_alignment <- ggplot(conditional_welfare, 
                         aes(x = participation_rate * 100, 
                             y = mean_f_j_if_matched,
                             linetype = quality_tier,
                             shape = quality_tier,
                             group = quality_tier)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_x_continuous(
      breaks = c(0, 5, 20, 50, 90, 100),
      labels = c("0", "5", "20", "50", "90", "100")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = expression(E*"[Alignment "*f[j]*" | Matched]"),
      linetype = "Candidate Tier",
      shape = "Candidate Tier"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  combined_2panel <- p1_conditional + p2_gains +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
  
  combined_3panel <- p1_conditional + p2_gains + p3_alignment +
    plot_layout(ncol = 3, guides = "collect") &
    theme(legend.position = "bottom")
  
  list(
    plot = combined_2panel,
    plot_3panel = combined_3panel,
    plot_conditional = p1_conditional,
    plot_gains = p2_gains,
    plot_alignment = p3_alignment,
    conditional_welfare = conditional_welfare,
    conditional_gains = conditional_gains,
    unconditional_welfare = unconditional_welfare,
    welfare_data = welfare_data_by_tier,
    conditional_tests = conditional_tests,
    conditional_summaries = conditional_summaries
  )
}

# =============================================================================
# 5. CANDIDATE BY PARTICIPATION (Participating vs Non-participating)
# =============================================================================
make_fig_candidate_by_participation <- function(all_sim_results, year_filter = c(1, 10),
                                                include_scramble = FALSE) {
  
  # Baseline stats
  bl_matches <- all_sim_results$baseline$results %>%
    filter(strategy == "pairwise", accepted == 1)
  bl_wy <- bl_matches %>% filter(year >= year_filter[1], year <= year_filter[2])
  if (!include_scramble) {
    bl_wy <- bl_wy %>% filter(interviewed == 1 | is.na(interviewed))
  }
  bl_all <- bl_wy %>%
    mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))
  
  baseline_stats <- all_sim_results$baseline$cand_roster %>%
    filter(year >= year_filter[1], year <= year_filter[2]) %>%
    dplyr::select(year, cand_id) %>%
    left_join(bl_all %>%
                group_by(year, cand_id) %>%
                slice(1) %>%
                summarise(welfare = first(V_ij), .groups = "drop"),
              by = c("year", "cand_id")) %>%
    mutate(welfare = coalesce(welfare, 0), matched = welfare > 0) %>%
    summarise(baseline_mean_welfare = mean(welfare),
              baseline_matching_rate = mean(matched),
              baseline_mean_welfare_if_matched = mean(welfare[matched], na.rm = TRUE))
  
  cat("\n=== BASELINE STATISTICS ===\n")
  if (!include_scramble) cat("  (scramble hires EXCLUDED)\n")
  else cat("  (scramble hires INCLUDED)\n")
  cat(sprintf("Mean welfare: %.4f | Matching rate: %.2f%% | Mean welfare if matched: %.4f\n",
              baseline_stats$baseline_mean_welfare, baseline_stats$baseline_matching_rate * 100,
              baseline_stats$baseline_mean_welfare_if_matched))
  
  interior_rates <- setdiff(names(all_sim_results), c("baseline", "1"))
  
  comparison_data <- purrr::map_dfr(interior_rates, function(rate_name) {
    rate <- as.numeric(rate_name)
    
    roster <- all_sim_results[[rate_name]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2])
    if (!"participates" %in% names(roster)) {
      n_per_year <- roster %>% group_by(year) %>% summarise(n = n(), .groups = "drop") %>% pull(n) %>% unique()
      n_part <- floor(n_per_year[1] * rate)
      roster <- roster %>% group_by(year) %>% mutate(participates = cand_id <= n_part) %>% ungroup()
    }
    
    full_results <- all_sim_results[[rate_name]]$results
    matched_regular <- full_results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1)
    if (!include_scramble) {
      matched_regular <- matched_regular %>% filter(interviewed == 1 | is.na(interviewed))
    }
    matched_regular <- matched_regular %>%
      mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j)) %>%
      group_by(year, cand_id) %>%
      slice(1) %>%
      summarise(welfare = first(V_ij), .groups = "drop")
    
    roster %>%
      left_join(matched_regular, by = c("year", "cand_id")) %>%
      mutate(welfare = coalesce(welfare, 0), matched = welfare > 0) %>%
      group_by(participates) %>%
      summarise(participation_rate = rate, n_candidates = n(), n_matched = sum(matched),
                matching_rate = mean(matched), mean_welfare = mean(welfare),
                se_welfare = sd(welfare) / sqrt(n()),
                mean_welfare_if_matched = mean(welfare[matched], na.rm = TRUE),
                se_welfare_if_matched = sd(welfare[matched], na.rm = TRUE) / sqrt(sum(matched)),
                .groups = "drop") %>%
      mutate(participation_status = ifelse(participates, "Participating", "Non-participating"))
  })
  
  if (nrow(comparison_data) == 0) {
    cat("  No interior rates found for participation comparison.\n")
    cat("  Need rates between 0 and 1 (e.g. 0.05, 0.2, 0.5, 0.9).\n")
    return(list(data = comparison_data, baseline_stats = baseline_stats))
  }
  
  # Diagnostic
  cat("\n=== MATCHING RATE SANITY CHECK ===\n")
  for (rate in unique(comparison_data$participation_rate)) {
    sub <- comparison_data %>% filter(participation_rate == rate)
    total_cand <- sum(sub$n_candidates)
    total_matched <- sum(sub$n_matched)
    cat(sprintf("Rate=%.0f%%: %d/%d matched overall (%.1f%%)\n",
                rate * 100, total_matched, total_cand, 100 * total_matched / total_cand))
    for (i in 1:nrow(sub)) {
      cat(sprintf("  %s: %d/%d matched (%.1f%%)\n",
                  sub$participation_status[i], sub$n_matched[i],
                  sub$n_candidates[i], sub$matching_rate[i] * 100))
    }
  }
  
  # Statistical tests
  cat("\n=== PARTICIPATING vs NON-PARTICIPATING ===\n")
  for (rate in unique(comparison_data$participation_rate)) {
    pd <- comparison_data %>% filter(participation_rate == rate, participates == TRUE)
    nd <- comparison_data %>% filter(participation_rate == rate, participates == FALSE)
    if (nrow(pd) > 0 && nrow(nd) > 0) {
      wd <- pd$mean_welfare - nd$mean_welfare
      se_d <- sqrt(pd$se_welfare^2 + nd$se_welfare^2)
      cat(sprintf("Rate=%.0f%%: Part=%.4f, NonPart=%.4f, Diff=%.4f (t=%.2f) %s\n",
                  rate * 100, pd$mean_welfare, nd$mean_welfare, wd, wd / se_d,
                  ifelse(abs(wd / se_d) > 1.96, "\u2713 sig", "")))
    }
  }
  
  # Plots with styling from second version
  p1 <- ggplot(comparison_data, 
               aes(x = participation_rate * 100,
                   y = mean_welfare,
                   linetype = participation_status,
                   shape = participation_status)) +
    geom_hline(yintercept = baseline_stats$baseline_mean_welfare,
               linetype = "dotted", color = "gray50", linewidth = 0.8) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3.5, color = "black") +
    scale_linetype_manual(
      values = c("Participating" = "solid", "Non-participating" = "dashed")
    ) +
    scale_shape_manual(
      values = c("Participating" = 16, "Non-participating" = 17)
    ) +
    scale_x_continuous(
      breaks = c(5, 20, 50, 90),
      labels = c("5", "20", "50", "90")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = "Market Participation Rate (%)",
         y = "Mean Candidate Welfare",
         linetype = NULL, shape = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  p2 <- ggplot(comparison_data,
               aes(x = participation_rate * 100,
                   y = matching_rate,
                   linetype = participation_status,
                   shape = participation_status)) +
    geom_hline(yintercept = baseline_stats$baseline_matching_rate,
               linetype = "dotted", color = "gray50", linewidth = 0.8) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3.5, color = "black") +
    scale_linetype_manual(
      values = c("Participating" = "solid", "Non-participating" = "dashed")
    ) +
    scale_shape_manual(
      values = c("Participating" = 16, "Non-participating" = 17)
    ) +
    scale_x_continuous(
      breaks = c(5, 20, 50, 90),
      labels = c("5", "20", "50", "90")
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = "Market Participation Rate (%)",
         y = "Candidate Matching Rate",
         linetype = NULL, shape = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  p3 <- ggplot(comparison_data,
               aes(x = participation_rate * 100,
                   y = mean_welfare_if_matched,
                   linetype = participation_status,
                   shape = participation_status)) +
    geom_hline(yintercept = baseline_stats$baseline_mean_welfare_if_matched,
               linetype = "dotted", color = "gray50", linewidth = 0.8) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3.5, color = "black") +
    scale_linetype_manual(
      values = c("Participating" = "solid", "Non-participating" = "dashed")
    ) +
    scale_shape_manual(
      values = c("Participating" = 16, "Non-participating" = 17)
    ) +
    scale_x_continuous(
      breaks = c(5, 20, 50, 90),
      labels = c("5", "20", "50", "90")
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = "Market Participation Rate (%)",
         y = "Mean Welfare (If Matched)",
         linetype = NULL, shape = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(2.5, "lines"),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  combined_2panel <- (p1 | p2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  combined_3panel <- (p1 | p2 | p3) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  list(
    plot = combined_2panel,
    plot_3panel = combined_3panel,
    plot_welfare = p1,
    plot_matching = p2,
    plot_welfare_conditional = p3,
    data = comparison_data,
    baseline_stats = baseline_stats
  )
}



#source("visualization_functions.R")



# Convert once
all_sim_results <- convert_to_viz_format(full_results)

# Then call any figure function
(interview_heatmap  <- make_fig_dept_interview_heatmap(all_sim_results, year_filter = c(1, 10), include_scramble = F))
(hiring_heatmap     <- make_fig_dept_hiring_heatmap(all_sim_results, year_filter = c(1, 10), include_scramble = T))
(dept_welfare       <- make_fig_department_welfare_by_tier_normalized(all_sim_results, year_filter = c(1, 10), include_scramble = F))
(cand_welfare       <- make_fig_candidate_welfare_by_tier_revised(all_sim_results, year_filter = c(1, 10), include_scramble = F))
(cand_participation <- make_fig_candidate_by_participation(all_sim_results, year_filter = c(1, 10), include_scramble = F))


#######################################################
################## SAVE FIGURES #######################
#######################################################

ggsave("fig_dept_interview_heatmap.pdf",
       interview_heatmap$plot,
       width = 10, height = 5,
       device = cairo_pdf)


ggsave("fig_dept_hiring_heatmap.pdf",
       hiring_heatmap$plot,
       width = 10, height = 5,
       device = cairo_pdf)


ggsave("fig_department_welfare.pdf",
       dept_welfare$plot_welfare_per_slot,
       width = 7, height = 5,
       device = cairo_pdf)


ggsave("fig_candidate_welfare_by_tier.pdf",
       cand_welfare$plot_conditional,
       width = 7, height = 5,
       device = cairo_pdf)


ggsave("fig_candidate_by_participation.pdf",
       cand_participation$plot,
       width = 10, height = 4,
       device = cairo_pdf)

#######################################################
#######################################################
#######################################################

