

# =============================================================================
# ACADEMIC JOB MARKET SIMULATION - FIXED VERSION
# =============================================================================
# Key fixes:
# 1. Proper handling of categorical variables with type coercion
# 2. Safe indexing for embeddings
# 3. Better debugging output
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
})

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

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

true_utility <- function(s_j, v_i_bar, f_j, dept_tier, cand_tier, lambda = 1.2) {
  tier_gap <- cand_tier - dept_tier
  U_det <- exp(s_j * log(v_i_bar + 1e-8) + (1 - s_j) * log(f_j + 1e-8))
  penalty <- exp(-lambda * pmax(tier_gap, 0))
  U_det * penalty
}

candidate_utility <- function(v_i_bar, s_j, f_j) {
  exp(v_i_bar * log(s_j + 1e-8) + (1 - v_i_bar) * log(f_j + 1e-8))
}

# =============================================================================
# QUESTIONNAIRE STRUCTURE
# =============================================================================

questions <- list(
  numerical = c(
    "q4_cost_of_living",
    "q6_typical_salary_range",
    "q14_phd_student_ratio"
  ),
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

# =============================================================================
# SAFE CATEGORICAL INDEX FUNCTION
# =============================================================================

#' Safely convert categorical values to indices for embedding
#' @param values Vector of values to convert
#' @param levels Valid levels for this categorical
#' @param var_name Name of variable (for debugging)
#' @return Integer vector of indices in range [1, length(levels)]
safe_categorical_to_index <- function(values, levels, var_name = "unknown") {
  # Ensure everything is character for consistent matching
  values_char <- as.character(values)
  levels_char <- as.character(levels)
  n_levels <- length(levels_char)
  
  # Handle NA, empty, and "NA" strings
  values_char[is.na(values_char) | values_char == "" | values_char == "NA"] <- levels_char[1]
  
  # Match values to levels
  indices <- match(values_char, levels_char)
  
  # Check for unmatched values
  unmatched_mask <- is.na(indices)
  if (any(unmatched_mask)) {
    unmatched_vals <- unique(values_char[unmatched_mask])
    cat("WARNING [", var_name, "]: Unmatched values: ", 
        paste(head(unmatched_vals, 5), collapse = ", "), "\n")
    cat("  Expected levels: ", paste(levels_char, collapse = ", "), "\n")
    # Default unmatched to first level
    indices[unmatched_mask] <- 1L
  }
  
  # Final safety: ensure all indices are in valid range
  indices <- as.integer(indices)
  out_of_range <- indices < 1 | indices > n_levels
  if (any(out_of_range, na.rm = TRUE)) {
    cat("ERROR [", var_name, "]: Out of range indices detected, clamping\n")
    indices <- pmax(1L, pmin(indices, n_levels))
  }
  
  indices
}

# =============================================================================
# ALIGNMENT FUNCTION f_j
# =============================================================================

calculate_f_j <- function(candidate_row, department_row, questions) {
  if (is.data.frame(candidate_row))  candidate_row  <- as.list(candidate_row[1, ])
  if (is.data.frame(department_row)) department_row <- as.list(department_row[1, ])
  
  num_q <- length(questions$numerical)
  cat_q <- length(questions$categorical)
  total_q <- num_q + cat_q
  
  # Use department-specific weights if available, otherwise equal weights
  if (!is.null(department_row$weight_vector) && 
      length(department_row$weight_vector[[1]]) == total_q) {
    weights <- department_row$weight_vector[[1]]
  } else {
    weights <- rep(1/total_q, total_q)
  }
  
  w_sum <- 0
  S_num <- 0
  idx <- 1L
  
  # Process numerical questions
  for (q in questions$numerical) {
    cand_val <- candidate_row[[paste0("q_", q)]]
    dept_val <- department_row[[q]]
    w_k <- weights[idx]
    idx <- idx + 1L
    
    if (is.null(cand_val) || is.na(cand_val) || is.null(dept_val) || is.na(dept_val)) {
      s_k <- 0.5
    } else {
      if (q %in% c("q4_cost_of_living", "q6_typical_salary_range")) {
        if (q == "q4_cost_of_living") {
          range_vals <- c(60000, 140000)
        } else {
          range_vals <- c(80000, 200000)
        }
        cx <- (as.numeric(cand_val) - range_vals[1]) / (range_vals[2] - range_vals[1])
        dx <- (as.numeric(dept_val) - range_vals[1]) / (range_vals[2] - range_vals[1])
        cx <- pmin(pmax(cx, 0), 1)
        dx <- pmin(pmax(dx, 0), 1)
        s_k <- 1 - abs(cx - dx)
      } else if (q == "q14_phd_student_ratio") {
        cx <- (as.numeric(cand_val) - 0.5) / 4.5
        dx <- (as.numeric(dept_val) - 0.5) / 4.5
        cx <- pmin(pmax(cx, 0), 1)
        dx <- pmin(pmax(dx, 0), 1)
        s_k <- 1 - abs(cx - dx)
      } else {
        s_k <- 0.5
      }
    }
    S_num <- S_num + w_k * s_k
    w_sum <- w_sum + w_k
  }
  
  # Process categorical questions
  for (q_name in names(questions$categorical)) {
    cand_val <- candidate_row[[paste0("q_", q_name)]]
    dept_val <- department_row[[q_name]]
    w_k <- weights[idx]
    idx <- idx + 1L
    
    if (is.null(cand_val) || is.na(cand_val) || is.null(dept_val) || is.na(dept_val)) {
      s_k <- 0
    } else {
      # Handle q2_region multi-select
      if (q_name == "q2_region") {
        cand_regions <- if(is.character(cand_val)) str_split(cand_val, ",")[[1]] else as.character(cand_val)
        cand_regions <- trimws(cand_regions)
        s_k <- as.numeric(as.character(dept_val) %in% cand_regions)
      } else {
        s_k <- as.numeric(as.character(cand_val) == as.character(dept_val))
      }
    }
    S_num <- S_num + w_k * s_k
    w_sum <- w_sum + w_k
  }
  
  S_ij <- if (w_sum > 0) S_num / w_sum else 0
  
  gamma <- 0.1 #2.0
  f <- (exp(gamma * S_ij) - 1) / (exp(gamma) - 1)
  
  eps <- 1e-6
  as.numeric(pmin(pmax(f, eps), 1 - eps))
}


# Candidate's evaluation of how well a department matches THEIR preferences
calculate_candidate_f_j <- function(candidate_row, department_row, questions) {
  if (is.data.frame(candidate_row))  candidate_row  <- as.list(candidate_row[1, ])
  if (is.data.frame(department_row)) department_row <- as.list(department_row[1, ])
  
  num_q <- length(questions$numerical)
  cat_q <- length(questions$categorical)
  total_q <- num_q + cat_q
  
  # Use CANDIDATE-specific weights if available, otherwise equal weights
  if (!is.null(candidate_row$weight_vector) && 
      length(candidate_row$weight_vector[[1]]) == total_q) {
    weights <- candidate_row$weight_vector[[1]]
  } else {
    # Default: candidates weight things differently than departments
    # They care more about compensation, location, teaching load
    weights <- rep(1/total_q, total_q)
  }
  
  # Rest of calculation is same as calculate_f_j...
  w_sum <- 0
  S_num <- 0
  idx <- 1L
  
  for (q in questions$numerical) {
    cand_val <- candidate_row[[paste0("q_", q)]]
    dept_val <- department_row[[q]]
    w_k <- weights[idx]
    idx <- idx + 1L
    
    if (is.null(cand_val) || is.na(cand_val) || is.null(dept_val) || is.na(dept_val)) {
      s_k <- 0.5
    } else {
      if (q %in% c("q4_cost_of_living", "q6_typical_salary_range")) {
        if (q == "q4_cost_of_living") {
          range_vals <- c(60000, 140000)
        } else {
          range_vals <- c(80000, 200000)
        }
        cx <- (as.numeric(cand_val) - range_vals[1]) / (range_vals[2] - range_vals[1])
        dx <- (as.numeric(dept_val) - range_vals[1]) / (range_vals[2] - range_vals[1])
        cx <- pmin(pmax(cx, 0), 1)
        dx <- pmin(pmax(dx, 0), 1)
        s_k <- 1 - abs(cx - dx)
      } else if (q == "q14_phd_student_ratio") {
        cx <- (as.numeric(cand_val) - 0.5) / 4.5
        dx <- (as.numeric(dept_val) - 0.5) / 4.5
        cx <- pmin(pmax(cx, 0), 1)
        dx <- pmin(pmax(dx, 0), 1)
        s_k <- 1 - abs(cx - dx)
      } else {
        s_k <- 0.5
      }
    }
    S_num <- S_num + w_k * s_k
    w_sum <- w_sum + w_k
  }
  
  for (q_name in names(questions$categorical)) {
    cand_val <- candidate_row[[paste0("q_", q_name)]]
    dept_val <- department_row[[q_name]]
    w_k <- weights[idx]
    idx <- idx + 1L
    
    if (is.null(cand_val) || is.na(cand_val) || is.null(dept_val) || is.na(dept_val)) {
      s_k <- 0
    } else {
      if (q_name == "q2_region") {
        cand_regions <- if(is.character(cand_val)) str_split(cand_val, ",")[[1]] else as.character(cand_val)
        cand_regions <- trimws(cand_regions)
        s_k <- as.numeric(as.character(dept_val) %in% cand_regions)
      } else {
        s_k <- as.numeric(as.character(cand_val) == as.character(dept_val))
      }
    }
    S_num <- S_num + w_k * s_k
    w_sum <- w_sum + w_k
  }
  
  S_ij <- if (w_sum > 0) S_num / w_sum else 0
  
  gamma <- 0.1#2.0
  f <- (exp(gamma * S_ij) - 1) / (exp(gamma) - 1)
  
  eps <- 1e-6
  as.numeric(pmin(pmax(f, eps), 1 - eps))
}

# =============================================================================
# CANDIDATE GENERATION
# =============================================================================

# generate_candidates_new <- function(n_candidates, questions, seed = NULL,
#                                     quality_correlation = 0.8) {
#   # quality_correlation: controls how much preferences correlate with quality
#   
#   # 0 = completely random preferences, 1 = strong correlation (original)
#   # Default 0.25 = mild correlation, mostly idiosyncratic
#   
#   if (!is.null(seed)) set.seed(seed)
#   
#   candidates <- tibble(
#     cand_id = 1:n_candidates,
#     v_i1 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5),
#     v_i2 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5),
#     v_i3 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5)
#   ) %>%
#     mutate(v_i_bar = (v_i1 * v_i2 * v_i3)^(1/3))
#   
#   # Create quality percentile for preference correlation
#   candidates <- candidates %>%
#     mutate(quality_pctl = percent_rank(v_i_bar))
#   
#   n <- n_candidates
#   v_pctl <- candidates$quality_pctl
#   qc <- quality_correlation  # Shorthand for scaling
#   
#   # ==========================================================================
#   # NUMERICAL QUESTIONS - Reduced quality correlation, more noise
#   # ==========================================================================
#   
#   # q4_cost_of_living: Mild correlation - some high-quality candidates prefer
#   # low cost areas, some low-quality candidates are fine with high cost
#   candidates$q_q4_cost_of_living <- 60000 + 
#     qc * v_pctl * 40000 +           # Reduced quality effect (was 50000)
#     (1 - qc) * runif(n, 0, 60000) + # Random component
#     rnorm(n, 0, 20000)              # Increased noise
#   candidates$q_q4_cost_of_living <- pmax(60000, pmin(140000, candidates$q_q4_cost_of_living))
#   
#   # q6_typical_salary_range: Some correlation but high variance
#   # Even low-quality candidates may have high salary expectations (dual career, location, etc.)
#   candidates$q_q6_typical_salary_range <- 80000 + 
#     qc * v_pctl * 60000 +           # Reduced from 80000
#     (1 - qc) * runif(n, 0, 80000) + # Random component
#     rnorm(n, 0, 25000)              # More noise
#   candidates$q_q6_typical_salary_range <- pmax(80000, pmin(200000, candidates$q_q6_typical_salary_range))
#   
#   # q14_phd_student_ratio: Weak correlation - research interest varies independently
#   candidates$q_q14_phd_student_ratio <- 0.5 + 
#     qc * v_pctl * 2.0 +             # Reduced from 3.0
#     (1 - qc) * runif(n, 0, 3.5) +   # Random component
#     rnorm(n, 0, 1.0)                # More noise
#   candidates$q_q14_phd_student_ratio <- pmax(0.5, pmin(5.0, candidates$q_q14_phd_student_ratio))
#   
#   # ==========================================================================
#   # CATEGORICAL QUESTIONS - Much more idiosyncratic
#   # ==========================================================================
#   
#   # Helper function: blend quality-based probs with uniform
#   blend_probs <- function(quality_probs, n_levels, qc) {
#     uniform <- rep(1/n_levels, n_levels)
#     blended <- qc * quality_probs + (1 - qc) * uniform
#     blended <- pmax(blended, 0.05)
#     blended / sum(blended)
#   }
#   
#   # q1_geographic_setting: A=Urban, B=Suburban, C=Small city, D=Rural
#   # Personal preference largely independent of quality
#   candidates$q_q1_geographic_setting <- sapply(v_pctl, function(p) {
#     # Quality-based component (high quality slightly prefer urban)
#     q_probs <- c(0.25 + 0.25*p, 0.25 + 0.10*p, 0.25 - 0.15*p, 0.25 - 0.20*p)
#     probs <- blend_probs(q_probs, 4, qc)
#     sample(c("A", "B", "C", "D"), 1, prob = probs)
#   })
#   
#   # q2_region: Geographic preference is highly personal (family, partner, lifestyle)
#   # Minimal quality correlation
#   candidates$q_q2_region <- sapply(v_pctl, function(p) {
#     # Number of regions: mostly random, slight quality effect on selectivity
#     base_selectivity <- sample(1:4, 1, prob = c(0.15, 0.35, 0.35, 0.15))
#     n_reg <- max(1, min(5, base_selectivity - round(qc * p)))
#     
#     # Region probabilities: mostly uniform with slight quality tilt
#     q_probs <- c(
#       "Northeast" = 0.20 + 0.15*p,
#       "West Coast" = 0.20 + 0.10*p,
#       "Midwest" = 0.20,
#       "Southeast" = 0.20 - 0.10*p,
#       "Southwest" = 0.20 - 0.05*p
#     )
#     probs <- blend_probs(q_probs, 5, qc)
#     
#     selected <- sample(names(probs), n_reg, prob = probs, replace = FALSE)
#     paste(selected, collapse = ",")
#   })
#   
#   # q3_airport_proximity: Personal preference, weak quality correlation
#   candidates$q_q3_airport_proximity <- sapply(v_pctl, function(p) {
#     q_probs <- c(0.25 + 0.20*p, 0.25 + 0.05*p, 0.25 - 0.10*p, 0.25 - 0.15*p)
#     probs <- blend_probs(q_probs, 4, qc)
#     sample(c("A", "B", "C", "D"), 1, prob = probs)
#   })
#   
#   # q5_dual_career: Completely random (life circumstances)
#   candidates$q_q5_dual_career <- sample(c("Y", "N"), n, replace = TRUE, prob = c(0.4, 0.6))
#   
#   # q7_typical_startup: A=Lowest, E=Highest
#   # Some correlation (research-focused candidates want more) but high variance
#   candidates$q_q7_typical_startup <- sapply(v_pctl, function(p) {
#     q_probs <- c(0.20 - 0.10*p, 0.20 - 0.05*p, 0.20, 0.20 + 0.05*p, 0.20 + 0.10*p)
#     probs <- blend_probs(q_probs, 5, qc)
#     sample(c("A", "B", "C", "D", "E"), 1, prob = probs)
#   })
#   
#   # q8_guaranteed_summer: A=Full, D=None
#   # Everyone wants summer support, weak quality correlation
#   candidates$q_q8_guaranteed_summer <- sapply(v_pctl, function(p) {
#     q_probs <- c(0.35 + 0.15*p, 0.30, 0.20 - 0.05*p, 0.15 - 0.10*p)
#     probs <- blend_probs(q_probs, 4, qc)
#     sample(c("A", "B", "C", "D"), 1, prob = probs)
#   })
#   
#   # q9_typical_teaching_load: A=Light (1-1), D=Heavy (3-3)
#   # Moderate correlation - research-focused want lighter loads
#   candidates$q_q9_typical_teaching_load <- sapply(v_pctl, function(p) {
#     q_probs <- c(0.25 + 0.25*p, 0.25 + 0.10*p, 0.25 - 0.15*p, 0.25 - 0.20*p)
#     probs <- blend_probs(q_probs, 4, qc)
#     sample(c("A", "B", "C", "D"), 1, prob = probs)
#   })
#   
#   # q10_course_types: A=Mostly grad, D=Mostly undergrad
#   # Weak correlation - some excellent teachers prefer undergrad
#   candidates$q_q10_course_types <- sapply(v_pctl, function(p) {
#     q_probs <- c(0.25 + 0.15*p, 0.25 + 0.05*p, 0.25 - 0.05*p, 0.25 - 0.15*p)
#     probs <- blend_probs(q_probs, 4, qc)
#     sample(c("A", "B", "C", "D"), 1, prob = probs)
#   })
#   
#   # q11_mentoring_program: A=Formal, D=None
#   # Essentially random - personal preference
#   candidates$q_q11_mentoring_program <- sapply(v_pctl, function(p) {
#     probs <- c(0.25, 0.30, 0.25, 0.20)  # No quality correlation
#     sample(c("A", "B", "C", "D"), 1, prob = probs)
#   })
#   
#   # q12_research_culture: A=Independent, E=Highly collaborative
#   # Weak correlation - collaboration preference is personal style
#   candidates$q_q12_research_culture <- sapply(v_pctl, function(p) {
#     q_probs <- c(0.20 - 0.05*p, 0.20, 0.20, 0.20, 0.20 + 0.05*p)
#     probs <- blend_probs(q_probs, 5, qc)
#     sample(c("A", "B", "C", "D", "E"), 1, prob = probs)
#   })
#   
#   # q13_publication_venues: A=Teaching focused, E=Top journals only
#   # Moderate correlation but still variable
#   candidates$q_q13_publication_venues <- sapply(v_pctl, function(p) {
#     q_probs <- c(0.20 - 0.10*p, 0.20 - 0.05*p, 0.20, 0.20 + 0.05*p, 0.20 + 0.10*p)
#     probs <- blend_probs(q_probs, 5, qc)
#     sample(c("A", "B", "C", "D", "E"), 1, prob = probs)
#   })
#   
#   # q15_medical_school_proximity: 0=No, 1=Yes
#   # Field-specific, mostly random
#   candidates$q_q15_medical_school_proximity <- sapply(v_pctl, function(p) {
#     q_probs <- c(0.55 - 0.10*p, 0.45 + 0.10*p)
#     probs <- blend_probs(q_probs, 2, qc)
#     sample(c("0", "1"), 1, prob = probs)
#   })
#   
#   # ==========================================================================
#   # GENERATE CANDIDATE-SPECIFIC WEIGHT VECTORS
#   # ==========================================================================
#   # Candidates weight questions differently than departments when evaluating fit
#   # This affects how candidates choose among multiple offers
#   # These weights are MORE IDIOSYNCRATIC - not strongly tied to quality
#   
#   num_q <- length(questions$numerical)
#   cat_q <- length(questions$categorical)
#   n_total_q <- num_q + cat_q
#   q_names <- c(questions$numerical, names(questions$categorical))
#   
#   weight_list <- vector("list", n_candidates)
#   
#   for (i in 1:n_candidates) {
#     p <- v_pctl[i]  # Quality percentile for this candidate
#     
#     # Start with random base weights (more variance)
#     base_weights <- runif(n_total_q, 0.5, 2.0)
#     
#     for (j in seq_along(q_names)) {
#       q <- q_names[j]
#       
#       # ----- COMPENSATION: Everyone cares, individual variation -----
#       if (q == "q6_typical_salary_range") {
#         # Random importance with slight inverse quality correlation
#         base_weights[j] <- runif(1, 1.5, 3.5) + (1 - p) * qc * runif(1, 0, 1.0)
#       }
#       if (q == "q7_typical_startup") {
#         # Random with slight quality correlation
#         base_weights[j] <- runif(1, 0.8, 2.5) + p * qc * runif(1, 0, 1.5)
#       }
#       if (q == "q8_guaranteed_summer") {
#         # Universally important with variance
#         base_weights[j] <- runif(1, 1.5, 3.0)
#       }
#       
#       # ----- LOCATION: Highly personal, quality-independent -----
#       if (q == "q1_geographic_setting") {
#         base_weights[j] <- runif(1, 1.0, 3.0)  # High variance
#       }
#       if (q == "q2_region") {
#         # Region is very important for many (family, partner)
#         base_weights[j] <- runif(1, 2.0, 4.5)  # High and variable
#       }
#       if (q == "q4_cost_of_living") {
#         # Important for everyone, individual circumstances vary
#         base_weights[j] <- runif(1, 1.0, 3.0)
#       }
#       if (q == "q3_airport_proximity") {
#         base_weights[j] <- runif(1, 0.5, 2.0)
#       }
#       
#       # ----- RESEARCH ENVIRONMENT: Quality correlation reduced -----
#       if (q == "q12_research_culture") {
#         base_weights[j] <- runif(1, 0.8, 2.5) + p * qc * runif(1, 0, 1.0)
#       }
#       if (q == "q13_publication_venues") {
#         base_weights[j] <- runif(1, 0.5, 2.0) + p * qc * runif(1, 0, 1.0)
#       }
#       if (q == "q14_phd_student_ratio") {
#         base_weights[j] <- runif(1, 0.5, 2.0)
#       }
#       
#       # ----- TEACHING: Individual preference -----
#       if (q == "q9_typical_teaching_load") {
#         base_weights[j] <- runif(1, 1.5, 3.5)  # Everyone cares
#       }
#       if (q == "q10_course_types") {
#         base_weights[j] <- runif(1, 0.5, 2.0)
#       }
#       
#       # ----- OTHER: Situational importance -----
#       if (q == "q5_dual_career") {
#         # Bimodal: either critical or irrelevant
#         base_weights[j] <- sample(c(runif(1, 0.2, 0.8), runif(1, 3.0, 5.0)), 
#                                   1, prob = c(0.55, 0.45))
#       }
#       if (q == "q11_mentoring_program") {
#         base_weights[j] <- runif(1, 0.3, 1.5)
#       }
#       if (q == "q15_medical_school_proximity") {
#         # Bimodal: field-dependent
#         base_weights[j] <- sample(c(runif(1, 0.1, 0.5), runif(1, 2.0, 3.5)), 
#                                   1, prob = c(0.70, 0.30))
#       }
#     }
#     
#     # Normalize weights to sum to 1
#     weight_list[[i]] <- base_weights / sum(base_weights)
#   }
#   
#   candidates$cand_weight_vector <- weight_list
#   
#   # Remove helper column and return
#   candidates %>% dplyr::select(-quality_pctl)
# }

generate_candidates_new <- function(n_candidates, questions, seed = NULL,
                                    quality_correlation = 0.7) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # candidates <- tibble(
  #   cand_id = 1:n_candidates,
  #   v_i1 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5),
  #   v_i2 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5),
  #   v_i3 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5)
  # ) %>%
  #   mutate(v_i_bar = (v_i1 * v_i2 * v_i3)^(1/3))
  
  candidates <- tibble(
    cand_id = 1:n_candidates,
    v_i1 = 0.7 * rbeta(n_candidates, 2, 4) + 0.3 * rbeta(n_candidates, 4, 1),
    v_i2 = 0.4 * rbeta(n_candidates, 2, 4) + 0.6 * rbeta(n_candidates, 4, 1),
    v_i3 = 0.8 * rbeta(n_candidates, 2, 4) + 0.2 * rbeta(n_candidates, 4, 1)
  ) %>%
    mutate(v_i_bar = (v_i1 * v_i2 * v_i3)^(1/3))
  
  
  candidates <- candidates %>%
    mutate(quality_pctl = percent_rank(v_i_bar))
  
  n <- n_candidates
  v_pctl <- candidates$quality_pctl
  qc <- quality_correlation
  
  # ==========================================================================
  # NUMERICAL QUESTIONS - MATCH DEPARTMENT DISTRIBUTIONS EXACTLY
  # ==========================================================================
  # Department ranges:
  # - Salary: [58k, 152k], mean=89k, strong tier gradient
  # - COL: [65k, 138k], mean=84k, moderate tier effect
  # - PhD ratio: [1.24, 3.79], mean=2.42, strong tier gradient
  
  # SALARY: Strong quality → salary expectation
  # Tier 1 depts: mean=134k, Tier 4: mean=73k (61k range)
  candidates$q_q6_typical_salary_range <- 58000 + 
    90000 * v_pctl^1.5 +            # Creates 58k→148k range
    rnorm(n, 0, 8000)               # ±8k noise
  candidates$q_q6_typical_salary_range <- pmax(58000, pmin(152000, candidates$q_q6_typical_salary_range))
  
  # COST OF LIVING: Moderate correlation
  # Higher quality candidates more willing to accept high COL (urban/prestigious areas)
  candidates$q_q4_cost_of_living <- 65000 + 
    65000 * v_pctl^1.2 +            # Creates 65k→130k range
    rnorm(n, 0, 9000)               # ±9k noise
  candidates$q_q4_cost_of_living <- pmax(65000, pmin(138000, candidates$q_q4_cost_of_living))
  
  # PHD RATIO: Strong correlation with research focus
  # Tier 1 depts: mean=3.44, Tier 4: mean=1.94 (1.5 range)
  candidates$q_q14_phd_student_ratio <- 1.24 + 
    2.3 * v_pctl^1.6 +              # Creates 1.24→3.54 range
    rnorm(n, 0, 0.35)               # ±0.35 noise
  candidates$q_q14_phd_student_ratio <- pmax(1.24, pmin(3.79, candidates$q_q14_phd_student_ratio))
  
  # ==========================================================================
  # CATEGORICAL QUESTIONS - MATCH TIER-STRATIFIED DISTRIBUTIONS
  # ==========================================================================
  
  blend_probs <- function(quality_probs, n_levels, qc) {
    uniform <- rep(1/n_levels, n_levels)
    blended <- qc * quality_probs + (1 - qc) * uniform
    blended <- pmax(blended, 0.05)
    blended / sum(blended)
  }
  
  # q1_geographic_setting: CRITICAL - Tier 1/2 are A/B, Tier 3/4 are C
  # A: Urban, B: Suburban, C: Small city, D: Rural
  candidates$q_q1_geographic_setting <- sapply(v_pctl, function(p) {
    # Very strong quality gradient to match tier stratification
    q_probs <- c(
      0.15 + 0.50*p,    # A: top candidates want urban
      0.20 + 0.20*p,    # B: top candidates ok with suburban
      0.55 - 0.60*p,    # C: lower candidates prefer small city
      0.10 - 0.10*p     # D: few want rural
    )
    probs <- blend_probs(q_probs, 4, qc)
    sample(c("A", "B", "C", "D"), 1, prob = probs)
  })
  
  # q2_region: Relatively even distribution, slight Northeast/West Coast preference for top
  candidates$q_q2_region <- sapply(v_pctl, function(p) {
    # Higher quality candidates are MORE geographically selective (fewer regions)
    n_reg <- sample(1:3, 1, prob = c(0.15 + 0.25*p, 0.50, 0.35 - 0.25*p))
    
    # Slight coastal bias for high quality
    q_probs <- c(
      "Northeast" = 0.21 + 0.10*p,
      "West Coast" = 0.19 + 0.08*p,
      "Midwest" = 0.30 - 0.08*p,
      "Southeast" = 0.19 - 0.05*p,
      "Southwest" = 0.11 - 0.05*p
    )
    probs <- blend_probs(q_probs, 5, qc)
    
    selected <- sample(names(probs), n_reg, prob = probs, replace = FALSE)
    paste(selected, collapse = ",")
  })
  
  # q3_airport_proximity: Not in your summary, use reasonable distribution
  candidates$q_q3_airport_proximity <- sapply(v_pctl, function(p) {
    q_probs <- c(0.35 + 0.25*p, 0.40 + 0.05*p, 0.20 - 0.25*p, 0.05 - 0.05*p)
    probs <- blend_probs(q_probs, 4, qc)
    sample(c("A", "B", "C", "D"), 1, prob = probs)
  })
  
  # q5_dual_career: Not in summary, life circumstances
  candidates$q_q5_dual_career <- sample(c("Y", "N"), n, replace = TRUE, 
                                        prob = c(0.30, 0.70))
  
  # q7_typical_startup: CRITICAL STRATIFICATION
  # Tier 1: 90% E, Tier 2: 100% D, Tier 3: mix C/B, Tier 4: 98% B
  candidates$q_q7_typical_startup <- sapply(v_pctl, function(p) {
    # Very strong gradient
    q_probs <- c(
      0.02 + 0.00*p,    # A: nobody wants lowest
      0.20 - 0.19*p,    # B: low quality ok with low startup
      0.28 - 0.23*p,    # C: mid quality
      0.10 + 0.05*p,    # D: upper-mid quality
      0.40 + 0.37*p     # E: top quality wants highest
    )
    probs <- blend_probs(q_probs, 5, qc)
    sample(c("A", "B", "C", "D", "E"), 1, prob = probs)
  })
  
  # q8_guaranteed_summer: CRITICAL STRATIFICATION
  # Tier 1: 80% B, Tier 2: 93% B, Tier 3: 100% C, Tier 4: 90% D
  candidates$q_q8_guaranteed_summer <- sapply(v_pctl, function(p) {
    # Very strong gradient
    q_probs <- c(
      0.05 + 0.15*p,    # A: top candidates want full support
      0.30 + 0.35*p,    # B: upper candidates want good support
      0.50 - 0.35*p,    # C: mid candidates accept partial
      0.15 - 0.15*p     # D: low candidates accept none
    )
    probs <- blend_probs(q_probs, 4, qc)
    sample(c("A", "B", "C", "D"), 1, prob = probs)
  })
  
  # q9_typical_teaching_load: CRITICAL STRATIFICATION
  # Tier 1: 100% B, Tier 2: 67% C, Tier 3: 84% C, Tier 4: 79% C/22% D
  candidates$q_q9_typical_teaching_load <- sapply(v_pctl, function(p) {
    # Strong gradient (no tier offers A)
    q_probs <- c(
      0.00,             # A: nobody gets 1-1
      0.15 + 0.60*p,    # B: top candidates want 2-2
      0.70 - 0.40*p,    # C: most get 2-3 or 3-2
      0.15 - 0.20*p     # D: low quality get 3-3
    )
    probs <- blend_probs(q_probs, 4, qc)
    sample(c("A", "B", "C", "D"), 1, prob = probs)
  })
  
  # q10_course_types: EXTREME STRATIFICATION
  # Tier 1: 100% A, Tier 2: 100% B, Tier 3: 72% B, Tier 4: 100% D
  candidates$q_q10_course_types <- sapply(v_pctl, function(p) {
    # Extreme gradient
    q_probs <- c(
      0.05 + 0.70*p,    # A: top candidates want grad courses
      0.45 + 0.15*p,    # B: upper want mix with grad emphasis
      0.00,             # C: nobody in middle (no depts offer this)
      0.50 - 0.85*p     # D: low quality teach undergrad
    )
    probs <- blend_probs(q_probs, 4, qc)
    sample(c("A", "B", "C", "D"), 1, prob = probs)
  })
  
  # q11_mentoring_program: Not in summary, use reasonable
  candidates$q_q11_mentoring_program <- sapply(v_pctl, function(p) {
    q_probs <- c(0.30 + 0.15*p, 0.35, 0.25 - 0.10*p, 0.10 - 0.05*p)
    probs <- blend_probs(q_probs, 4, qc)
    sample(c("A", "B", "C", "D"), 1, prob = probs)
  })
  
  # q12_research_culture: MODERATE STRATIFICATION
  # Tier 1: 50% D, Tier 2: 67% C, Tier 3: 80% C, Tier 4: 90% D
  candidates$q_q12_research_culture <- sapply(v_pctl, function(p) {
    q_probs <- c(
      0.02 + 0.08*p,    # A: few want independent
      0.08 + 0.02*p,    # B: 
      0.40 + 0.00*p,    # C: many depts offer moderate
      0.40 + 0.10*p,    # D: collaborative is common
      0.10 - 0.20*p     # E: very collaborative less common
    )
    probs <- blend_probs(q_probs, 5, qc)
    sample(c("A", "B", "C", "D", "E"), 1, prob = probs)
  })
  
  # q13_publication_venues: EXTREME STRATIFICATION
  # Tier 1: 60% A, Tier 2: 67% B, Tier 3: 80% B, Tier 4: 100% E
  candidates$q_q13_publication_venues <- sapply(v_pctl, function(p) {
    # Extreme gradient
    q_probs <- c(
      0.15 + 0.50*p,    # A: top candidates want top journals only
      0.45 + 0.15*p,    # B: upper want strong publication culture
      0.05 - 0.05*p,    # C: few in middle
      0.00,             # D: nobody (no depts offer)
      0.35 - 0.60*p     # E: low quality ok with teaching focus
    )
    probs <- blend_probs(q_probs, 5, qc)
    sample(c("A", "B", "C", "D", "E"), 1, prob = probs)
  })
  
  # q15_medical_school_proximity: Not strong pattern, slight preference
  candidates$q_q15_medical_school_proximity <- sapply(v_pctl, function(p) {
    q_probs <- c(0.55 - 0.15*p, 0.45 + 0.15*p)
    probs <- blend_probs(q_probs, 2, qc)
    sample(c("0", "1"), 1, prob = probs)
  })
  
  # ==========================================================================
  # CANDIDATE-SPECIFIC WEIGHT VECTORS
  # ==========================================================================
  num_q <- length(questions$numerical)
  cat_q <- length(questions$categorical)
  n_total_q <- num_q + cat_q
  q_names <- c(questions$numerical, names(questions$categorical))
  
  weight_list <- vector("list", n_candidates)
  
  for (i in 1:n_candidates) {
    p <- v_pctl[i]
    
    # More uniform base weights (less variance)
    base_weights <- runif(n_total_q, 0.8, 1.3)
    
    for (j in seq_along(q_names)) {
      q <- q_names[j]
      
      # CRITICAL FEATURES: Very high weights
      if (q == "q6_typical_salary_range") {
        base_weights[j] <- runif(1, 2.5, 4.0)
      }
      if (q == "q7_typical_startup") {
        base_weights[j] <- runif(1, 2.0, 3.5) + p * runif(1, 0, 1.0)
      }
      if (q == "q8_guaranteed_summer") {
        base_weights[j] <- runif(1, 2.5, 4.0)
      }
      if (q == "q9_typical_teaching_load") {
        base_weights[j] <- runif(1, 2.5, 4.0)
      }
      if (q == "q10_course_types") {
        base_weights[j] <- runif(1, 2.0, 3.5) + p * runif(1, 0, 1.5)
      }
      if (q == "q13_publication_venues") {
        base_weights[j] <- runif(1, 1.8, 3.0) + p * runif(1, 0, 1.5)
      }
      
      # LOCATION: Very important for many (life constraints)
      if (q == "q1_geographic_setting") {
        base_weights[j] <- runif(1, 2.0, 4.0)
      }
      if (q == "q2_region") {
        base_weights[j] <- runif(1, 3.0, 5.0)  # Most important for many
      }
      if (q == "q4_cost_of_living") {
        base_weights[j] <- runif(1, 1.8, 3.2)
      }
      
      # RESEARCH ENVIRONMENT: Important for research-focused
      if (q == "q14_phd_student_ratio") {
        base_weights[j] <- runif(1, 1.2, 2.5) + p * runif(1, 0, 1.2)
      }
      if (q == "q12_research_culture") {
        base_weights[j] <- runif(1, 1.0, 2.0)
      }
      
      # MODERATE IMPORTANCE
      if (q == "q3_airport_proximity") {
        base_weights[j] <- runif(1, 0.8, 2.0)
      }
      if (q == "q11_mentoring_program") {
        base_weights[j] <- runif(1, 0.6, 1.5)
      }
      
      # BIMODAL (critical for some, irrelevant for others)
      if (q == "q5_dual_career") {
        base_weights[j] <- sample(c(runif(1, 0.3, 0.9), runif(1, 4.0, 6.0)), 
                                  1, prob = c(0.65, 0.35))
      }
      if (q == "q15_medical_school_proximity") {
        base_weights[j] <- sample(c(runif(1, 0.3, 0.8), runif(1, 2.8, 4.5)), 
                                  1, prob = c(0.70, 0.30))
      }
    }
    
    # Normalize
    weight_list[[i]] <- base_weights / sum(base_weights)
  }
  
  candidates$cand_weight_vector <- weight_list
  
  candidates %>% dplyr::select(-quality_pctl)
}


# =============================================================================
# PREPARE DEPARTMENTS
# =============================================================================

# prepare_departments <- function(sampled_depts) {
#   sampled_depts %>%
#     mutate(
#       # h_j = case_when(
#       #   tier == "Tier 1" ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.2, 0.8)),
#       #   tier == "Tier 2" ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.2, 0.8)),
#       #   tier == "Tier 3" ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.4, 0.6)),
#       #   TRUE ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.4, 0.6))
#       # ),
#       h_j = case_when(
#         TRUE ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.5, 0.5))
#       ),
#       k_j = if_else(h_j == 1L, 5L, 10L),
#       
#       prestige_tier = tier,
#       # Convert ALL categorical columns to character
#       q1_geographic_setting = as.character(q1_geographic_setting),
#       q2_region = as.character(q2_region),
#       q3_airport_proximity = as.character(q3_airport_proximity),
#       q5_dual_career = as.character(q5_dual_career),
#       q7_typical_startup = as.character(q7_typical_startup),
#       q8_guaranteed_summer = as.character(q8_guaranteed_summer),
#       q9_typical_teaching_load = as.character(q9_typical_teaching_load),
#       q10_course_types = as.character(q10_course_types),
#       q11_mentoring_program = as.character(q11_mentoring_program),
#       q12_research_culture = as.character(q12_research_culture),
#       q13_publication_venues = as.character(q13_publication_venues),
#       # CRITICAL: Convert integer to character for binary variable
#       q15_medical_school_proximity = as.character(q15_medical_school_proximity)
#     )
# }
prepare_departments <- function(sampled_depts, questions, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  num_q <- length(questions$numerical)
  cat_q <- length(questions$categorical)
  n_total_q <- num_q + cat_q
  
  result <- sampled_depts %>%
    mutate(
      # h_j = case_when(
      #   tier == "Tier 1" ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.2, 0.8)),
      #   tier == "Tier 2" ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.2, 0.8)),
      #   tier == "Tier 3" ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.4, 0.6)),
      #   TRUE ~ sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.4, 0.6))
      # ),
      # #h_j = sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.5, 0.5)),
      # k_j = if_else(h_j == 1L, 5L, 10L),
      prestige_tier = tier,
      # Ensure s_j has a floor
      #s_j = pmax(s_j, 0.30),
      # Convert all categorical columns to character
      q1_geographic_setting = as.character(q1_geographic_setting),
      q2_region = as.character(q2_region),
      q3_airport_proximity = as.character(q3_airport_proximity),
      q5_dual_career = as.character(q5_dual_career),
      q7_typical_startup = as.character(q7_typical_startup),
      q8_guaranteed_summer = as.character(q8_guaranteed_summer),
      q9_typical_teaching_load = as.character(q9_typical_teaching_load),
      q10_course_types = as.character(q10_course_types),
      q11_mentoring_program = as.character(q11_mentoring_program),
      q12_research_culture = as.character(q12_research_culture),
      q13_publication_venues = as.character(q13_publication_venues),
      q15_medical_school_proximity = as.character(q15_medical_school_proximity)
    )
  
  # Generate department-specific weight vectors
  n_depts <- nrow(result)
  weight_list <- vector("list", n_depts)
  
  # Question names in order (numerical first, then categorical)
  q_names <- c(questions$numerical, names(questions$categorical))
  
  for (j in 1:n_depts) {
    dept_tier <- result$tier[j]
    
    # Base weights - start with slight variation around equal
    base_weights <- rep(1, n_total_q)
    
    # Department type determines preference structure
    # Type 1: Balanced (cares about many things equally)
    # Type 2: Location-focused (region, geography, airport matter most)
    # Type 3: Research-focused (PhD ratio, research culture, publications matter most)
    # Type 4: Compensation-focused (salary, startup, summer support matter most)
    
    dept_type <- sample(1:4, 1, prob = c(0.25, 0.25, 0.30, 0.20))
    
    if (dept_type == 1) {
      # Balanced: small random variations
      base_weights <- runif(n_total_q, 0.7, 1.3)
      
    } else if (dept_type == 2) {
      # Location-focused
      for (i in seq_along(q_names)) {
        if (q_names[i] %in% c("q1_geographic_setting", "q2_region", "q3_airport_proximity")) {
          base_weights[i] <- runif(1, 2.0, 4.0)
        } else if (q_names[i] == "q4_cost_of_living") {
          base_weights[i] <- runif(1, 1.5, 2.5)
        }
      }
      
    } else if (dept_type == 3) {
      # Research-focused (more common for higher tier departments)
      for (i in seq_along(q_names)) {
        if (q_names[i] %in% c("q14_phd_student_ratio", "q12_research_culture", 
                              "q13_publication_venues")) {
          base_weights[i] <- runif(1, 2.5, 4.5)
        } else if (q_names[i] %in% c("q9_typical_teaching_load", "q10_course_types")) {
          base_weights[i] <- runif(1, 1.5, 2.5)
        }
      }
      
    } else if (dept_type == 4) {
      # Compensation-focused
      for (i in seq_along(q_names)) {
        if (q_names[i] %in% c("q6_typical_salary_range", "q7_typical_startup", 
                              "q8_guaranteed_summer")) {
          base_weights[i] <- runif(1, 2.0, 4.0)
        }
      }
    }
    
    # Tier-based adjustments
    # Higher tier departments care more about research metrics
    if (dept_tier == "Tier 1") {
      for (i in seq_along(q_names)) {
        if (q_names[i] %in% c("q13_publication_venues", "q12_research_culture")) {
          base_weights[i] <- base_weights[i] * runif(1, 1.3, 1.8)
        }
      }
    }
    
    # Lower tier departments may care more about location/cost factors
    if (dept_tier == "Tier 4") {
      for (i in seq_along(q_names)) {
        if (q_names[i] %in% c("q1_geographic_setting", "q4_cost_of_living")) {
          base_weights[i] <- base_weights[i] * runif(1, 1.2, 1.6)
        }
      }
    }
    
    # Normalize to sum to 1
    weight_list[[j]] <- base_weights / sum(base_weights)
  }
  
  result$weight_vector <- weight_list
  
  # Print summary of weight distributions
  cat("\n=== DEPARTMENT WEIGHT SUMMARY ===\n")
  weight_matrix <- do.call(rbind, weight_list)
  colnames(weight_matrix) <- q_names
  
  cat("Mean weights by question:\n")
  print(round(colMeans(weight_matrix), 3))
  
  cat("\nWeight range (min-max) by question:\n")
  for (i in 1:ncol(weight_matrix)) {
    cat(sprintf("  %s: [%.3f, %.3f]\n", 
                q_names[i], min(weight_matrix[,i]), max(weight_matrix[,i])))
  }
  
  result
}

#' Generate consistent yearly hiring schedule for all departments
#' @param n_departments Number of departments
#' @param n_years Number of simulation years
#' @param departments Data frame with department info (needs prestige_tier)
#' @param seed Random seed for reproducibility
#' @return Matrix of h_j values (n_departments x n_years), each entry is 0 or 1
generate_yearly_hiring_schedule <- function(n_departments, n_years, departments, seed = 123) {
  set.seed(seed)
  
  # Hiring probability by tier (higher tier = more likely to hire each year)
  # These can be tuned based on realistic hiring patterns
  hire_prob_by_tier <- c(
    "Tier 1" = 0.6,   # Top departments hire most years
    "Tier 2" = 0.6,
    "Tier 3" = 0.6,
    "Tier 4" = 0.6    # Lower tier departments hire less frequently
  )
  
  # Create hiring schedule matrix
  hiring_schedule <- matrix(0L, nrow = n_departments, ncol = n_years)
  
  for (j in 1:n_departments) {
    tier <- as.character(departments$prestige_tier[j])
    prob <- hire_prob_by_tier[[tier]]
    if (is.null(prob)) prob <- 0.5  # Default if tier not found
    
    # Each year is independent Bernoulli draw
    hiring_schedule[j, ] <- rbinom(n_years, size = 1, prob = prob)
  }
  
  # Print summary
  cat("\n=== YEARLY HIRING SCHEDULE SUMMARY ===\n")
  cat("Total department-years:", n_departments * n_years, "\n")
  cat("Total hiring events:", sum(hiring_schedule), "\n")
  cat("Overall hiring rate:", round(mean(hiring_schedule), 3), "\n\n")
  
  for (tier in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
    tier_idx <- which(departments$prestige_tier == tier)
    if (length(tier_idx) > 0) {
      tier_rate <- mean(hiring_schedule[tier_idx, ])
      cat(tier, ": ", sum(hiring_schedule[tier_idx, ]), " hires over ", 
          length(tier_idx) * n_years, " dept-years (rate = ", 
          round(tier_rate, 3), ")\n", sep = "")
    }
  }
  
  hiring_schedule
}

# =============================================================================
# NEURAL NETWORK MODEL
# =============================================================================

acceptance_net <- nn_module(
  "AcceptancePredictionNet",
  initialize = function(n_cont_features, embedding_specs, hidden_dims = c(16, 8)) {
    self$n_cont <- n_cont_features
    self$embedding_specs <- embedding_specs
    self$embeddings <- nn_module_list()
    total_embed_dim <- 0
    if (length(embedding_specs) > 0) {
      for (i in seq_along(embedding_specs)) {
        spec <- embedding_specs[[i]]
        self$embeddings$append(nn_embedding(spec$n_levels, spec$embed_dim))
        total_embed_dim <- total_embed_dim + spec$embed_dim
      }
    }
    input_dim <- n_cont_features + total_embed_dim
    self$layers <- nn_module_list()
    self$batch_norms <- nn_module_list()
    self$dropouts <- nn_module_list()
    prev_dim <- input_dim
    for (hidden_dim in hidden_dims) {
      self$layers$append(nn_linear(prev_dim, hidden_dim))
      self$batch_norms$append(nn_batch_norm1d(hidden_dim, momentum = 0.005))
      self$dropouts$append(nn_dropout(0.4))
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
      h <- self$layers[[i]](h)
      h <- self$batch_norms[[i]](h)
      h <- torch_relu(h)
      h <- self$dropouts[[i]](h)
    }
    self$output(h)
  }
)

# =============================================================================
# FIXED prepare_nn_data FUNCTION
# =============================================================================

prepare_nn_data <- function(data, questions, include_fit = TRUE, include_questions = TRUE) {
  cont_features <- c("s_j")
  
  # Add v_i_bar
  if ("v_i_bar" %in% colnames(data)) {
    data$log_v_i_bar <- log(data$v_i_bar + 1e-6)
    cont_features <- c(cont_features, "log_v_i_bar")
  }
  
  if (include_fit && "f_j" %in% colnames(data)) {
    data$log_f_j <- log(data$f_j + 1e-6)
    cont_features <- c(cont_features, "log_f_j")
  }
  
  # Department numericals (without prefix)
  for (q in questions$numerical) {
    if (q %in% colnames(data)) {
      cont_features <- c(cont_features, q)
    }
  }
  
  # Candidate numericals (with q_ prefix)
  if (include_questions) {
    for (q in questions$numerical) {
      qcol <- paste0("q_", q)
      if (qcol %in% colnames(data)) {
        cont_features <- c(cont_features, qcol)
      }
    }
  }
  
  # Build categorical features using safe indexing
  x_cat_list <- list()
  embedding_specs <- list()
  cat_i <- 0L
  
  if (include_questions && length(questions$categorical) > 0) {
    for (q_name in names(questions$categorical)) {
      levels <- questions$categorical[[q_name]]
      if (is.null(levels) || length(levels) == 0) {
        levels <- c("unknown")
      }
      n_levels <- length(levels)
      embed_dim <- min(8, max(2, ceiling(n_levels/2)))
      
      # Candidate categorical (with q_ prefix)
      qcol <- paste0("q_", q_name)
      if (qcol %in% colnames(data)) {
        cat_i <- cat_i + 1L
        
        raw_values <- data[[qcol]]
        
        # Special handling for q2_region (multi-select) - take first region
        if (q_name == "q2_region") {
          values <- sapply(raw_values, function(x) {
            if (is.na(x) || !is.character(x) || x == "") return(levels[1])
            parts <- strsplit(as.character(x), ",")[[1]]
            if (length(parts) == 0) return(levels[1])
            trimws(parts[1])
          })
        } else {
          values <- raw_values
        }
        
        indices <- safe_categorical_to_index(values, levels, paste0("cand_", q_name))
        x_cat_list[[cat_i]] <- indices
        embedding_specs[[cat_i]] <- list(n_levels = n_levels, embed_dim = embed_dim)
      }
      
      # Department categorical (without prefix)
      if (q_name %in% colnames(data)) {
        cat_i <- cat_i + 1L
        
        values <- data[[q_name]]
        indices <- safe_categorical_to_index(values, levels, paste0("dept_", q_name))
        x_cat_list[[cat_i]] <- indices
        embedding_specs[[cat_i]] <- list(n_levels = n_levels, embed_dim = embed_dim)
      }
    }
  }
  
  # Validate continuous features
  missing_cont <- setdiff(cont_features, colnames(data))
  if (length(missing_cont) > 0) {
    cat("Warning: Missing continuous features:", paste(missing_cont, collapse = ", "), "\n")
    cont_features <- intersect(cont_features, colnames(data))
  }
  
  # Create continuous tensor
  cont_matrix <- as.matrix(data[, cont_features, drop = FALSE])
  # Replace any NA with column means
  for (col in 1:ncol(cont_matrix)) {
    na_mask <- is.na(cont_matrix[, col])
    if (any(na_mask)) {
      col_mean <- mean(cont_matrix[, col], na.rm = TRUE)
      if (is.na(col_mean)) col_mean <- 0
      cont_matrix[na_mask, col] <- col_mean
    }
  }
  x_cont_tensor <- torch_tensor(cont_matrix, dtype = torch_float())
  
  # Create categorical tensors with explicit validation
  x_cat_tensors <- list()
  for (i in seq_along(x_cat_list)) {
    idx_vec <- x_cat_list[[i]]
    n_lvl <- embedding_specs[[i]]$n_levels
    
    # Final safety check
    if (any(idx_vec < 1 | idx_vec > n_lvl, na.rm = TRUE)) {
      cat("CRITICAL: Clamping out-of-range indices in cat variable", i, "\n")
      idx_vec <- pmax(1L, pmin(idx_vec, n_lvl))
    }
    
    x_cat_tensors[[i]] <- torch_tensor(as.integer(idx_vec), dtype = torch_long())
  }
  
  list(
    x_cont = x_cont_tensor, 
    x_cat = x_cat_tensors,
    embedding_specs = embedding_specs, 
    n_cont_features = ncol(x_cont_tensor)
  )
}

# =============================================================================
# INITIALIZE DEPARTMENT MODEL
# =============================================================================

initialize_department_model <- function(questions, include_fit = TRUE, include_questions = TRUE) {
  embedding_specs <- list()
  
  if (include_questions && length(questions$categorical) > 0) {
    for (q_name in names(questions$categorical)) {
      levels <- questions$categorical[[q_name]]
      if (is.null(levels) || length(levels) == 0) levels <- c("unknown")
      n_levels <- length(levels)
      embed_dim <- min(8, max(2, ceiling(n_levels/2)))
      
      # Two embedding specs per categorical (candidate + department)
      embedding_specs[[length(embedding_specs) + 1]] <- list(n_levels = n_levels, embed_dim = embed_dim)
      embedding_specs[[length(embedding_specs) + 1]] <- list(n_levels = n_levels, embed_dim = embed_dim)
    }
  }
  
  # Count continuous features: s_j + log_v_i_bar + log_f_j + numericals
  n_cont <- 1L + 1L + as.integer(include_fit)
  if (include_questions) {
    n_cont <- n_cont + length(questions$numerical) * 2
  }
  
  model <- acceptance_net(n_cont, embedding_specs)
  list(
    model = model,
    optimizer = optim_adam(model$parameters, lr = 0.001),
    historical_data = tibble(),
    questions = questions,
    is_trained = FALSE,
    include_fit = include_fit,
    include_questions = include_questions
  )
}

# =============================================================================
# TRAIN DEPARTMENT MODEL
# =============================================================================

train_department_model <- function(dept_model, train_data, n_epochs = 60,
                                   include_fit = dept_model$include_fit,
                                   include_questions = dept_model$include_questions, 
                                   seed = NULL) {
  train_data <- dplyr::filter(train_data, offered == 1L)
  if (nrow(train_data) < 1) return(dept_model)
  
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }
  
  nn_data <- prepare_nn_data(train_data, dept_model$questions,
                             include_fit = include_fit, 
                             include_questions = include_questions)
  y_tensor <- torch::torch_tensor(train_data$accepted, dtype = torch::torch_float())$unsqueeze(2)
  
  n_train <- nrow(train_data)
  use_validation <- n_train >= 40
  
  if (use_validation) {
    val_idx <- sample.int(n_train, size = ceiling(0.2 * n_train))
    train_idx <- setdiff(seq_len(n_train), val_idx)
    
    x_train <- nn_data$x_cont[train_idx, ]
    y_train <- y_tensor[train_idx, ]
    x_val   <- nn_data$x_cont[val_idx, ]
    y_val   <- y_tensor[val_idx, ]
    
    x_cat_train <- lapply(nn_data$x_cat, function(t) t[train_idx])
    x_cat_val   <- lapply(nn_data$x_cat, function(t) t[val_idx])
  } else {
    x_train <- nn_data$x_cont
    y_train <- y_tensor
    x_cat_train <- nn_data$x_cat
  }
  
  optimizer <- optim_adam(dept_model$model$parameters, lr = 0.002)
  scheduler <- lr_multiplicative(optimizer, lr_lambda = function(epoch) 0.98)
  
  best_val_loss <- Inf
  patience <- 20
  patience_counter <- 0
  best_state <- NULL
  
  dept_model$model$train()
  
  for (epoch in 1:n_epochs) {
    log_odds <- dept_model$model(x_train, x_cat_train)
    
    bce_loss <- nn_bce_with_logits_loss(reduction = "none")(log_odds, y_train)
    probs <- torch_sigmoid(log_odds)
    focal_weight <- torch_pow(1 - probs * y_train - (1 - probs) * (1 - y_train), 2)
    loss <- torch_mean(focal_weight * bce_loss)
    
    l2_loss <- 0
    for (param in dept_model$model$parameters) {
      l2_loss <- l2_loss + torch_sum(param^2)
    }
    
    l2_lambda <- 0.01 * exp(-epoch / 100)
    total_loss <- loss + l2_lambda * l2_loss
    
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
        best_val_loss <- val_loss
        patience_counter <- 0
        best_state <- lapply(dept_model$model$parameters, function(p) p$clone()$detach())
      } else {
        patience_counter <- patience_counter + 1
        if (patience_counter >= patience) {
          if (!is.null(best_state)) {
            param_list <- dept_model$model$parameters
            for (i in seq_along(param_list)) {
              param_list[[i]]$data <- best_state[[i]]$data
            }
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
# PREDICT ACCEPTANCE PROBABILITY
# =============================================================================

predict_acceptance_probability <- function(dept_model, applicant_data, 
                                           n_bootstrap = 200,
                                           include_fit = dept_model$include_fit,
                                           include_questions = dept_model$include_questions,
                                           seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }
  
  # Fallback if model not trained
  if (!dept_model$is_trained || nrow(applicant_data) == 0) {
    if (include_fit) {
      base <- applicant_data %>% mutate(
        pi_pred = plogis(
          qlogis(0.35) +
            0.8 * log(pmax(s_j, 1e-6)) +
            0.7 * log(pmax(f_j, 1e-6)) +
            0.5 * v_i_bar * s_j -
            0.8 * v_i_bar * (1 - s_j) +
            rnorm(n(), 0, 0.35)
        )
      )
    } else {
      base <- applicant_data %>% mutate(
        pi_pred = plogis(
          qlogis(0.28) +
            1.0 * log(pmax(s_j, 1e-6)) +
            0.5 * v_i_bar * (1 - s_j) +
            rnorm(n(), 0, 0.40)
        )
      )
    }
    
    clip <- function(x, lo = 1e-5, hi = 1 - 1e-5) pmin(pmax(x, lo), hi)
    mu  <- clip(base$pi_pred)
    draws <- matrix(mu, nrow = 1)
    res <- base %>% mutate(pi_var = 0)
    attr(res, "pi_draws") <- draws
    return(res)
  }
  
  hist_data <- dept_model$historical_data %>% filter(offered == 1L)
  if (nrow(hist_data) < 10) {
    nn_data <- prepare_nn_data(applicant_data, dept_model$questions,
                               include_fit = include_fit,
                               include_questions = include_questions)
    dept_model$model$eval()
    with_no_grad({
      log_odds <- dept_model$model(nn_data$x_cont, nn_data$x_cat)
      pi_mean  <- as.numeric(torch_sigmoid(log_odds)$squeeze()$to(device = "cpu"))
    })
    pi_mean <- pmin(pmax(pi_mean, 1e-5), 1 - 1e-5)
    res <- applicant_data %>% mutate(pi_pred = pi_mean, pi_var = 0)
    attr(res, "pi_draws") <- matrix(pi_mean, nrow = 1)
    return(res)
  }
  
  n_hist <- nrow(hist_data)
  pi_draws_matrix <- matrix(NA_real_, nrow = n_bootstrap, ncol = nrow(applicant_data))
  
  for (b in 1:n_bootstrap) {
    boot_idx <- sample.int(n_hist, size = n_hist, replace = TRUE)
    boot_data <- hist_data[boot_idx, ]
    
    temp_model <- initialize_department_model(dept_model$questions, 
                                              include_fit, include_questions)
    temp_model$historical_data <- boot_data
    
    temp_model <- train_department_model(temp_model, boot_data, 
                                         n_epochs = 30, 
                                         include_fit = include_fit,
                                         include_questions = include_questions,
                                         seed = seed + b)
    
    nn_data <- prepare_nn_data(applicant_data, temp_model$questions,
                               include_fit = include_fit,
                               include_questions = include_questions)
    temp_model$model$eval()
    with_no_grad({
      log_odds <- temp_model$model(nn_data$x_cont, nn_data$x_cat)
      pi_b <- as.numeric(torch_sigmoid(log_odds)$squeeze()$to(device = "cpu"))
    })
    pi_draws_matrix[b, ] <- pmin(pmax(pi_b, 1e-5), 1 - 1e-5)
  }
  
  pi_mean <- colMeans(pi_draws_matrix)
  pi_var <- apply(pi_draws_matrix, 2, var)
  
  res <- applicant_data %>%
    mutate(
      pi_pred = pi_mean,
      pi_var  = pi_var
    )
  
  attr(res, "pi_draws") <- pi_draws_matrix
  res
}

# =============================================================================
# PAIRWISE RANKING FUNCTIONS (unchanged from original)
# =============================================================================

make_repeated_rank_draws <- function(applicant_data,
                                     L = 200,
                                     tuple_size = NULL,
                                     method = c("bootstrap","gumbel","gaussian"),
                                     noise_scale = 0.15,
                                     seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  method <- match.arg(method)
  n <- nrow(applicant_data)
  
  if (!"U_det" %in% names(applicant_data)) {
    applicant_data <- applicant_data %>%
      mutate(U_det = exp(s_j * log(v_i_bar + 1e-8) + (1 - s_j) * log(f_j + 1e-8)))
  }
  
  mu <- pmin(pmax(applicant_data$pi_pred, 1e-5), 1 - 1e-5)
  Uhat_full <- applicant_data$U_det * mu
  
  if (is.null(tuple_size)) {
    idx <- seq_len(n)
  } else {
    M_use <- min(tuple_size, n)
    idx <- order(Uhat_full, decreasing = TRUE)[seq_len(M_use)]
  }
  M <- length(idx)
  U_hat <- Uhat_full[idx]
  
  if (method == "bootstrap") {
    if (!"U_true" %in% names(applicant_data)) {
      applicant_data <- applicant_data %>%
        mutate(
          dept_tier = dept_tier %||% 4L,
          cand_tier = cand_tier %||% 4L,
          U_true = true_utility(
            s_j       = s_j,
            v_i_bar   = v_i_bar,
            f_j       = f_j,
            dept_tier = dept_tier,
            cand_tier = cand_tier
          )
        )
    }
    
    U_true_sub <- applicant_data$U_true[idx]
    resid      <- U_true_sub - U_hat
    
    if (all(!is.finite(resid)) || sd(resid, na.rm = TRUE) == 0) {
      U_draws <- matrix(rep(U_hat, each = L), nrow = L, ncol = M)
      return(list(U_draws = U_draws, idx = idx, U_hat = U_hat))
    }
    
    U_draws <- matrix(NA_real_, nrow = L, ncol = M)
    for (b in 1:L) {
      eps_b <- sample(resid, size = M, replace = TRUE)
      U_b   <- pmax(U_hat + eps_b, 0)
      U_draws[b, ] <- U_b
    }
    
    return(list(U_draws = U_draws, idx = idx, U_hat = U_hat))
  }
  
  if (method == "gumbel") {
    base <- matrix(rep(U_hat, each = L), nrow = L)
    gumb <- -log(-log(matrix(runif(L * M), nrow = L, ncol = M)))
    U_draws <- base + noise_scale * gumb
  } else if (method == "gaussian") {
    base <- matrix(rep(U_hat, each = L), nrow = L, ncol = M)
    U_draws <- base + noise_scale * matrix(rnorm(L * M), nrow = L, ncol = M)
    U_draws[U_draws < 0] <- 0
  }
  
  list(U_draws = U_draws, idx = idx, U_hat = U_hat)
}

compute_c_alpha_from_draws <- function(U_hat, U_draws, alpha = 0.05, ridge = 0.01) {
  stopifnot(is.numeric(U_hat), is.matrix(U_draws))
  n <- length(U_hat); B <- nrow(U_draws)
  if (n <= 1L || B <= 1L) return(list(c_alpha = 0, sigma_hat = matrix(0, n, n)))
  Zb <- rep(-Inf, B)
  sigma_hat <- matrix(NA_real_, n, n)
  for (i in 1:(n-1)) {
    for (l in (i+1):n) {
      delta_hat <- U_hat[i] - U_hat[l]
      delta_b   <- U_draws[, i] - U_draws[, l]
      s <- stats::sd(delta_b - delta_hat, na.rm = T)
      if (is.na(s) || !is.finite(s) || s < ridge) s <- ridge
      sigma_hat[i, l] <- sigma_hat[l, i] <- max(s, ridge)
      z_pair <- (delta_b - delta_hat) / s
      z_pair[!is.finite(z_pair)] <- -Inf
      Zb <- pmax(Zb, z_pair)
    }
  }
  c_alpha <- stats::quantile(Zb, probs = 1 - alpha, na.rm = TRUE, names = FALSE)
  list(c_alpha = as.numeric(c_alpha), sigma_hat = sigma_hat)
}

compute_pairwise_lower_ranks <- function(applicant_data,
                                         L_repeats   = 200,
                                         tuple_size  = NULL,
                                         noise_method = c("bootstrap","gumbel","gaussian"),
                                         noise_scale  = 0.15,
                                         alpha        = 0.05,
                                         seed         = NULL) {
  noise_method <- match.arg(noise_method)
  
  if (!"U_det" %in% names(applicant_data) || !"U_hat" %in% names(applicant_data)) {
    applicant_data <- applicant_data %>%
      mutate(
        U_det = exp(s_j * log(v_i_bar + 1e-8) + (1 - s_j) * log(f_j + 1e-8)),
        U_hat = U_det * pmin(pmax(pi_pred, 1e-5), 1 - 1e-5)
      )
  }
  
  repack  <- make_repeated_rank_draws(
    applicant_data, L = L_repeats, tuple_size = tuple_size,
    method = noise_method, noise_scale = noise_scale, seed = seed
  )
  idx      <- repack$idx
  U_draws  <- repack$U_draws
  U_hat    <- repack$U_hat
  cand_ids <- applicant_data$cand_id[idx]
  M <- length(U_hat)
  
  cal <- compute_c_alpha_from_draws(U_hat, U_draws, alpha = alpha, ridge = 0.0001)
  c_alpha   <- cal$c_alpha
  sigma_hat <- cal$sigma_hat
  
  rank_lower  <- integer(M)
  wins_cert   <- integer(M)
  losses_cert <- integer(M)
  
  for (i in 1:M) {
    rl <- 1L; Ui <- U_hat[i]
    for (l in 1:M) if (l != i) {
      s_il <- sigma_hat[l, i]
      if (is.finite(s_il) && s_il > 0) {
        z_li <- (U_hat[l] - Ui) / s_il
        if (z_li >  c_alpha) {
          rl             <- rl + 1L
          losses_cert[i] <- losses_cert[i] + 1L
        }
        if (z_li < -c_alpha) {
          wins_cert[i] <- wins_cert[i] + 1L
        }
      }
    }
    rank_lower[i] <- rl
  }
  pair_score <- wins_cert - losses_cert
  
  R <- t(apply(-U_draws, 1, rank, ties.method = "average"))
  mean_rank <- colMeans(R)
  
  tibble(
    cand_id    = cand_ids,
    rank_lower = rank_lower,
    pair_score = pair_score,
    mean_rank  = mean_rank
  ) %>%
    right_join(applicant_data %>% dplyr::select(cand_id), by = "cand_id") %>%
    mutate(
      rank_lower = ifelse(is.na(rank_lower), Inf, rank_lower),
      mean_rank  = ifelse(is.na(mean_rank),  Inf, mean_rank)
    )
}

select_interviews_sure_screening <- function(applicant_data,
                                             k_j,
                                             h_j,
                                             alpha = 0.05,
                                             L_repeats = 200,
                                             tuple_size = NULL,
                                             noise_method = c("bootstrap","gumbel","gaussian"),
                                             noise_scale = 0.15,
                                             seed = NULL,
                                             tier_width = 0.1) {
  noise_method <- match.arg(noise_method)
  n <- nrow(applicant_data)
  if (n == 0L) return(integer())
  if (n <= k_j) return(applicant_data$cand_id)
  
  if (!"U_det" %in% names(applicant_data) || !"U_hat" %in% names(applicant_data)) {
    applicant_data <- applicant_data %>%
      dplyr::mutate(
        U_det = exp(s_j * log(v_i_bar + 1e-8) + (1 - s_j) * log(f_j + 1e-8)),
        U_hat = U_det * pmin(pmax(pi_pred, 1e-5), 1 - 1e-5)
      )
  }
  
  rank_tbl <- compute_pairwise_lower_ranks(
    applicant_data, L_repeats, tuple_size, noise_method, noise_scale, alpha, seed
  )
  
  applicant_data <- applicant_data %>%
    dplyr::select(-dplyr::any_of(c("rank_lower", "pair_score", "mean_rank"))) %>%
    dplyr::left_join(
      rank_tbl %>% dplyr::select(cand_id, rank_lower, pair_score, mean_rank),
      by = "cand_id"
    )
  
  target_m <- k_j
  sure_idx <- which(applicant_data$rank_lower <= target_m)
  
  if (length(sure_idx) >= k_j) {
    selected_ids <- applicant_data %>%
      dplyr::slice(sure_idx) %>%
      dplyr::arrange(mean_rank, dplyr::desc(pair_score), dplyr::desc(U_hat)) %>%
      dplyr::slice_head(n = k_j) %>%
      dplyr::pull(cand_id)
  } else {
    sure_ids <- applicant_data$cand_id[sure_idx]
    remaining_slots <- k_j - length(sure_idx)
    
    remaining_pool <- applicant_data %>%
      dplyr::filter(!(cand_id %in% sure_ids))
    
    if (nrow(remaining_pool) == 0) {
      selected_ids <- sure_ids
    } else {
      sure_U_hat <- applicant_data %>%
        dplyr::filter(cand_id %in% sure_ids) %>%
        dplyr::pull(U_hat)
      
      if (length(sure_U_hat) > 0) {
        tier_threshold <- min(sure_U_hat, na.rm = TRUE) - tier_width
        tier_candidates <- remaining_pool %>%
          dplyr::filter(U_hat >= tier_threshold)
        
        if (nrow(tier_candidates) > 0) {
          weights <- exp((tier_candidates$U_hat - mean(tier_candidates$U_hat, na.rm = TRUE)) / 0.1)
          weights <- weights / sum(weights)
          
          n_to_sample <- min(remaining_slots, nrow(tier_candidates))
          sampled_ids <- sample(tier_candidates$cand_id, size = n_to_sample,
                                prob = weights, replace = FALSE)
          additional_ids <- sampled_ids
        } else {
          additional_ids <- remaining_pool %>%
            dplyr::arrange(dplyr::desc(U_hat)) %>%
            dplyr::slice_head(n = remaining_slots) %>%
            dplyr::pull(cand_id)
        }
      } else {
        additional_ids <- remaining_pool %>%
          dplyr::arrange(dplyr::desc(U_hat)) %>%
          dplyr::slice_head(n = remaining_slots) %>%
          dplyr::pull(cand_id)
      }
      
      selected_ids <- c(sure_ids, additional_ids)
    }
  }
  
  sure_ids <- applicant_data$cand_id[sure_idx]
  attr(selected_ids, "sure_ids") <- sure_ids
  
  selected_ids
}

# =============================================================================
# APPLICATIONS AND RESOLUTION FUNCTIONS
# =============================================================================

generate_applications <- function(candidates, departments, questions, ...) {
  tibble(cand_id = candidates$cand_id) %>%
    tidyr::crossing(tibble(dept_id = departments$dept_id))
}

resolve_accept_one <- function(results_tbl, departments, questions,
                               seed = NULL, temperature = 0.3, noise_sd = 0.4) {
  if (!is.null(seed)) set.seed(seed)
  if (!nrow(results_tbl)) {
    return(results_tbl %>% dplyr::mutate(accepted = integer()))
  }
  
  # Calculate candidate-side fit for each offer using a loop (more reliable than rowwise)
  f_j_candidate_values <- numeric(nrow(results_tbl))
  
  for (i in 1:nrow(results_tbl)) {
    row_data <- results_tbl[i, ]
    dept_row <- departments[departments$dept_id == row_data$dept_id, ]
    
    if (nrow(dept_row) == 0) {
      # Fallback if department not found
      f_j_candidate_values[i] <- row_data$f_j
    } else {
      f_j_candidate_values[i] <- calculate_candidate_f_j(
        candidate_row = row_data,
        department_row = dept_row,
        questions = questions
      )
    }
  }
  
  results_tbl$f_j_candidate <- f_j_candidate_values
  
  # Use candidate-side f_j for utility
  softmax_safe <- function(x, temp = 1) {
    x <- as.numeric(x)
    if (length(x) == 0L) return(numeric())
    if (all(!is.finite(x))) return(rep(1 / length(x), length(x)))
    x[!is.finite(x)] <- min(x[is.finite(x)], na.rm = TRUE) - 10
    if (!is.finite(temp) || temp <= 0) temp <- 1
    z  <- (x - max(x, na.rm = TRUE)) / temp
    z  <- pmax(pmin(z, 700), -700)
    ez <- exp(z)
    s  <- sum(ez)
    if (!is.finite(s) || s <= 0) return(rep(1 / length(x), length(x)))
    p <- ez / s
    if (any(!is.finite(p)) || any(p < 0)) p <- rep(1 / length(x), length(p))
    p
  }
  
  results_tbl %>%
    dplyr::mutate(
      # Use candidate-side f_j for their utility calculation
      cand_util = exp(v_i_bar * log(s_j + 1e-8) + (1 - v_i_bar) * log(f_j_candidate + 1e-8)),
      accepted  = 0L
    ) %>%
    dplyr::group_by(strategy, year, cand_id) %>%
    dplyr::group_modify(function(df, keys) {
      rows <- which(df$offered == 1L)
      if (length(rows) == 0L) {
        df$accepted <- 0L
        return(df)
      }
      if (length(rows) == 1L) {
        acc <- integer(nrow(df))
        acc[rows] <- 1L
        df$accepted <- acc
        return(df)
      }
      wu <- df$cand_util[rows]
      log_u <- log(pmax(wu, 1e-12))
      log_u_noisy <- log_u + rnorm(length(rows), mean = 0, sd = noise_sd)
      probs <- softmax_safe(log_u_noisy, temp = temperature)
      pick  <- sample(rows, size = 1, replace = FALSE, prob = probs)
      acc <- integer(nrow(df))
      acc[pick] <- 1L
      df$accepted <- acc
      df
    }) %>%
    dplyr::ungroup()
}

# resolve_accept_one <- function(results_tbl, departments, questions,
#                                seed = NULL, temperature = 0.3, noise_sd = 0.4,
#                                outside_option_scaling = 0.3,
#                                tier_penalty_rate = 0.06) {
#   if (!is.null(seed)) set.seed(seed)
#   if (!nrow(results_tbl)) {
#     return(results_tbl %>% dplyr::mutate(accepted = integer()))
#   }
#   
#   # Get department tiers for prestige preference
#   dept_tiers <- departments %>%
#     dplyr::select(dept_id, prestige_tier) %>%
#     mutate(
#       dept_tier_num = case_when(
#         prestige_tier == "Tier 1" ~ 1L,
#         prestige_tier == "Tier 2" ~ 2L,
#         prestige_tier == "Tier 3" ~ 3L,
#         TRUE ~ 4L
#       )
#     )
#   
#   # Calculate candidate-side fit for each offer
#   f_j_candidate_values <- numeric(nrow(results_tbl))
#   for (i in 1:nrow(results_tbl)) {
#     row_data <- results_tbl[i, ]
#     dept_row <- departments[departments$dept_id == row_data$dept_id, ]
#     if (nrow(dept_row) == 0) {
#       f_j_candidate_values[i] <- row_data$f_j
#     } else {
#       f_j_candidate_values[i] <- calculate_candidate_f_j(
#         candidate_row = row_data,
#         department_row = dept_row,
#         questions = questions
#       )
#     }
#   }
#   results_tbl$f_j_candidate <- f_j_candidate_values
#   
#   # Join department tier info
#   results_tbl <- results_tbl %>%
#     left_join(dept_tiers, by = "dept_id")
#   
#   softmax_safe <- function(x, temp = 1) {
#     x <- as.numeric(x)
#     if (length(x) == 0L) return(numeric())
#     if (all(!is.finite(x))) return(rep(1 / length(x), length(x)))
#     x[!is.finite(x)] <- min(x[is.finite(x)], na.rm = TRUE) - 10
#     if (!is.finite(temp) || temp <= 0) temp <- 1
#     z  <- (x - max(x, na.rm = TRUE)) / temp
#     z  <- pmax(pmin(z, 700), -700)
#     ez <- exp(z)
#     s  <- sum(ez)
#     if (!is.finite(s) || s <= 0) return(rep(1 / length(x), length(x)))
#     p <- ez / s
#     if (any(!is.finite(p)) || any(p < 0)) p <- rep(1 / length(p), length(p))
#     p
#   }
#   
#   results_tbl %>%
#     dplyr::mutate(
#       # Base utility from preference alignment
#       base_util = exp(v_i_bar * log(s_j + 1e-8) + (1 - v_i_bar) * log(f_j_candidate + 1e-8)),
#       # Prestige preference: higher quality candidates prefer higher tier departments
#       prestige_bonus = exp(-0.8 * v_i_bar * (dept_tier_num - 1)),
#       # Combined utility for this offer
#       cand_util = base_util * prestige_bonus,
#       accepted = 0L
#     ) %>%
#     dplyr::group_by(strategy, year, cand_id) %>%
#     dplyr::group_modify(function(df, keys) {
#       rows <- which(df$offered == 1L)
#       if (length(rows) == 0L) {
#         df$accepted <- 0L
#         return(df)
#       }
#       
#       offer_utils <- df$cand_util[rows]
#       best_offer_util <- max(offer_utils)
#       best_offer_tier <- df$dept_tier_num[rows][which.max(offer_utils)]
#       v_i_bar <- df$v_i_bar[1]
#       cand_tier <- df$cand_tier[1]
#       
#       # Base outside option: higher quality candidates have better alternatives
#       base_outside <- case_when(
#         cand_tier == 1 ~ 0.30,
#         cand_tier == 2 ~ 0.18,
#         cand_tier == 3 ~ 0.10,
#         TRUE ~ 0.05
#       )
#       
#       # GENTLE disappointment: linear scaling with tier gap
#       # tier_gap = how many tiers below their level
#       tier_gap <- pmax(0, best_offer_tier - cand_tier)
#       
#       # Linear disappointment bonus
#       # tier_penalty_rate controls how much each tier gap adds to outside option
#       # Default 0.06 means:
#       #   Gap = 1: +0.06 * 1 * v_i_bar ≈ +0.04 for Tier 1 candidate
#       #   Gap = 2: +0.06 * 2 * v_i_bar ≈ +0.08 for Tier 1 candidate
#       #   Gap = 3: +0.06 * 3 * v_i_bar ≈ +0.12 for Tier 1 candidate
#       disappointment_bonus <- tier_penalty_rate * tier_gap * v_i_bar
#       
#       outside_util <- (base_outside + disappointment_bonus) * outside_option_scaling
#       
#       all_utils <- c(offer_utils, outside_util)
#       
#       log_u <- log(pmax(all_utils, 1e-12))
#       log_u_noisy <- log_u + rnorm(length(all_utils), mean = 0, sd = noise_sd)
#       probs <- softmax_safe(log_u_noisy, temp = temperature)
#       
#       pick_idx <- sample(length(all_utils), size = 1, prob = probs)
#       acc <- integer(nrow(df))
#       if (pick_idx <= length(rows)) {
#         acc[rows[pick_idx]] <- 1L
#       }
#       df$accepted <- acc
#       df
#     }) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-base_util, -prestige_bonus, -dept_tier_num)
# }
# =============================================================================
# NESTED PARTICIPATION ASSIGNMENTS
# =============================================================================

generate_nested_participation_assignments <- function(n_candidates, 
                                                      participation_rates = c(0.05, 0.20, 0.50, 0.90),
                                                      seed = 123) {
  set.seed(seed)
  propensities <- runif(n_candidates)
  cand_ids <- 1:n_candidates
  participation_sets <- list()
  participation_sets[["0"]] <- integer(0)
  sorted_idx <- order(propensities)
  
  for (rate in participation_rates) {
    n_participants <- floor(n_candidates * rate)
    participation_sets[[as.character(rate)]] <- cand_ids[sorted_idx[1:n_participants]]
  }
  
  participation_sets[["1"]] <- cand_ids
  participation_sets
}

# =============================================================================
# ADAPTIVE DEPARTMENT STRATEGIES
# =============================================================================

generate_department_strategies_adaptive <- function(n_departments,
                                                    n_years,
                                                    participation_rate,
                                                    seed = 456) {
  set.seed(seed + round(participation_rate * 1000))
  prob_S1 <- pmin(0.9, pmax(0.0, participation_rate))
  strategy_matrix <- matrix(
    sample(1:2, n_departments * n_years, replace = TRUE,
           prob = c(prob_S1, 1 - prob_S1)),
    nrow = n_departments,
    ncol = n_years
  )
  strategy_matrix
}



# generate_department_strategies_adaptive <- function(n_departments,
#                                                     n_years,
#                                                     participation_rate,
#                                                     seed = 456) {
#   set.seed(seed + round(participation_rate * 1000))
#   
#   # Only allow S1 when participation rate >= 50%
#   if (participation_rate < 0.50) {
#     # Force all departments to use S2 (no questionnaire data)
#     prob_S1 <- 0
#   } else {
#     # Scale probability from 0 at 50% to 0.9 at 100%
#     # Linear interpolation: prob_S1 = (rate - 0.5) / 0.5 * 0.9
#     prob_S1 <- pmin(0.9, pmax(0.0, participation_rate))
#     # prob_S1 <- pmin(0.9, (participation_rate - 0.50) / 0.50 * 0.9)
#   }
#   
#   strategy_matrix <- matrix(
#     sample(1:2, n_departments * n_years, replace = TRUE,
#            prob = c(prob_S1, 1 - prob_S1)),
#     nrow = n_departments,
#     ncol = n_years
#   )
#   strategy_matrix
# }

# =============================================================================
# MAIN MARKET SIMULATION
# =============================================================================

simulate_market_year_adaptive <- function(candidates, departments, questions, year, 
                                          dept_models_pairwise,
                                          participants_this_year,
                                          dept_strategies,
                                          yearly_hiring_schedule,  # NEW PARAMETER
                                          participation_rate = 0,
                                          alpha = 0.05, 
                                          L_repeats = 200, 
                                          tuple_size = NULL,
                                          noise_method = "bootstrap", 
                                          noise_scale = 0.15,
                                          shortlist_enabled = TRUE,
                                          collect_ranking_panel = TRUE,
                                          cand_tier_col = "quality_tier", 
                                          seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  candidates <- candidates %>%
    mutate(participates = cand_id %in% participants_this_year)
  
  tier_to_int <- function(x) {
    as.integer(factor(as.character(x), levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  }
  
  applications <- generate_applications(candidates, departments, questions)
  all_results <- tibble()
  learning_data_pairwise <- vector("list", length = nrow(departments))
  rank_panel <- tibble()
  diag_list <- list()
  
  for (j in 1:nrow(departments)) {
    dept <- departments[j, ]
    
    # GET YEAR-SPECIFIC HIRING QUOTA
    h_j_this_year <- yearly_hiring_schedule[j, year]
    k_j_this_year <- 5L * h_j_this_year  # Interview budget is 5x hiring quota
    
    # SKIP DEPARTMENT IF NOT HIRING THIS YEAR
    if (h_j_this_year == 0L) {
      # Still record diagnostics for non-hiring departments
      dept_applications <- applications %>%
        filter(dept_id == dept$dept_id)
      
      if (nrow(dept_applications) > 0) {
        apps_all <- dept_applications %>%
          left_join(candidates, by = "cand_id") %>%
          mutate(s_j = dept$s_j)
        
        diag_df <- apps_all %>%
          mutate(
            year = year, dept_id = dept$dept_id, strategy = "pairwise",
            pi_pred = NA_real_, U_true = NA_real_, U_hat = NA_real_,
            r_true = NA_real_, r_hat = NA_real_,
            interviewed = 0L, considered = 0L,
            h_j = 0L, k_j = 0L  # Record that dept wasn't hiring
          )
        diag_list[[length(diag_list) + 1L]] <- diag_df
      }
      next
    }
    
    dept_applications <- applications %>%
      filter(dept_id == dept$dept_id)
    if (nrow(dept_applications) == 0) next
    
    dept_strategy <- if (participation_rate == 0) {
      "S2"
    } else {
      ifelse(dept_strategies[j, year] == 1, "S1", "S2")
    }
    
    apps_all <- dept_applications %>%
      left_join(candidates, by = "cand_id") %>%
      mutate(s_j = dept$s_j)
    
    if (!cand_tier_col %in% names(apps_all)) {
      apps_all[[cand_tier_col]] <- factor("Tier 4", levels = c("Tier 1","Tier 2","Tier 3","Tier 4"))
    } else {
      apps_all[[cand_tier_col]] <- factor(
        as.character(apps_all[[cand_tier_col]]),
        levels = c("Tier 1","Tier 2","Tier 3","Tier 4")
      )
    }
    
    apps_all <- apps_all %>%
      mutate(
        dept_tier = tier_to_int(dept$prestige_tier %||% "Tier 4"),
        cand_tier = tier_to_int(.data[[cand_tier_col]])
      )
    
    # Calculate f_j for ALL applications
    f_j_values_all <- vapply(
      1:nrow(apps_all),
      function(ii) calculate_f_j(apps_all[ii, ], dept, questions),
      numeric(1)
    )
    apps_all$f_j <- f_j_values_all
    
    # Shortlist
    if (shortlist_enabled) {
      allow_map <- list(
        "Tier 1" = c("Tier 1"),
        "Tier 2" = c("Tier 1","Tier 2"),
        "Tier 3" = c("Tier 1","Tier 2","Tier 3"),
        "Tier 4" = c("Tier 1","Tier 2","Tier 3","Tier 4")
      )
      pt <- as.character(dept$prestige_tier %||% "Tier 4")
      allowed <- allow_map[[pt]] %||% allow_map[["Tier 4"]]
      considered_mask <- as.character(apps_all[[cand_tier_col]]) %in% allowed
    } else {
      considered_mask <- rep(TRUE, nrow(apps_all))
    }
    
    applicant_data <- apps_all[considered_mask, , drop = FALSE]
    
    if (nrow(applicant_data) == 0) {
      diag_df <- apps_all %>%
        mutate(
          year = year, dept_id = dept$dept_id, strategy = "pairwise",
          pi_pred = NA_real_, U_true = NA_real_, U_hat = NA_real_,
          r_true = NA_real_, r_hat = NA_real_,
          interviewed = 0L, considered = 0L,
          h_j = h_j_this_year, k_j = k_j_this_year
        )
      diag_list[[length(diag_list) + 1L]] <- diag_df
      next
    }
    
    applicant_data <- applicant_data %>% mutate(row_id = row_number())
    
    # Apply adaptive strategy
    # Apply adaptive strategy
    applicant_data_pw <- applicant_data %>%
      mutate(
        f_j_used = case_when(
          !participates & dept_strategy == "S1" ~ 0.0,
          !participates & dept_strategy == "S2" ~ v_i_bar,
          TRUE ~ f_j
        )
      )
    
    # SAVE the actual f_j before prediction
    actual_f_j <- applicant_data_pw$f_j
    
    # Predict acceptance probabilities
    # Create temp dataframe with f_j_used for the neural network
    applicant_data_pw_temp <- applicant_data_pw
    applicant_data_pw_temp$f_j <- applicant_data_pw$f_j_used  # NN sees f_j_used
    
    # Add department characteristics for prediction
    for (q in questions$numerical) {
      applicant_data_pw_temp[[q]] <- dept[[q]]
    }
    for (q_name in names(questions$categorical)) {
      applicant_data_pw_temp[[q_name]] <- as.character(dept[[q_name]])
    }
    
    applicant_data_pw <- predict_acceptance_probability(
      dept_models_pairwise[[j]], 
      applicant_data_pw_temp,
      n_bootstrap = L_repeats,
      include_fit = TRUE,
      include_questions = TRUE,
      seed = j * 1000 + year
    )
    
    # RESTORE the actual f_j after prediction
    applicant_data_pw$f_j <- actual_f_j
    
    # U_det using f_j_used (for ranking/selection)
    U_det_pw <- exp(
      dept$s_j * log(applicant_data_pw$v_i_bar + 1e-8) +
        (1 - dept$s_j) * log(applicant_data_pw$f_j_used + 1e-8)
    )
    
    applicant_data_pw <- applicant_data_pw %>%
      dplyr::mutate(
        f_j_used = applicant_data_pw$f_j_used,
        U_det = U_det_pw,
        U_hat = U_det * pmin(pmax(pi_pred, 1e-5), 1 - 1e-5)
      )
    
    # True utility with ACTUAL f_j (now correctly preserved)
    U_true_pw <- true_utility(
      s_j = applicant_data_pw$s_j,
      v_i_bar = applicant_data_pw$v_i_bar,
      f_j = applicant_data_pw$f_j,  # This is now the real f_j
      dept_tier = applicant_data_pw$dept_tier,
      cand_tier = applicant_data_pw$cand_tier
    )
    applicant_data_pw$U_true <- U_true_pw
    applicant_data_pw$r_true <- rank(-U_true_pw, ties.method = "average")
    applicant_data_pw$r_point <- rank(-applicant_data_pw$U_hat, ties.method = "average")
    
    # Pairwise ranking
    if (collect_ranking_panel) {
      rpair <- compute_pairwise_lower_ranks(
        applicant_data_pw, L_repeats, tuple_size, noise_method, noise_scale, alpha
      )
      applicant_data_pw <- applicant_data_pw %>%
        dplyr::left_join(rpair, by = "cand_id") %>%
        dplyr::mutate(r_pair_lb = rank_lower) %>%
        dplyr::select(-rank_lower)
    }
    
    # USE YEAR-SPECIFIC k_j and h_j FOR INTERVIEW SELECTION
    interviewed_ids_pw <- select_interviews_sure_screening(
      applicant_data_pw,
      k_j = k_j_this_year,  # USE YEAR-SPECIFIC VALUE
      h_j = h_j_this_year,  # USE YEAR-SPECIFIC VALUE
      alpha = alpha, L_repeats = L_repeats,
      tuple_size = tuple_size,
      noise_method = noise_method,
      noise_scale = noise_scale
    )
    
    applicant_data_pw$interviewed_flag <- as.integer(applicant_data_pw$cand_id %in% interviewed_ids_pw)
    
    # Store ranking panel
    if (collect_ranking_panel) {
      rank_panel <- dplyr::bind_rows(
        rank_panel,
        applicant_data_pw %>%
          dplyr::transmute(
            year, dept_id = dept$dept_id, strategy = "pairwise",
            cand_id, s_j, v_i_bar, f_j, participates,
            U_true, U_hat, r_true, r_point,
            r_pair_lb = dplyr::if_else(is.finite(r_pair_lb), r_pair_lb, NA_real_),
            interviewed = interviewed_flag,
            
            k_j = k_j_this_year,  # YEAR-SPECIFIC
            
            h_j = h_j_this_year   # YEAR-SPECIFIC
          )
      )
    }
    
    # Prepare interviewed data for offers
    interviewed_data_pw <- applicant_data_pw %>%
      dplyr::filter(cand_id %in% interviewed_ids_pw) %>%
      dplyr::mutate(interviewed = 1L)
    
    # Extend offers
    if (nrow(interviewed_data_pw) > 0) {
      interviewed_data_pw <- interviewed_data_pw %>%
        dplyr::arrange(dplyr::desc(U_hat)) %>%
        dplyr::mutate(offered = as.integer(dplyr::row_number() <= h_j_this_year))
    } else {
      interviewed_data_pw <- interviewed_data_pw %>% dplyr::mutate(offered = 0L)
    }
    
    interviewed_data_pw <- interviewed_data_pw %>%
      
      dplyr::mutate(
        
        year = year, 
        
        dept_id = dept$dept_id, 
        
        strategy = "pairwise",
        
        h_j = h_j_this_year,  # ADD YEAR-SPECIFIC VALUES
        
        k_j = k_j_this_year
        
      )
    
    if (nrow(interviewed_data_pw) > 0) {
      all_results <- dplyr::bind_rows(all_results, interviewed_data_pw)
    }
    
    # Diagnostics
    diag_pw <- apps_all %>%
      dplyr::mutate(
        considered = as.integer(cand_id %in% applicant_data$cand_id),
        
        interviewed = as.integer(cand_id %in% interviewed_ids_pw),
        
        h_j = h_j_this_year,
        
        k_j = k_j_this_year
      ) %>%
      dplyr::left_join(
        applicant_data_pw %>% dplyr::select(cand_id, pi_pred, U_true, U_hat, r_true),
        by = "cand_id"
      ) %>%
      dplyr::transmute(
        year = year, dept_id = dept$dept_id, strategy = "pairwise",
        cand_id, s_j, v_i_bar, f_j, pi_pred, U_true, U_hat, r_true,
        interviewed, considered, participates, h_j, k_j
      )
    
    diag_list[[length(diag_list) + 1L]] <- diag_pw
  }
  
  # Resolve acceptances
  if (nrow(all_results) > 0) {
    all_results <- resolve_accept_one(all_results, departments = departments, questions = questions, seed = year, temperature = 0.05, noise_sd = 0.01)
  }
  
  # Build learning data with proper department columns
  for (j in 1:nrow(departments)) {
    ld_pw <- all_results %>% 
      dplyr::filter(dept_id == departments$dept_id[j], strategy == "pairwise")
    
    if (nrow(ld_pw) > 0) {
      dept_info <- departments[j, ]
      
      learning_data_pairwise[[j]] <- ld_pw %>%
        dplyr::select(
          year, dept_id, cand_id, s_j, v_i_bar, f_j, offered, accepted,
          dplyr::starts_with("q_")
        ) %>%
        dplyr::mutate(
          # Numerical department features
          q4_cost_of_living = dept_info$q4_cost_of_living,
          q6_typical_salary_range = dept_info$q6_typical_salary_range,
          q14_phd_student_ratio = dept_info$q14_phd_student_ratio,
          # Categorical department features - ensure character type
          q1_geographic_setting = as.character(dept_info$q1_geographic_setting),
          q2_region = as.character(dept_info$q2_region),
          q3_airport_proximity = as.character(dept_info$q3_airport_proximity),
          q5_dual_career = as.character(dept_info$q5_dual_career),
          q7_typical_startup = as.character(dept_info$q7_typical_startup),
          q8_guaranteed_summer = as.character(dept_info$q8_guaranteed_summer),
          q9_typical_teaching_load = as.character(dept_info$q9_typical_teaching_load),
          q10_course_types = as.character(dept_info$q10_course_types),
          q11_mentoring_program = as.character(dept_info$q11_mentoring_program),
          q12_research_culture = as.character(dept_info$q12_research_culture),
          q13_publication_venues = as.character(dept_info$q13_publication_venues),
          q15_medical_school_proximity = as.character(dept_info$q15_medical_school_proximity)
        )
    }
  }
  
  list(
    results = all_results,
    learning_data_pairwise = learning_data_pairwise,
    rank_panel = rank_panel,
    diagnostics = list(applicant_level = bind_rows(diag_list))
  )
}

# =============================================================================
# MAIN DRIVER FUNCTION
# =============================================================================

# =============================================================================
# MAIN DRIVER FUNCTION (with cumulative diagnostics)
# =============================================================================

run_job_market_sim_adaptive <- function(departments,
                                        questions,
                                        n_candidates = 500,
                                        sim_years = 10,
                                        participation_rate,
                                        yearly_candidate_cohorts = NULL,
                                        participation_sets = NULL,
                                        yearly_hiring_schedule = NULL,
                                        seed = 123,
                                        alpha = 0.05,
                                        L_repeats = 200,
                                        tuple_size = NULL,
                                        noise_method = "bootstrap",
                                        noise_scale = 0.15,
                                        cand_tier_cutpoints = c(0.10, 0.25, 0.50),
                                        print_diagnostics = TRUE) {
  
  set.seed(seed)
  torch::torch_manual_seed(seed)
  
  n_departments <- nrow(departments)
  
  # Generate hiring schedule if not provided (for consistency across scenarios)
  if (is.null(yearly_hiring_schedule)) {
    yearly_hiring_schedule <- generate_yearly_hiring_schedule(
      n_departments = n_departments,
      n_years = sim_years,
      departments = departments,
      seed = seed + 500
    )
  }
  
  dept_strategies <- generate_department_strategies_adaptive(
    n_departments = n_departments,
    n_years = sim_years,
    participation_rate = participation_rate,
    seed = seed + 1000
  )
  
  generate_cohorts <- is.null(yearly_candidate_cohorts)
  if (generate_cohorts) {
    yearly_candidate_cohorts <- vector("list", sim_years)
  }
  
  cand_roster_all <- list()
  rank_all <- list()
  diag_all <- list()
  
  mdl_pairwise <- purrr::map(1:nrow(departments), 
                             ~ initialize_department_model(questions, 
                                                           include_fit = TRUE, 
                                                           include_questions = TRUE))
  
  res_all <- list()
  
  # Get department tier info for diagnostics
  dept_tier_info <- departments %>%
    dplyr::select(dept_id, tier)
  
  for (year in 1:sim_years) {
    cat("Simulating year", year, "with participation rate", participation_rate, "...\n")
    
    if (generate_cohorts) {
      yearly_candidate_cohorts[[year]] <- generate_candidates_new(
        n_candidates, questions, seed = seed + year
      )
    }
    
    candidates <- yearly_candidate_cohorts[[year]]
    
    rate_key <- as.character(participation_rate)
    participants_this_year <- if (rate_key %in% names(participation_sets)) {
      participation_sets[[rate_key]]
    } else {
      integer(0)
    }
    
    candidates <- candidates %>%
      mutate(quality_tier = assign_tiers_from_quantiles(v_i_bar, cutpoints = cand_tier_cutpoints))
    
    cand_roster_all[[year]] <- candidates %>%
      mutate(participates = cand_id %in% participants_this_year) %>%
      transmute(year = !!year, cand_id, quality_tier, v_i_bar, participates)
    
    out <- simulate_market_year_adaptive(
      candidates, departments, questions, year,
      dept_models_pairwise = mdl_pairwise,
      participants_this_year = participants_this_year,
      dept_strategies = dept_strategies,
      yearly_hiring_schedule = yearly_hiring_schedule,
      participation_rate = participation_rate,
      alpha = alpha,
      L_repeats = L_repeats,
      tuple_size = tuple_size,
      noise_method = noise_method,
      noise_scale = noise_scale,
      shortlist_enabled = TRUE,
      collect_ranking_panel = TRUE,
      cand_tier_col = "quality_tier",
      seed = year
    )
    
    diag_all[[year]] <- out$diagnostics$applicant_level
    res_all[[year]] <- out$results
    rank_all[[year]] <- out$rank_panel
    
    
    # ==========================================================================
    # CUMULATIVE DIAGNOSTICS
    # ==========================================================================
    if (print_diagnostics) {
      # Combine all results so far
      cumulative_results <- bind_rows(res_all[1:year])
      
      if (nrow(cumulative_results) > 0) {
        # Join with department tiers
        cumulative_with_tiers <- cumulative_results %>%
          left_join(dept_tier_info, by = "dept_id")
        
        # Calculate cumulative quotas from hiring schedule
        cumulative_quota <- tibble(
          dept_id = 1:n_departments
        ) %>%
          left_join(dept_tier_info, by = "dept_id") %>%
          crossing(yr = 1:year) %>%
          mutate(h_j = purrr::map2_int(dept_id, yr, ~yearly_hiring_schedule[.x, .y])) %>%
          group_by(tier) %>%
          summarise(total_quota = sum(h_j), .groups = "drop")
        
        # Overall metrics
        overall_offers <- sum(cumulative_with_tiers$offered, na.rm = TRUE)
        overall_accepts <- sum(cumulative_with_tiers$accepted, na.rm = TRUE)
        overall_yield <- if (overall_offers > 0) overall_accepts / overall_offers else NA
        overall_mean_U <- mean(cumulative_with_tiers$U_true[cumulative_with_tiers$accepted == 1], na.rm = TRUE)
        overall_mean_f <- mean(cumulative_with_tiers$f_j[cumulative_with_tiers$accepted == 1], na.rm = TRUE)
        
        # By tier metrics
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
            tier = factor(tier, 
                          levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
          ) %>%
          arrange(tier)
        
        # Print cumulative diagnostics
        cat("\n")
        cat(strrep("─", 70), "\n")
        cat(sprintf("  CUMULATIVE RESULTS (Years 1-%d) | ρ = %.0f%%\n", year, participation_rate * 100))
        cat(strrep("─", 70), "\n")
        cat(sprintf("  Overall: Offers=%d | Accepts=%d | Yield=%.1f%% | Mean U=%.4f | Mean f=%.4f\n",
                    overall_offers, overall_accepts, overall_yield * 100, 
                    overall_mean_U, overall_mean_f))
        cat(strrep("─", 70), "\n")
        cat(sprintf("  %-8s %7s %7s %7s %7s %9s %9s %9s\n", 
                    "Tier", "Quota", "Offers", "Accepts", "Yield", "FillRate", "Mean_U", "Mean_f"))
        cat(strrep("─", 70), "\n")
        
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
        cat(strrep("─", 70), "\n\n")
      }
      
      cat("\nDEBUG - Correlation check:\n")
      cat("  Corr(v_i_bar, f_j) among accepted: ", 
          cor(cumulative_with_tiers$v_i_bar[cumulative_with_tiers$accepted == 1],
              cumulative_with_tiers$f_j[cumulative_with_tiers$accepted == 1], 
              use = "complete.obs"), "\n")
    }
    
    # Train models
    for (j in 1:nrow(departments)) {
      if (!is.null(out$learning_data_pairwise[[j]]) && 
          nrow(out$learning_data_pairwise[[j]]) > 0) {
        mdl_pairwise[[j]]$historical_data <- bind_rows(
          mdl_pairwise[[j]]$historical_data,
          out$learning_data_pairwise[[j]]
        )
        if (year >= 2 && nrow(mdl_pairwise[[j]]$historical_data) >= 30) {
          mdl_pairwise[[j]] <- train_department_model(
            mdl_pairwise[[j]], mdl_pairwise[[j]]$historical_data,
            include_fit = TRUE, include_questions = TRUE, seed = year * j
          )
        }
      }
    }
  }
  
  list(
    results = bind_rows(res_all),
    departments = departments,
    questions = questions,
    cand_roster = bind_rows(cand_roster_all),
    rank_panel = bind_rows(rank_all),
    diagnostics = list(applicant_level = bind_rows(diag_all)),
    participation_rate = participation_rate,
    yearly_candidate_cohorts = yearly_candidate_cohorts,
    yearly_hiring_schedule = yearly_hiring_schedule
  )
}


# =============================================================================
# MAIN EXECUTION EXAMPLE
# =============================================================================
setwd("/Users/alikmofrad/UCLA PhD/SCALE Group/Research/Job Market /Codes/Academic Job Market/department_generator")
# This section shows how to run the simulation
# Uncomment and modify as needed

# Load department data
departments_final <- read_csv("departments_dataset.csv")

# Sample 20 departments with stratified sampling by tier

################################################################################
# Approach FULL (using all Departments)
################################################################################
departments <- departments_final %>%
  mutate(dept_id = row_number()) %>%
  prepare_departments(questions, seed = 42)   # updated signature below
################################################################################
################################################################################

################################################################################
# Approach 1 
################################################################################
# set.seed(6)
# tier_n <- c("Tier 1" = 2L, "Tier 2" = 3L, "Tier 3" = 5L, "Tier 4" = 10L)
# sampled_depts <- departments_final %>%
#   group_split(tier) %>%
#   map_dfr(function(df) {
#     n_pick <- tier_n[[as.character(df$tier[1])]]
#     if (is.null(n_pick)) n_pick <- 0L
#     df %>% slice_sample(n = min(n_pick, nrow(df)))
#   }) %>%
#   mutate(dept_id = row_number())
# 
# departments <- prepare_departments(sampled_depts, questions)
# 
# departments %>% group_by(tier) %>% summarise(10*sum(h_j))
# 
# departments %>% group_by(tier) %>% summarise(max(s_j), min(s_j),mean(s_j), sd(s_j))
################################################################################
################################################################################

################################################################################
# Approach 2
################################################################################
# set.seed(14)
# dep_sample <- sample_n(departments_final, 20)
# 
# departments <- prepare_departments(dep_sample, questions)
# 
# departments %>% group_by(tier) %>% summarise(10*sum(h_j))
# 
# table(departments$tier)
# 
# departments %>% group_by(tier) %>% summarise(max(s_j), min(s_j),mean(s_j), sd(s_j))
################################################################################
################################################################################


################################################################################
# Approach 3
################################################################################
# set.seed(123)
# tier_n <- c("Tier 1" = 5L, "Tier 2" = 5L, "Tier 3" = 5L, "Tier 4" = 5L)
# sampled_depts <- departments_final %>%
#   group_split(tier) %>%
#   map_dfr(function(df) {
#     n_pick <- tier_n[[as.character(df$tier[1])]]
#     if (is.null(n_pick)) n_pick <- 0L
#     df %>% slice_sample(n = min(n_pick, nrow(df)))
#   }) %>%
#   mutate(dept_id = row_number())
# 
# #departments <- prepare_departments(sampled_depts)
# departments <- prepare_departments(sampled_depts, questions, seed = 42)
# 
# departments %>% group_by(tier) %>% summarise(Total_Slots = 10*sum(h_j), Annual_Slots = sum(h_j))
# 
# departments %>% group_by(tier) %>% summarise(max(s_j), min(s_j), mean(s_j), sd(s_j))
################################################################################
################################################################################



# Generate hiring schedule ONCE before any simulations
sim_years <- 10
n_candidates <- 400

# First prepare departments (without h_j/k_j)
departments <- departments_final %>%
  mutate(dept_id = row_number()) %>%
  prepare_departments(questions, seed = 42)

# Generate hiring schedule ONCE
yearly_hiring_schedule <- generate_yearly_hiring_schedule(
  n_departments = nrow(departments),
  n_years = sim_years,
  departments = departments,
  seed = 789  # Fixed seed for consistency
)

# Generate participation sets
participation_sets <- generate_nested_participation_assignments(
  n_candidates = n_candidates,
  participation_rates = c(0.05, 0.20, 0.50, 0.90),
  seed = 123
)

# Run baseline simulation WITH hiring schedule
baseline_sim <- run_job_market_sim_adaptive(
  departments = departments,
  questions = questions,
  n_candidates = n_candidates,
  sim_years = sim_years,
  participation_rate = 0,
  yearly_candidate_cohorts = NULL,
  participation_sets = participation_sets,
  yearly_hiring_schedule = yearly_hiring_schedule,  # PASS THE SCHEDULE
  seed = 101,
  alpha = 0.05,
  L_repeats = 10,
  noise_method = "bootstrap",
  noise_scale = 0.15,
  cand_tier_cutpoints = c(0.10, 0.25, 0.50)
)

yearly_cohorts <- baseline_sim$yearly_candidate_cohorts

# Run all participation scenarios WITH SAME hiring schedule
all_sim_results <- list()
all_sim_results[["baseline"]] <- baseline_sim

for (rate in c(0.05, 0.20, 0.50, 0.90, 1.00)) {
  scenario_name <- as.character(rate)
  
  cat("\n========================================\n")
  cat("Running simulation with ρ =", rate * 100, "%\n")
  cat("========================================\n")
  
  all_sim_results[[scenario_name]] <- run_job_market_sim_adaptive(
    departments = departments,
    questions = questions,
    n_candidates = n_candidates,
    sim_years = sim_years,
    participation_rate = rate,
    yearly_candidate_cohorts = yearly_cohorts,
    participation_sets = participation_sets,
    yearly_hiring_schedule = yearly_hiring_schedule,  # SAME SCHEDULE FOR ALL
    seed = 101,
    alpha = 0.05,
    L_repeats = 10,
    noise_method = "bootstrap",
    noise_scale = 0.15,
    cand_tier_cutpoints = c(0.10, 0.25, 0.50)
  )
}

cat("\n✓ All simulations complete!\n")




#all_sim_results_W <- all_sim_results


# all_sim_results <- all_sim_results_10L
# all_sim_results <- all_sim_results_20L
# all_sim_results <- all_sim_results_50L
# all_sim_results <- all_sim_results_100L










































# =============================================================================
# NEW FIGURES AND STATISTICAL TESTS
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
      plot.title         = element_text(face = "bold", size = rel(1.2)),
      plot.subtitle      = element_text(color = "grey25", size = rel(0.9))
    )
}

tier_colors <- c(
  "Tier 1" = "#1b2838", 
  "Tier 2" = "#3c6e71",  
  "Tier 3" = "#a64b29",  
  "Tier 4" = "#7a4f82"
)

# =============================================================================
# FIGURE 1: Candidate Welfare Gain (Theorem 4.1)
# =============================================================================

# =============================================================================
# CORRECTED FIGURE 1: Candidate Welfare Gain (Theorem 4.1)
# =============================================================================

make_fig_candidate_welfare_gain <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Get the total number of candidates in the market (cohort size)
  n_candidates_per_year <- all_sim_results[["baseline"]]$cand_roster %>%
    filter(year >= year_filter[1], year <= year_filter[2]) %>%
    group_by(year) %>%
    summarise(n = n(), .groups = "drop") %>%
    pull(n) %>%
    mean()
  
  n_years <- length(year_filter[1]:year_filter[2])
  total_candidates <- n_candidates_per_year * n_years
  
  # Extract accepted offers for welfare calculation
  welfare_data <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      mutate(
        participation_rate = rate,
        V_ij = candidate_utility(v_i_bar, s_j, f_j)
      )
  })
  
  # Aggregate welfare accounting for ALL candidates
  aggregate_welfare <- welfare_data %>%
    group_by(participation_rate) %>%
    summarise(
      total_welfare = sum(V_ij, na.rm = TRUE),
      n_matches = n(),
      mean_welfare_per_match = mean(V_ij, na.rm = TRUE),
      se_per_match = sd(V_ij, na.rm = TRUE) / sqrt(n()),
      mean_welfare_per_candidate = total_welfare / total_candidates,
      .groups = "drop"
    )
  
  # Create candidate-level welfare (0 if unmatched)
  candidate_level_welfare <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    all_cands <- all_sim_results[[rate_name]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2]) %>%
      dplyr::select(year, cand_id)
    
    matched <- all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j)) %>%
      group_by(year, cand_id) %>%
      summarise(welfare = sum(V_ij), .groups = "drop")
    
    all_cands %>%
      left_join(matched, by = c("year", "cand_id")) %>%
      mutate(
        welfare = coalesce(welfare, 0),
        participation_rate = rate
      )
  })
  
  # Statistical test
  baseline_welfare <- candidate_level_welfare %>% 
    filter(participation_rate == 0) %>% 
    pull(welfare)
  
  full_welfare <- candidate_level_welfare %>% 
    filter(participation_rate == 1) %>% 
    pull(welfare)
  
  welfare_test <- t.test(full_welfare, baseline_welfare, paired = TRUE)
  
  cat("\n=== CANDIDATE WELFARE GAIN TEST ===\n")
  cat("Baseline: Total =", sprintf("%.2f", sum(baseline_welfare)), 
      "| Matches:", sum(baseline_welfare > 0), "\n")
  cat("Full:     Total =", sprintf("%.2f", sum(full_welfare)), 
      "| Matches:", sum(full_welfare > 0), "\n")
  cat("Gain:", sprintf("%.2f (%.1f%%)", 
                       sum(full_welfare) - sum(baseline_welfare),
                       100 * (sum(full_welfare) / sum(baseline_welfare) - 1)), "\n\n")
  
  # Plot 1: Total welfare
  p1 <- ggplot(aggregate_welfare, aes(x = participation_rate * 100, 
                                      y = total_welfare)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    geom_hline(yintercept = aggregate_welfare$total_welfare[1],
               linetype = "dashed", color = "gray50", linewidth = 0.8) +
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
      y = "Aggregate Candidate Welfare"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Plot 2: Matching rate
  p2 <- ggplot(aggregate_welfare, aes(x = participation_rate * 100, 
                                      y = n_matches / total_candidates)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    scale_x_continuous(
      breaks = c(0, 5, 20, 50, 90, 100),
      labels = c("0", "5", "20", "50", "90", "100")
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = "Matching Rate"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Combine plots
  combined_plot <- p1 + p2 +
    plot_layout(ncol = 2)
  
  list(
    plot = combined_plot,
    plot_welfare = p1,
    plot_matching = p2,
    test = welfare_test, 
    aggregate_data = aggregate_welfare,
    candidate_data = candidate_level_welfare
  )
}


# make_fig_candidate_welfare_gain <- function(all_sim_results, year_filter = c(1, 10)) {
# 
#   # Get the total number of candidates in the market (cohort size)
#   n_candidates_per_year <- all_sim_results[["baseline"]]$cand_roster %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2]) %>%
#     dplyr::group_by(year) %>%
#     dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#     dplyr::pull(n) %>%
#     mean()
# 
#   n_years <- length(year_filter[1]:year_filter[2])
#   total_candidates <- n_candidates_per_year * n_years
# 
#   # Extract accepted offers for welfare calculation
#   welfare_data <- purrr::map_dfr(names(all_sim_results), function(rate_name) {
#     rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
# 
#     all_sim_results[[rate_name]]$results %>%
#       dplyr::filter(
#         year >= year_filter[1], year <= year_filter[2],
#         strategy == "pairwise", accepted == 1
#       ) %>%
#       dplyr::mutate(
#         participation_rate = rate,
#         V_ij = candidate_utility(v_i_bar, s_j, f_j)
#       )
#   })
# 
#   # Aggregate welfare accounting for ALL candidates
#   aggregate_welfare <- welfare_data %>%
#     dplyr::group_by(participation_rate) %>%
#     dplyr::summarise(
#       total_welfare = sum(V_ij, na.rm = TRUE),
#       n_matches = dplyr::n(),
#       mean_welfare_per_match = mean(V_ij, na.rm = TRUE),
#       se_per_match = sd(V_ij, na.rm = TRUE) / sqrt(dplyr::n()),
#       mean_welfare_per_candidate = total_welfare / total_candidates,
#       .groups = "drop"
#     ) %>%
#     dplyr::mutate(
#       match_rate = n_matches / total_candidates
#     )
# 
#   # Baselines (0% participation)
#   baseline_total_welfare <- aggregate_welfare %>%
#     dplyr::filter(participation_rate == 0) %>%
#     dplyr::pull(total_welfare)
# 
#   baseline_match_rate <- aggregate_welfare %>%
#     dplyr::filter(participation_rate == 0) %>%
#     dplyr::pull(match_rate)
# 
#   # Safety (avoid divide-by-zero / missing baselines)
#   if (length(baseline_total_welfare) != 1 || !is.finite(baseline_total_welfare) || baseline_total_welfare == 0) {
#     stop("Baseline total_welfare is missing, non-finite, or zero; cannot compute % increase.")
#   }
#   if (length(baseline_match_rate) != 1 || !is.finite(baseline_match_rate) || baseline_match_rate == 0) {
#     stop("Baseline match_rate is missing, non-finite, or zero; cannot compute relative increase.")
#   }
# 
#   # Convert y-axes:
#   #  - Welfare: percent increase vs baseline
#   #  - Matching: relative increase in hiring probability (dimensionless; label as %)
#   aggregate_welfare <- aggregate_welfare %>%
#     dplyr::mutate(
#       pct_increase_welfare = 100 * (total_welfare / baseline_total_welfare - 1),
#       rel_increase_hiring_prob = (match_rate - baseline_match_rate) / baseline_match_rate
#     )
# 
#   # Create candidate-level welfare (0 if unmatched)
#   candidate_level_welfare <- purrr::map_dfr(names(all_sim_results), function(rate_name) {
#     rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
# 
#     all_cands <- all_sim_results[[rate_name]]$cand_roster %>%
#       dplyr::filter(year >= year_filter[1], year <= year_filter[2]) %>%
#       dplyr::select(year, cand_id)
# 
#     matched <- all_sim_results[[rate_name]]$results %>%
#       dplyr::filter(
#         year >= year_filter[1], year <= year_filter[2],
#         strategy == "pairwise", accepted == 1
#       ) %>%
#       dplyr::mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j)) %>%
#       dplyr::group_by(year, cand_id) %>%
#       dplyr::summarise(welfare = sum(V_ij), .groups = "drop")
# 
#     all_cands %>%
#       dplyr::left_join(matched, by = c("year", "cand_id")) %>%
#       dplyr::mutate(
#         welfare = dplyr::coalesce(welfare, 0),
#         participation_rate = rate
#       )
#   })
# 
#   # Statistical test (paired by construction in your setup)
#   baseline_welfare <- candidate_level_welfare %>%
#     dplyr::filter(participation_rate == 0) %>%
#     dplyr::pull(welfare)
# 
#   full_welfare <- candidate_level_welfare %>%
#     dplyr::filter(participation_rate == 1) %>%
#     dplyr::pull(welfare)
# 
#   welfare_test <- t.test(full_welfare, baseline_welfare, paired = TRUE)
# 
#   cat("\n=== CANDIDATE WELFARE GAIN TEST ===\n")
#   cat("Baseline: Total =", sprintf("%.2f", sum(baseline_welfare)),
#       "| Matches:", sum(baseline_welfare > 0), "\n")
#   cat("Full:     Total =", sprintf("%.2f", sum(full_welfare)),
#       "| Matches:", sum(full_welfare > 0), "\n")
#   cat("Gain:", sprintf("%.2f (%.1f%%)",
#                        sum(full_welfare) - sum(baseline_welfare),
#                        100 * (sum(full_welfare) / sum(baseline_welfare) - 1)), "\n\n")
# 
#   # Plot 1: Welfare (% increase vs baseline)
#   p1 <- ggplot2::ggplot(
#     aggregate_welfare,
#     ggplot2::aes(x = participation_rate * 100, y = pct_increase_welfare)
#   ) +
#     ggplot2::geom_line(linewidth = 1.2, color = "black") +
#     ggplot2::geom_point(size = 3, color = "black") +
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
#                         color = "gray50", linewidth = 0.8) +
#     ggplot2::scale_x_continuous(
#       breaks = c(0, 5, 20, 50, 90, 100),
#       labels = c("0", "5", "20", "50", "90", "100")
#     ) +
#     ggplot2::scale_y_continuous(
#       labels = function(x) paste0(x, "%"),
#       expand = ggplot2::expansion(mult = c(0.05, 0.05))
#     ) +
#     ggplot2::labs(
#       x = "Market Participation Rate (%)",
#       y = "Relative change in aggregate welfare"
#     ) +
#     ggplot2::theme_minimal(base_size = 14) +
#     ggplot2::theme(
#       panel.grid.minor = ggplot2::element_blank(),
#       panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.5),
#       axis.title = ggplot2::element_text(size = 14, face = "bold"),
#       axis.text  = ggplot2::element_text(size = 12),
#       plot.margin = ggplot2::margin(10, 10, 10, 10)
#     )
# 
#   # Plot 2: Relative increase in hiring probability (vs baseline)
#   p2 <- ggplot2::ggplot(
#     aggregate_welfare,
#     ggplot2::aes(x = participation_rate * 100, y = rel_increase_hiring_prob)
#   ) +
#     ggplot2::geom_line(linewidth = 1.2, color = "black") +
#     ggplot2::geom_point(size = 3, color = "black") +
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
#                         color = "gray50", linewidth = 0.8) +
#     ggplot2::scale_x_continuous(
#       breaks = c(0, 5, 20, 50, 90, 100),
#       labels = c("0", "5", "20", "50", "90", "100")
#     ) +
#     ggplot2::scale_y_continuous(
#       labels = scales::percent_format(accuracy = 1), # 0.25 -> "25%"
#       expand = ggplot2::expansion(mult = c(0.05, 0.05))
#     ) +
#     ggplot2::labs(
#       x = "Market Participation Rate (%)",
#       y = "Relative increase in hiring probability"
#     ) +
#     ggplot2::theme_minimal(base_size = 14) +
#     ggplot2::theme(
#       panel.grid.minor = ggplot2::element_blank(),
#       panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.5),
#       axis.title = ggplot2::element_text(size = 14, face = "bold"),
#       axis.text  = ggplot2::element_text(size = 12),
#       plot.margin = ggplot2::margin(10, 10, 10, 10)
#     )
# 
#   combined_plot <- p1 + p2 + patchwork::plot_layout(ncol = 2)
# 
#   list(
#     plot = combined_plot,
#     plot_welfare = p1,
#     plot_matching = p2,
#     test = welfare_test,
#     aggregate_data = aggregate_welfare,
#     candidate_data = candidate_level_welfare
#   )
# }


# Generate figure
welfare_results <- make_fig_candidate_welfare_gain(all_sim_results, year_filter = c(1, 10))

welfare_results$plot
# Save with appropriate dimensions for two-column layout
ggsave("fig_candidate_welfare_gain.pdf",
       welfare_results$plot,
       width = 10,
       height = 4,
       device = cairo_pdf)


# =============================================================================
# CANDIDATE WELFARE GAIN STRATIFIED BY TIER
# =============================================================================

# make_fig_candidate_welfare_by_tier <- function(all_sim_results, year_filter = c(1, 10)) {
#   
#   # Get candidate roster with tiers
#   all_candidates <- map_dfr(names(all_sim_results), function(rate_name) {
#     rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
#     
#     all_sim_results[[rate_name]]$cand_roster %>%
#       filter(year >= year_filter[1], year <= year_filter[2]) %>%
#       mutate(participation_rate = rate)
#   })
#   
#   # Calculate candidates per tier
#   cand_totals_by_tier <- all_candidates %>%
#     filter(participation_rate == 0) %>%
#     count(quality_tier, name = "n_cand")
#   
#   # Extract welfare by tier
#   welfare_data_by_tier <- map_dfr(names(all_sim_results), function(rate_name) {
#     rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
#     
#     roster <- all_sim_results[[rate_name]]$cand_roster %>%
#       filter(year >= year_filter[1], year <= year_filter[2])
#     
#     matches <- all_sim_results[[rate_name]]$results %>%
#       filter(year >= year_filter[1], year <= year_filter[2],
#              strategy == "pairwise", accepted == 1) %>%
#       mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))
#     
#     roster %>%
#       left_join(
#         matches %>% 
#           group_by(year, cand_id) %>%
#           summarise(welfare = sum(V_ij), .groups = "drop"),
#         by = c("year", "cand_id")
#       ) %>%
#       mutate(
#         welfare = coalesce(welfare, 0),
#         participation_rate = rate
#       )
#   })
#   
#   # Aggregate welfare by tier
#   aggregate_welfare_by_tier <- welfare_data_by_tier %>%
#     group_by(participation_rate, quality_tier) %>%
#     summarise(
#       n_cand = n(),
#       total_welfare = sum(welfare, na.rm = TRUE),
#       n_matches = sum(welfare > 0),
#       mean_welfare_per_candidate = total_welfare / n_cand,
#       matching_rate = n_matches / n_cand,
#       mean_welfare_per_match = mean(welfare[welfare > 0], na.rm = TRUE),
#       se_per_match = sd(welfare[welfare > 0], na.rm = TRUE) / sqrt(n_matches),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       quality_tier = factor(quality_tier, 
#                             levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
#     )
#   
#   # Statistical tests by tier
#   cat("\n=== CANDIDATE WELFARE GAINS BY TIER ===\n")
#   
#   tier_tests <- list()
#   tier_summaries <- list()
#   
#   for (tier in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
#     baseline_tier <- welfare_data_by_tier %>%
#       filter(participation_rate == 0, quality_tier == tier) %>%
#       pull(welfare)
#     
#     full_tier <- welfare_data_by_tier %>%
#       filter(participation_rate == 1, quality_tier == tier) %>%
#       pull(welfare)
#     
#     if (length(baseline_tier) > 1 && length(full_tier) > 1) {
#       test <- t.test(full_tier, baseline_tier, paired = TRUE)
#       tier_tests[[tier]] <- test
#       
#       baseline_match_rate <- mean(baseline_tier > 0)
#       full_match_rate <- mean(full_tier > 0)
#       
#       tier_summaries[[tier]] <- list(
#         baseline_mean = mean(baseline_tier),
#         full_mean = mean(full_tier),
#         gain = mean(full_tier) - mean(baseline_tier),
#         baseline_match_rate = baseline_match_rate,
#         full_match_rate = full_match_rate,
#         p_value = test$p.value
#       )
#       
#       cat(tier, ":\n")
#       cat("  Baseline: Welfare/cand = %.5f, Match rate = %.1f%%\n" %>% 
#             sprintf(mean(baseline_tier), baseline_match_rate * 100))
#       cat("  Full:     Welfare/cand = %.5f, Match rate = %.1f%%\n" %>%
#             sprintf(mean(full_tier), full_match_rate * 100))
#       cat("  Gain:     %.5f (p = %.4f) %s\n" %>%
#             sprintf(mean(full_tier) - mean(baseline_tier), test$p.value,
#                     ifelse(test$p.value < 0.05, "✓", "")))
#       cat("\n")
#     }
#   }
#   
#   # Test differential gains
#   gains_by_tier <- tibble(
#     tier = names(tier_summaries),
#     gain = map_dbl(tier_summaries, "gain"),
#     tier_numeric = as.integer(factor(tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
#   )
#   
#   if (nrow(gains_by_tier) > 2) {
#     gain_corr <- cor.test(gains_by_tier$tier_numeric, gains_by_tier$gain, 
#                           method = "spearman")
#     
#     cat("Correlation(tier, gain):", sprintf("%.3f (p = %.4f)\n", 
#                                             gain_corr$estimate, gain_corr$p.value))
#   }
#   
#   # Plot 1: Matching rate by tier
#   p2 <- ggplot(aggregate_welfare_by_tier, 
#                aes(x = participation_rate * 100, 
#                    y = matching_rate,
#                    linetype = quality_tier,
#                    shape = quality_tier,
#                    group = quality_tier)) +
#     geom_line(linewidth = 1.2, color = "black") +
#     geom_point(size = 3, color = "black") +
#     scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
#     scale_shape_manual(values = c(16, 17, 15, 18)) +
#     scale_x_continuous(
#       breaks = c(0, 5, 20, 50, 90, 100),
#       labels = c("0", "5", "20", "50", "90", "100")
#     ) +
#     scale_y_continuous(
#       labels = scales::percent_format(accuracy = 0.1),
#       limits = c(0, NA),
#       expand = expansion(mult = c(0, 0.05))
#     ) +
#     labs(
#       x = "Market Participation Rate (%)",
#       y = "Matching Rate",
#       linetype = "Candidate Tier",
#       shape = "Candidate Tier"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
#       axis.title = element_text(size = 14, face = "bold"),
#       axis.text = element_text(size = 12),
#       legend.position = "bottom",
#       legend.title = element_text(size = 12, face = "bold"),
#       legend.text = element_text(size = 11),
#       plot.margin = margin(10, 10, 10, 10)
#     )
#   
#   # Plot 2: Welfare gains (relative to baseline)
#   welfare_gains <- aggregate_welfare_by_tier %>%
#     group_by(quality_tier) %>%
#     mutate(
#       baseline_welfare = mean_welfare_per_candidate[participation_rate == 0],
#       welfare_gain = mean_welfare_per_candidate - baseline_welfare
#     ) %>%
#     ungroup() %>%
#     filter(participation_rate > 0)
#   
#   p4 <- ggplot(welfare_gains, 
#                aes(x = participation_rate * 100, 
#                    y = welfare_gain,
#                    linetype = quality_tier,
#                    shape = quality_tier,
#                    group = quality_tier)) +
#     geom_line(linewidth = 1.2, color = "black") +
#     geom_point(size = 3, color = "black") +
#     geom_hline(yintercept = 0, linetype = "solid", color = "gray50", linewidth = 0.8) +
#     scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
#     scale_shape_manual(values = c(16, 17, 15, 18)) +
#     scale_x_continuous(
#       breaks = c(5, 20, 50, 90, 100),
#       labels = c("5", "20", "50", "90", "100")
#     ) +
#     scale_y_continuous(
#       expand = expansion(mult = c(0.05, 0.05))
#     ) +
#     labs(
#       x = "Market Participation Rate (%)",
#       y = "Welfare Gain vs. Baseline",
#       linetype = "Candidate Tier",
#       shape = "Candidate Tier"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
#       axis.title = element_text(size = 14, face = "bold"),
#       axis.text = element_text(size = 12),
#       legend.position = "bottom",
#       legend.title = element_text(size = 12, face = "bold"),
#       legend.text = element_text(size = 11),
#       plot.margin = margin(10, 10, 10, 10)
#     )
#   
#   # Combine plots
#   combined_plot <- p2 + p4 +
#     plot_layout(ncol = 2, guides = "collect") &
#     theme(legend.position = "bottom")
#   
#   list(
#     plot = combined_plot,
#     plot_matching = p2,
#     plot_gains = p4,
#     tests = tier_tests,
#     tier_summaries = tier_summaries,
#     gain_correlation = if(exists("gain_corr")) gain_corr else NULL,
#     aggregate_data = aggregate_welfare_by_tier,
#     candidate_data = welfare_data_by_tier
#   )
# }
# 
# # Generate figure
# cand_welfare_by_tier <- make_fig_candidate_welfare_by_tier(all_sim_results, year_filter = c(1, 10))
# 
# cand_welfare_by_tier$plot



make_fig_candidate_welfare_by_tier_revised <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # ============================================================================
  # Data Extraction (same as before)
  # ============================================================================
  
  all_candidates <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    all_sim_results[[rate_name]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2]) %>%
      mutate(participation_rate = rate)
  })
  
  welfare_data_by_tier <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    roster <- all_sim_results[[rate_name]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2])
    
    # In make_fig_candidate_welfare_by_tier_revised, change:
    matches <- all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))
    
    roster %>%
      left_join(
        matches %>% 
          group_by(year, cand_id) %>%
          slice(1) %>%  # ← ADD THIS: Take only first acceptance
          summarise(
            welfare = V_ij,  # ← CHANGE: Don't sum, just take value
            match_s_j = first(s_j),
            match_f_j = first(f_j),
            .groups = "drop"
          ),
        by = c("year", "cand_id")
      ) %>%
      mutate(
        welfare = coalesce(welfare, 0),
        matched = welfare > 0,
        participation_rate = rate
      )
  })
  
  # ============================================================================
  # Compute Metrics
  # ============================================================================
  
  # Unconditional metrics (for reference)
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
  
  # Conditional metrics (welfare given matched)
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
  conditional_gains <- conditional_welfare %>%
    group_by(quality_tier) %>%
    mutate(
      baseline_welfare = mean_welfare_if_matched[participation_rate == 0],
      welfare_gain = mean_welfare_if_matched - baseline_welfare,
      pct_gain = (mean_welfare_if_matched / baseline_welfare - 1) * 100
    ) %>%
    ungroup()
  
  # ============================================================================
  # Statistical Tests for Conditional Welfare
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
      # Welch's t-test (unpaired since different candidates may match)
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
    }
  }
  
  # ============================================================================
  # PANEL 1: Conditional Welfare E[W | Matched]
  # ============================================================================
  
  p1_conditional <- ggplot(conditional_welfare, 
                           aes(x = participation_rate * 100, 
                               y = mean_welfare_if_matched,
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
      # y = expression("Mean Candidate Utility "*bar(V)[ij]),
      y = expression(bar(V)[ij]),
      #y = expression(E*"[Welfare | Matched]"),
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
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # ============================================================================
  # PANEL 2: Conditional Welfare Gains vs Baseline
  # ============================================================================
  
  p2_gains <- conditional_gains %>%
    filter(participation_rate > 0) %>%
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
      # y = expression(Delta*" Candidate Utility "*bar(V)[ij]),
      y = expression(Delta*bar(V)[ij]),
      # y = expression(Delta*E*"[Welfare | Matched]"),
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
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # ============================================================================
  # PANEL 3: Preference Alignment f_j | Matched
  # ============================================================================
  
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
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # ============================================================================
  # Combined Plots
  # ============================================================================
  
  # Two-panel version (for main text)
  combined_2panel <- p1_conditional + p2_gains +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
  
  # Three-panel version (shows alignment as mechanism)
  combined_3panel <- p1_conditional + p2_gains + p3_alignment +
    plot_layout(ncol = 3, guides = "collect") &
    theme(legend.position = "bottom")
  
  # ============================================================================
  # Return Results
  # ============================================================================
  
  list(
    # Main plots
    plot = combined_2panel,
    plot_3panel = combined_3panel,
    
    # Individual panels
    plot_conditional = p1_conditional,
    plot_gains = p2_gains,
    plot_alignment = p3_alignment,
    
    # Data
    conditional_welfare = conditional_welfare,
    conditional_gains = conditional_gains,
    unconditional_welfare = unconditional_welfare,
    welfare_data = welfare_data_by_tier,
    
    # Tests
    conditional_tests = conditional_tests,
    conditional_summaries = conditional_summaries
  )
}

# =============================================================================
# Run the analysis
# =============================================================================

cand_welfare_revised <- make_fig_candidate_welfare_by_tier_revised(all_sim_results, year_filter = c(1, 10))

# Display
cand_welfare_revised$plot_conditional   # Two-panel
#cand_welfare_revised$plot_3panel # Three-panel: adds alignment

# Save
ggsave("fig_candidate_welfare_by_tier.pdf",
       cand_welfare_revised$plot_conditional,
       width = 6, height = 4, device = cairo_pdf)



###############################################################################

make_fig_candidate_by_participation <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Get baseline statistics for reference lines
  baseline_stats <- all_sim_results$baseline$cand_roster %>%
    filter(year >= year_filter[1], year <= year_filter[2]) %>%
    dplyr::select(year, cand_id) %>%
    left_join(
      all_sim_results$baseline$results %>%
        filter(year >= year_filter[1], year <= year_filter[2],
               strategy == "pairwise", accepted == 1) %>%
        mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j)) %>%
        group_by(year, cand_id) %>%
        summarise(welfare = sum(V_ij), .groups = "drop"),
      by = c("year", "cand_id")
    ) %>%
    mutate(
      welfare = coalesce(welfare, 0),
      matched = welfare > 0
    ) %>%
    summarise(
      baseline_mean_welfare = mean(welfare),
      baseline_matching_rate = mean(matched),
      baseline_mean_welfare_if_matched = mean(welfare[matched], na.rm = TRUE)
    )
  
  cat("\n=== BASELINE STATISTICS ===\n")
  cat(sprintf("Baseline mean welfare: %.4f\n", baseline_stats$baseline_mean_welfare))
  cat(sprintf("Baseline matching rate: %.2f%%\n", baseline_stats$baseline_matching_rate * 100))
  cat(sprintf("Baseline mean welfare (if matched): %.4f\n", baseline_stats$baseline_mean_welfare_if_matched))
  
  # For each participation rate, compare participating vs non-participating candidates
  comparison_data <- map_dfr(names(all_sim_results), function(rate_name) {
    if (rate_name == "baseline" || rate_name == "1") return(NULL)  # Skip extremes
    
    rate <- as.numeric(rate_name)
    
    # Get roster with participation status
    roster <- all_sim_results[[rate_name]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2])
    
    # Determine participation status
    if (!"participates" %in% names(roster)) {
      n_per_year <- roster %>% 
        group_by(year) %>% 
        summarise(n = n(), .groups = "drop") %>%
        pull(n) %>% unique()
      n_participants <- floor(n_per_year[1] * rate)
      
      roster <- roster %>%
        group_by(year) %>%
        mutate(participates = cand_id <= n_participants) %>%
        ungroup()
    }
    
    # Get matched welfare - take only first acceptance per candidate
    matched <- all_sim_results[[rate_name]]$results %>%
      filter(accepted == 1) %>%
      mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j)) %>%
      group_by(year, cand_id) %>%
      slice(1) %>%
      summarise(welfare = V_ij, .groups = "drop")
    
    # Join and compute statistics by participation status
    roster %>%
      left_join(matched, by = c("year", "cand_id")) %>%
      mutate(
        welfare = coalesce(welfare, 0),
        matched = welfare > 0
      ) %>%
      group_by(participates) %>%
      summarise(
        participation_rate = rate,
        n_candidates = n(),
        n_matched = sum(matched),
        matching_rate = mean(matched),
        mean_welfare = mean(welfare),
        se_welfare = sd(welfare) / sqrt(n()),
        mean_welfare_if_matched = mean(welfare[matched], na.rm = TRUE),
        se_welfare_if_matched = sd(welfare[matched], na.rm = TRUE) / sqrt(sum(matched)),
        .groups = "drop"
      ) %>%
      mutate(
        participation_status = ifelse(participates, "Participating", "Non-participating")
      )
  })
  
  # Print summary statistics
  cat("\n=== THEOREM 1 VALIDATION: Participating vs Non-Participating ===\n")
  print(comparison_data %>%
          dplyr::select(participation_rate, participation_status, n_candidates,
                        n_matched, matching_rate, mean_welfare, mean_welfare_if_matched) %>%
          arrange(participation_rate), n = 20)
  
  # Statistical tests
  cat("\n=== STATISTICAL TESTS (Within Each Market Rate) ===\n")
  for (rate in unique(comparison_data$participation_rate)) {
    part_data <- comparison_data %>% 
      filter(participation_rate == rate, participates == TRUE)
    non_part_data <- comparison_data %>% 
      filter(participation_rate == rate, participates == FALSE)
    
    if (nrow(part_data) > 0 && nrow(non_part_data) > 0) {
      # Test on mean welfare (all candidates)
      welfare_diff <- part_data$mean_welfare - non_part_data$mean_welfare
      se_diff <- sqrt(part_data$se_welfare^2 + non_part_data$se_welfare^2)
      t_stat <- welfare_diff / se_diff
      
      # Test on mean welfare conditional on matching
      welfare_matched_diff <- part_data$mean_welfare_if_matched - non_part_data$mean_welfare_if_matched
      se_matched_diff <- sqrt(part_data$se_welfare_if_matched^2 + non_part_data$se_welfare_if_matched^2)
      t_stat_matched <- welfare_matched_diff / se_matched_diff
      
      cat(sprintf("\nMarket Participation Rate = %.0f%%:\n", rate * 100))
      cat(sprintf("  [All Candidates]\n"))
      cat(sprintf("    Participating: n=%d, welfare=%.4f (SE=%.4f)\n",
                  part_data$n_candidates, part_data$mean_welfare, part_data$se_welfare))
      cat(sprintf("    Non-participating: n=%d, welfare=%.4f (SE=%.4f)\n",
                  non_part_data$n_candidates, non_part_data$mean_welfare, 
                  non_part_data$se_welfare))
      cat(sprintf("    Difference: %.4f (SE=%.4f, t=%.2f) %s\n",
                  welfare_diff, se_diff, t_stat,
                  ifelse(abs(t_stat) > 1.96, "✓ significant", "")))
      
      cat(sprintf("  [Matched Candidates Only]\n"))
      cat(sprintf("    Participating: n=%d, welfare=%.4f (SE=%.4f)\n",
                  part_data$n_matched, part_data$mean_welfare_if_matched, 
                  part_data$se_welfare_if_matched))
      cat(sprintf("    Non-participating: n=%d, welfare=%.4f (SE=%.4f)\n",
                  non_part_data$n_matched, non_part_data$mean_welfare_if_matched,
                  non_part_data$se_welfare_if_matched))
      cat(sprintf("    Difference: %.4f (SE=%.4f, t=%.2f) %s\n",
                  welfare_matched_diff, se_matched_diff, t_stat_matched,
                  ifelse(abs(t_stat_matched) > 1.96, "✓ significant", "")))
    }
  }
  
  # Plot 1: Mean welfare comparison (all candidates)
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
    labs(
      x = "Market Participation Rate (%)",
      y = "Mean Candidate Welfare",
      linetype = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Plot 2: Matching rate comparison
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
    labs(
      x = "Market Participation Rate (%)",
      y = "Matching Rate",
      linetype = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Plot 3: Mean welfare conditional on matching
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
    labs(
      x = "Market Participation Rate (%)",
      y = "Mean Welfare (If Matched)",
      linetype = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Combined 2-panel plot (original)
  combined_2panel <- (p1 | p2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Combined 3-panel plot (with conditional welfare)
  combined_3panel <- (p1 | p2 | p3) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  list(
    plot = combined_2panel,  # Default 2-panel
    plot_3panel = combined_3panel,  # 3-panel version
    plot_welfare = p1,
    plot_matching = p2,
    plot_welfare_conditional = p3,
    data = comparison_data,
    baseline_stats = baseline_stats
  )
}


# Example usage:
candidate_by_participation <- make_fig_candidate_by_participation(all_sim_results, year_filter = c(1, 10))
candidate_by_participation$plot
ggsave("fig_candidate_by_participation.pdf", candidate_by_participation$plot, width = 10, height = 4, device = cairo_pdf)



# =============================================================================
########################### DEPARTMENT FIGURES ################################
# =============================================================================
# =============================================================================
# FIGURE 2: Department Welfare by Informativeness (Theorem 4.2)
# =============================================================================

# =============================================================================
# CORRECTED FIGURE 2: Department Welfare by Informativeness (Theorem 4.2)
# =============================================================================

# =============================================================================
# FIGURE 2: Department Welfare by Informativeness (Revised for variable yearly hiring)
# =============================================================================

make_fig_department_welfare_by_tier <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Get department info (without h_j since it's now year-specific)
  departments <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier, s_j)
  
  # Get the yearly hiring schedule
  yearly_hiring_schedule <- all_sim_results[["baseline"]]$yearly_hiring_schedule
  
  years_in_filter <- year_filter[1]:year_filter[2]
  n_years <- length(years_in_filter)
  
  # Calculate total quota by tier from the yearly hiring schedule
  quota_by_tier <- tibble(
    dept_id = 1:nrow(departments)
  ) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(
      h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y])
    ) %>%
    group_by(prestige_tier) %>%
    summarise(
      total_quota = sum(h_j),
      n_depts = n_distinct(dept_id),
      n_hiring_events = sum(h_j > 0),
      .groups = "drop"
    )
  
  # Create department-year level data with year-specific h_j
  dept_year_quotas <- tibble(
    dept_id = 1:nrow(departments)
  ) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(
      h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y])
    )
  
  # Extract hired candidates
  dept_welfare_data <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      left_join(departments, by = "dept_id") %>%
      mutate(participation_rate = rate, prestige_tier = prestige_tier)
  })
  
  # Calculate welfare metrics by tier
  welfare_by_tier <- dept_welfare_data %>%
    group_by(participation_rate, prestige_tier) %>%
    summarise(
      n_hires = n(),
      total_utility = sum(U_true, na.rm = TRUE),
      mean_utility_per_hire = mean(U_true, na.rm = TRUE),
      se_per_hire = sd(U_true, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    left_join(quota_by_tier, by = "prestige_tier") %>%
    mutate(
      mean_utility_per_slot = ifelse(total_quota > 0, total_utility / total_quota, NA_real_),
      fill_rate = ifelse(total_quota > 0, n_hires / total_quota, NA_real_),
      prestige_tier = factor(prestige_tier,
                             levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
    )
  
  # For statistical testing, we need department-year level data with year-specific h_j
  dept_level_welfare <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    # Get all department-years with their year-specific quotas
    all_dept_years <- dept_year_quotas
    
    hires <- all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      group_by(dept_id, year) %>%
      summarise(
        total_utility = sum(U_true, na.rm = TRUE),
        n_hires = n(),
        .groups = "drop"
      )
    
    all_dept_years %>%
      left_join(hires, by = c("dept_id", "year")) %>%
      mutate(
        total_utility = coalesce(total_utility, 0),
        n_hires = coalesce(n_hires, 0L),
        # Only calculate welfare_per_slot when h_j > 0 (department was hiring)
        welfare_per_slot = ifelse(h_j > 0, total_utility / h_j, NA_real_),
        participation_rate = rate
      )
  })
  
  # Statistical tests by tier (only using department-years where h_j > 0)
  cat("\n=== DEPARTMENT WELFARE GAINS BY TIER ===\n")
  
  tier_tests <- list()
  tier_summaries <- list()
  
  for (tier in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
    # Filter to only department-years that were actually hiring
    baseline_dept <- dept_level_welfare %>%
      filter(participation_rate == 0, prestige_tier == tier, h_j > 0) %>%
      pull(welfare_per_slot)
    
    full_dept <- dept_level_welfare %>%
      filter(participation_rate == 1, prestige_tier == tier, h_j > 0) %>%
      pull(welfare_per_slot)
    
    if (length(baseline_dept) > 1 && length(full_dept) > 1) {
      test <- t.test(full_dept, baseline_dept, paired = TRUE)
      tier_tests[[tier]] <- test
      
      # Calculate fill rates only for hiring department-years
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
  
  # Test differential gains by prestige
  if (length(tier_summaries) >= 2) {
    gains_by_tier <- tibble(
      tier = names(tier_summaries),
      gain = map_dbl(tier_summaries, "gain")
    ) %>%
      left_join(
        departments %>%
          group_by(prestige_tier) %>%
          summarise(s_j_mean = mean(s_j), .groups = "drop") %>%
          rename(tier = prestige_tier),
        by = "tier"
      ) %>%
      arrange(s_j_mean)
    
    if (nrow(gains_by_tier) >= 3) {
      gain_corr <- cor.test(gains_by_tier$s_j_mean, gains_by_tier$gain,
                            method = "spearman")
      cat("Correlation(s_j, gain):", sprintf("%.3f (p = %.4f)\n",
                                             gain_corr$estimate, gain_corr$p.value))
    } else {
      gain_corr <- NULL
      cat("Insufficient tiers for correlation test\n")
    }
  } else {
    gain_corr <- NULL
  }
  
  # Plot 1: Total welfare by tier
  p1 <- ggplot(welfare_by_tier, aes(x = participation_rate * 100,
                                    y = total_utility,
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
      y = "Total Department Welfare",
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
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Plot 2: Welfare gains (relative to baseline)
  welfare_gains <- welfare_by_tier %>%
    group_by(prestige_tier) %>%
    mutate(
      baseline_welfare = total_utility[participation_rate == 0],
      welfare_gain = total_utility - baseline_welfare
    ) %>%
    ungroup() %>%
    filter(participation_rate > 0)
  
  p2 <- ggplot(welfare_gains, aes(x = participation_rate * 100,
                                  y = welfare_gain,
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
      y = "Welfare Gain vs. Baseline",
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
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Combine plots
  combined_plot <- p1 + p2 +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
  
  list(
    plot = combined_plot,
    plot_total_welfare = p1,
    plot_gains = p2,
    tests = tier_tests,
    tier_summaries = tier_summaries,
    gain_correlation = gain_corr,
    aggregate_data = welfare_by_tier,
    dept_level_data = dept_level_welfare,
    quota_by_tier = quota_by_tier
  )
}

# Generate figure
dept_welfare_results <- make_fig_department_welfare_by_tier(all_sim_results, year_filter = c(1, 10))

dept_welfare_results$plot
# Save with appropriate dimensions for two-column layout
ggsave("fig_department_welfare_combined.pdf",
       dept_welfare_results$plot,
       width = 12,
       height = 6,
       device = cairo_pdf)

# =============================================================================
# FIGURE 3: Monotonicity of Offer Probability (Lemma 4.1)
# =============================================================================

make_fig_department_welfare_by_tier_normalized <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Get department info (without h_j since it's now year-specific)
  departments <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier, s_j)
  
  # Get the yearly hiring schedule
  yearly_hiring_schedule <- all_sim_results[["baseline"]]$yearly_hiring_schedule
  
  years_in_filter <- year_filter[1]:year_filter[2]
  n_years <- length(years_in_filter)
  
  # Calculate total quota by tier from the yearly hiring schedule
  quota_by_tier <- tibble(
    dept_id = 1:nrow(departments)
  ) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(
      h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y])
    ) %>%
    group_by(prestige_tier) %>%
    summarise(
      total_quota = sum(h_j),
      n_depts = n_distinct(dept_id),
      n_hiring_events = sum(h_j > 0),
      .groups = "drop"
    )
  
  # Create department-year level data with year-specific h_j
  dept_year_quotas <- tibble(
    dept_id = 1:nrow(departments)
  ) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(
      h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y])
    )
  
  # Extract hired candidates
  dept_welfare_data <- purrr::map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      left_join(departments, by = "dept_id") %>%
      mutate(participation_rate = rate, prestige_tier = prestige_tier)
  })
  
  # Calculate welfare metrics by tier
  welfare_by_tier <- dept_welfare_data %>%
    group_by(participation_rate, prestige_tier) %>%
    summarise(
      n_hires = n(),
      total_utility = sum(U_true, na.rm = TRUE),
      mean_utility_per_hire = mean(U_true, na.rm = TRUE),
      se_per_hire = sd(U_true, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    left_join(quota_by_tier, by = "prestige_tier") %>%
    mutate(
      mean_utility_per_slot = ifelse(total_quota > 0, total_utility / total_quota, NA_real_),
      fill_rate = ifelse(total_quota > 0, n_hires / total_quota, NA_real_),
      # NEW: normalize by number of departments in the tier
      utility_per_dept = ifelse(n_depts > 0, total_utility / n_depts, NA_real_),
      prestige_tier = factor(prestige_tier,
                             levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
    )
  
  # For statistical testing, we need department-year level data with year-specific h_j
  dept_level_welfare <- purrr::map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    # Get all department-years with their year-specific quotas
    all_dept_years <- dept_year_quotas
    
    hires <- all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      group_by(dept_id, year) %>%
      summarise(
        total_utility = sum(U_true, na.rm = TRUE),
        n_hires = n(),
        .groups = "drop"
      )
    
    all_dept_years %>%
      left_join(hires, by = c("dept_id", "year")) %>%
      mutate(
        total_utility = dplyr::coalesce(total_utility, 0),
        n_hires = dplyr::coalesce(n_hires, 0L),
        # Only calculate welfare_per_slot when h_j > 0 (department was hiring)
        welfare_per_slot = ifelse(h_j > 0, total_utility / h_j, NA_real_),
        participation_rate = rate
      )
  })
  
  # Statistical tests by tier (only using department-years where h_j > 0)
  cat("\n=== DEPARTMENT WELFARE GAINS BY TIER ===\n")
  
  tier_tests <- list()
  tier_summaries <- list()
  
  for (tier in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
    # Filter to only department-years that were actually hiring
    baseline_dept <- dept_level_welfare %>%
      filter(participation_rate == 0, prestige_tier == tier, h_j > 0) %>%
      pull(welfare_per_slot)
    
    full_dept <- dept_level_welfare %>%
      filter(participation_rate == 1, prestige_tier == tier, h_j > 0) %>%
      pull(welfare_per_slot)
    
    if (length(baseline_dept) > 1 && length(full_dept) > 1) {
      test <- t.test(full_dept, baseline_dept, paired = TRUE)
      tier_tests[[tier]] <- test
      
      # Calculate fill rates only for hiring department-years
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
  
  # Test differential gains by prestige
  if (length(tier_summaries) >= 2) {
    gains_by_tier <- tibble(
      tier = names(tier_summaries),
      gain = purrr::map_dbl(tier_summaries, "gain")
    ) %>%
      left_join(
        departments %>%
          group_by(prestige_tier) %>%
          summarise(s_j_mean = mean(s_j), .groups = "drop") %>%
          rename(tier = prestige_tier),
        by = "tier"
      ) %>%
      arrange(s_j_mean)
    
    if (nrow(gains_by_tier) >= 3) {
      gain_corr <- cor.test(gains_by_tier$s_j_mean, gains_by_tier$gain,
                            method = "spearman")
      cat("Correlation(s_j, gain):", sprintf("%.3f (p = %.4f)\n",
                                             gain_corr$estimate, gain_corr$p.value))
    } else {
      gain_corr <- NULL
      cat("Insufficient tiers for correlation test\n")
    }
  } else {
    gain_corr <- NULL
  }
  
  # Plot 1: Welfare per department (tier-normalized)
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
      y = "Department Welfare per Department",
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
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Plot 2: Welfare gains per department (relative to baseline)
  welfare_gains <- welfare_by_tier %>%
    group_by(prestige_tier) %>%
    mutate(
      baseline_utility_per_dept = utility_per_dept[participation_rate == 0],
      welfare_gain_per_dept = utility_per_dept - baseline_utility_per_dept
    ) %>%
    ungroup() %>%
    filter(participation_rate > 0)
  
  p2 <- ggplot(welfare_gains, aes(x = participation_rate * 100,
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
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Combine plots
  combined_plot <- p1 + p2 +
    patchwork::plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
  
  list(
    plot = combined_plot,
    plot_total_welfare = p1,
    plot_gains = p2,
    tests = tier_tests,
    tier_summaries = tier_summaries,
    gain_correlation = gain_corr,
    aggregate_data = welfare_by_tier,
    dept_level_data = dept_level_welfare,
    quota_by_tier = quota_by_tier
  )
}

dept_welfare_results <- make_fig_department_welfare_by_tier_normalized(all_sim_results, year_filter = c(1, 10))

dept_welfare_results$plot_total_welfare
# Save with appropriate dimensions for two-column layout
ggsave("fig_department_welfare.pdf",
       dept_welfare_results$plot_total_welfare,
       width = 12,
       height = 6,
       device = cairo_pdf)


make_fig_offer_monotonicity <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Get ALL applications (not just offers)
  all_apps <- map_dfr(names(all_sim_results)[-1], function(rate_name) {
    rate <- as.numeric(rate_name)
    
    all_sim_results[[rate_name]]$diagnostics$applicant_level %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             considered == 1, strategy == "pairwise") %>%
      mutate(participation_rate = rate)
  })
  
  # Join with results to get offer information
  offer_data <- map_dfr(names(all_sim_results)[-1], function(rate_name) {
    rate <- as.numeric(rate_name)
    
    all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise") %>%
      dplyr::select(year, cand_id, dept_id, offered) %>%
      mutate(participation_rate = rate)
  })
  
  all_apps <- all_apps %>%
    left_join(offer_data, by = c("year", "cand_id", "dept_id", "participation_rate")) %>%
    mutate(offered = coalesce(offered, 0L))
  
  # Bin f_j
  breaks_f <- seq(0, 1, by = 0.05)
  
  offer_prob_by_f <- all_apps %>%
    mutate(
      f_bin = cut(f_j, breaks = breaks_f, include.lowest = TRUE),
      participation_pct = participation_rate * 100
    ) %>%
    group_by(participation_pct, f_bin) %>%
    summarise(
      n_apps = n(),
      p_offer = mean(offered == 1, na.rm = TRUE),
      se_offer = sqrt(p_offer * (1 - p_offer) / n_apps),
      .groups = "drop"
    ) %>%
    filter(n_apps >= 50) %>%
    mutate(
      f_mid = {
        b_id <- as.numeric(f_bin)
        width <- diff(breaks_f)[1]
        breaks_f[1] + (b_id - 0.5) * width
      }
    )
  
  # Test monotonicity using Spearman correlation
  cat("\n=== OFFER PROBABILITY MONOTONICITY TEST (Lemma 4.1) ===\n")
  
  for (rate_pct in unique(offer_prob_by_f$participation_pct)) {
    test_data <- offer_prob_by_f %>% filter(participation_pct == rate_pct)
    cor_test <- cor.test(test_data$f_mid, test_data$p_offer, method = "spearman")
    
    cat("\nParticipation rate", rate_pct, "%:\n")
    cat("  Spearman ρ:", cor_test$estimate, "\n")
    cat("  p-value:", cor_test$p.value, "\n")
  }
  
  # Plot
  p <- ggplot(offer_prob_by_f, aes(x = f_mid, y = p_offer,
                                   color = factor(participation_pct),
                                   group = participation_pct)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_smooth(method = "loess", se = FALSE, linetype = "dashed", alpha = 0.5) +
    scale_color_viridis_d(option = "plasma", end = 0.9) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      x = "Preference Alignment (f_j)",
      y = "P(offer | application)",
      title = "Monotonicity of Offer Probability in Alignment",
      subtitle = "Demonstrating Lemma 4.1: P(offer) is nondecreasing in f_j",
      color = "Participation\nRate"
    ) +
    theme_jasa()
  
  list(plot = p, data = offer_prob_by_f)
}

# =============================================================================
# Generate all new figures
# =============================================================================


# # Save individual plots
# # ggsave("fig_candidate_welfare_by_tier_main.pdf", 
# #        welfare_by_tier_results$plot_welfare, 
# #        width = 10, height = 6)
# # 
# # ggsave("fig_candidate_welfare_gains_by_tier.pdf", 
# #        welfare_by_tier_results$plot_gains, 
# #        width = 10, height = 6)
# 
# # Optional: Print summary table
# cat("\n=== WELFARE SUMMARY BY TIER ===\n")
# print(welfare_by_tier_results$aggregate_data %>%
#         filter(participation_rate %in% c(0, 1)) %>%
#         dplyr::select(participation_rate, quality_tier, 
#                       mean_welfare_per_candidate, matching_rate, 
#                       mean_welfare_per_match) %>%
#         arrange(quality_tier, participation_rate))


# Figure 2: Department welfare by tier

# Run corrected analysis
# dept_welfare_results <- make_fig_department_welfare_by_tier(all_sim_results, year_filter = c(1, 10))
# ggsave("fig_department_welfare_combined.pdf", dept_welfare_results$plot, width = 20, height = 8)
# # ggsave("fig_department_total_welfare.pdf", dept_welfare_results$plot_total_welfare, width = 10, height = 6)
# # ggsave("fig_department_welfare_per_slot.pdf", dept_welfare_results$plot_welfare_per_slot, width = 10, height = 6)
# print(dept_welfare_results$plot)
# print(dept_welfare_results$plot_total_welfare)
# print(dept_welfare_results$plot_welfare_per_slot)


print(dept_welfare_results$aggregate_data, n = 30)

# Figure 3: Offer monotonicity
(monotonicity_results <- make_fig_offer_monotonicity(all_sim_results, year_filter = c(1, 10)))
#ggsave("fig_offer_monotonicity.pdf", monotonicity_results$plot, width = 10, height = 6)

cat("\n✓ All new figures generated with statistical tests!\n")














# =============================================================================
# HIRING DISTRIBUTION HEATMAP (Revised for variable yearly hiring)
# =============================================================================
# =============================================================================
# HIRING DISTRIBUTION HEATMAP (Revised for variable yearly hiring)
# =============================================================================
make_fig_dept_hiring_heatmap <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Use baseline (ρ=0) and full participation (ρ=1.0)
  baseline_results <- all_sim_results[["baseline"]]$results %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                  strategy == "pairwise", accepted == 1)
  
  full_results <- all_sim_results[["1"]]$results %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                  strategy == "pairwise", accepted == 1)
  
  # Get department tiers (without h_j since it's now year-specific)
  departments <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier)
  
  # Get the yearly hiring schedule
  yearly_hiring_schedule <- all_sim_results[["baseline"]]$yearly_hiring_schedule
  
  # Get candidate tiers
  baseline_roster <- all_sim_results[["baseline"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  full_roster <- all_sim_results[["1"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  # Calculate total candidates by tier
  cand_totals <- baseline_roster %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier, 
                                 levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  
  # Calculate hiring quotas from the yearly hiring schedule
  years_in_filter <- year_filter[1]:year_filter[2]
  
  quota_totals <- tibble(
    dept_id = 1:nrow(departments)
  ) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(
      h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y])
    ) %>%
    group_by(prestige_tier) %>%
    summarise(
      quota = sum(h_j),
      n_hiring_events = sum(h_j > 0),
      .groups = "drop"
    ) %>%
    mutate(prestige_tier = factor(prestige_tier, 
                                  levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  
  # Combine baseline and full results
  combined_hires <- bind_rows(
    baseline_results %>%
      left_join(departments, by = "dept_id") %>%
      left_join(baseline_roster %>% dplyr::select(year, cand_id, quality_tier),
                by = c("year", "cand_id")) %>%
      mutate(scenario = "Baseline"),
    full_results %>%
      left_join(departments, by = "dept_id") %>%
      left_join(full_roster %>% dplyr::select(year, cand_id, quality_tier),
                by = c("year", "cand_id")) %>%
      mutate(scenario = "Questionnaire")
  )
  
  # Handle potential duplicate quality_tier columns from joins
  if ("quality_tier.y" %in% names(combined_hires)) {
    combined_hires$quality_tier <- combined_hires$quality_tier.y
  }
  
  # Handle potential duplicate prestige_tier columns from joins
  if ("prestige_tier.y" %in% names(combined_hires)) {
    combined_hires$prestige_tier <- combined_hires$prestige_tier.y
  }
  
  # Create heatmap data
  heatmap_data <- combined_hires %>%
    group_by(scenario, prestige_tier, quality_tier) %>%
    summarise(
      n_hires = n(),
      mean_utility = mean(U_true, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prestige_tier = factor(prestige_tier, 
                             levels = c("Tier 4", "Tier 3", "Tier 2", "Tier 1")),
      quality_tier = factor(quality_tier, 
                            levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
      scenario = factor(scenario, levels = c("Baseline", "Questionnaire"))
    ) %>%
    complete(scenario, prestige_tier, quality_tier, 
             fill = list(n_hires = 0, mean_utility = NA))
  
  # Create axis labels with totals
  x_labels <- cand_totals %>%
    mutate(label = paste0(quality_tier, "\n(n=", n_cand, ")")) %>%
    arrange(quality_tier) %>%
    dplyr::select(quality_tier, label) %>%
    deframe()
  
  # Y-axis labels now show actual quota from schedule
  y_labels <- quota_totals %>%
    mutate(
      prestige_tier_plot = factor(prestige_tier, 
                                  levels = c("Tier 4", "Tier 3", "Tier 2", "Tier 1")),
      label = paste0(prestige_tier, "\n(h=", quota, ")")
    ) %>%
    arrange(desc(prestige_tier_plot)) %>%
    dplyr::select(prestige_tier_plot, label) %>%
    deframe()
  
  # Print summary statistics
  cat("\n=== HIRING DISTRIBUTION SUMMARY ===\n")
  cat("Candidate totals by tier:\n")
  print(cand_totals)
  cat("\nHiring quota totals by department tier (from yearly schedule):\n")
  print(quota_totals)
  cat("\n")
  
  # Create plot
  p <- ggplot(heatmap_data, aes(x = quality_tier, y = prestige_tier, fill = mean_utility)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = ifelse(n_hires > 0, n_hires, "")),
              fontface = "bold", size = 5, vjust = 0.3, color = "white") +
    geom_text(aes(label = ifelse(is.finite(mean_utility),
                                 sprintf("bar(U)[ji]==%.3f", mean_utility), "")),
              size = 3.5, vjust = 1.8, color = "white", parse = TRUE) +
    facet_wrap(~ scenario, ncol = 2) +
    scale_fill_viridis_c(
      name = expression(atop(bar(U)[ji], "Mean Utility")), 
      limits = c(0, 1),
      na.value = "grey70", 
      option = "viridis",
      breaks = seq(0, 1, 0.25)
    ) +
    scale_x_discrete(labels = x_labels) +
    scale_y_discrete(labels = y_labels) +
    labs(
      x = "Candidate Quality Tier",
      y = "Department Prestige Tier"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 13, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = NA),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      panel.spacing = unit(1.5, "lines"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Calculate fill rates using actual quotas from schedule
  # Use character conversion for joining to avoid factor level issues
  fill_rate_summary <- heatmap_data %>%
    mutate(
      tier_for_join = as.character(prestige_tier)
    ) %>%
    group_by(scenario, tier_for_join) %>%
    summarise(
      total_hires = sum(n_hires),
      .groups = "drop"
    ) %>%
    left_join(
      quota_totals %>% 
        mutate(tier_for_join = as.character(prestige_tier)),
      by = "tier_for_join"
    ) %>%
    mutate(
      fill_rate = ifelse(quota > 0, total_hires / quota, NA_real_)
    ) %>%
    dplyr::select(scenario, prestige_tier, total_hires, quota, fill_rate)
  
  # Return plot and data
  list(
    plot = p,
    data = heatmap_data,
    candidate_totals = cand_totals,
    hiring_quotas = quota_totals,
    fill_rates = fill_rate_summary
  )
}


# =============================================================================
# INTERVIEW DISTRIBUTION HEATMAP (Revised for variable yearly hiring)
# =============================================================================
# =============================================================================
# INTERVIEW DISTRIBUTION HEATMAP (Revised for variable yearly hiring)
# =============================================================================
make_fig_dept_interview_heatmap <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Use baseline (ρ=0) and full participation (ρ=1.0)
  baseline_results <- all_sim_results[["baseline"]]$results %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                  strategy == "pairwise", interviewed == 1)
  
  full_results <- all_sim_results[["1"]]$results %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                  strategy == "pairwise", interviewed == 1)
  
  # Get department tiers (without k_j since it's now year-specific)
  departments <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier)
  
  # Get the yearly hiring schedule
  yearly_hiring_schedule <- all_sim_results[["baseline"]]$yearly_hiring_schedule
  
  # Get candidate tiers
  baseline_roster <- all_sim_results[["baseline"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  full_roster <- all_sim_results[["1"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  # Calculate total candidates by tier
  cand_totals <- baseline_roster %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier, 
                                 levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  
  # Calculate interview budgets from the yearly hiring schedule (k_j = 5 * h_j)
  years_in_filter <- year_filter[1]:year_filter[2]
  
  interview_budget_totals <- tibble(
    dept_id = 1:nrow(departments)
  ) %>%
    left_join(departments, by = "dept_id") %>%
    crossing(year = years_in_filter) %>%
    mutate(
      h_j = purrr::map2_int(dept_id, year, ~yearly_hiring_schedule[.x, .y]),
      k_j = 5L * h_j  # Interview budget is 5x hiring quota
    ) %>%
    group_by(prestige_tier) %>%
    summarise(
      budget = sum(k_j),
      n_interviewing_events = sum(k_j > 0),
      .groups = "drop"
    ) %>%
    mutate(prestige_tier = factor(prestige_tier, 
                                  levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  
  # Combine baseline and full results
  combined_interviews <- bind_rows(
    baseline_results %>%
      left_join(departments, by = "dept_id") %>%
      left_join(baseline_roster %>% dplyr::select(year, cand_id, quality_tier),
                by = c("year", "cand_id")) %>%
      mutate(scenario = "Baseline"),
    full_results %>%
      left_join(departments, by = "dept_id") %>%
      left_join(full_roster %>% dplyr::select(year, cand_id, quality_tier),
                by = c("year", "cand_id")) %>%
      mutate(scenario = "Questionnaire")
  )
  
  # Handle potential duplicate quality_tier columns from joins
  if ("quality_tier.y" %in% names(combined_interviews)) {
    combined_interviews$quality_tier <- combined_interviews$quality_tier.y
  }
  
  # Handle potential duplicate prestige_tier columns from joins
  if ("prestige_tier.y" %in% names(combined_interviews)) {
    combined_interviews$prestige_tier <- combined_interviews$prestige_tier.y
  }
  
  # Create heatmap data
  heatmap_data <- combined_interviews %>%
    group_by(scenario, prestige_tier, quality_tier) %>%
    summarise(
      n_interviews = n(),
      mean_utility = mean(U_true, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Keep original tier for joining later
      prestige_tier_orig = prestige_tier,
      prestige_tier = factor(prestige_tier, 
                             levels = c("Tier 4", "Tier 3", "Tier 2", "Tier 1")),
      quality_tier = factor(quality_tier, 
                            levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
      scenario = factor(scenario, levels = c("Baseline", "Questionnaire"))
    ) %>%
    complete(scenario, prestige_tier, quality_tier, 
             fill = list(n_interviews = 0, mean_utility = NA))
  
  # Create axis labels with totals
  x_labels <- cand_totals %>%
    mutate(label = paste0(quality_tier, "\n(n=", n_cand, ")")) %>%
    arrange(quality_tier) %>%
    dplyr::select(quality_tier, label) %>%
    deframe()
  
  # Y-axis labels now show actual budget from schedule
  y_labels <- interview_budget_totals %>%
    mutate(
      prestige_tier_plot = factor(prestige_tier, 
                                  levels = c("Tier 4", "Tier 3", "Tier 2", "Tier 1")),
      label = paste0(prestige_tier, "\n(k=", budget, ")")
    ) %>%
    arrange(desc(prestige_tier_plot)) %>%
    dplyr::select(prestige_tier_plot, label) %>%
    deframe()
  
  # Print summary statistics
  cat("\n=== INTERVIEW DISTRIBUTION SUMMARY ===\n")
  cat("Candidate totals by tier:\n")
  print(cand_totals)
  cat("\nInterview budget totals by department tier (from yearly schedule):\n")
  print(interview_budget_totals)
  cat("\n")
  
  # Create plot
  p <- ggplot(heatmap_data, aes(x = quality_tier, y = prestige_tier, fill = mean_utility)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = ifelse(n_interviews > 0, n_interviews, "")),
              fontface = "bold", size = 5, vjust = 0.3, color = "white") +
    geom_text(aes(label = ifelse(is.finite(mean_utility),
                                 sprintf("bar(U)[ji]==%.3f", mean_utility), "")),
              size = 3.5, vjust = 1.8, color = "white", parse = TRUE) +
    facet_wrap(~ scenario, ncol = 2) +
    scale_fill_viridis_c(
      name = expression(atop(bar(U)[ji], "Mean Utility")), 
      limits = c(0, 1),
      na.value = "grey70", 
      option = "viridis",
      breaks = seq(0, 1, 0.25)
    ) +
    scale_x_discrete(labels = x_labels) +
    scale_y_discrete(labels = y_labels) +
    labs(
      x = "Candidate Quality Tier",
      y = "Department Prestige Tier"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 13, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = NA),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      panel.spacing = unit(1.5, "lines"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Calculate interview fill rates - use the prestige_tier directly as character for joining
  interview_fill_summary <- heatmap_data %>%
    mutate(
      # Convert back to standard tier levels for joining
      tier_for_join = as.character(prestige_tier)
    ) %>%
    group_by(scenario, tier_for_join) %>%
    summarise(
      total_interviews = sum(n_interviews),
      .groups = "drop"
    ) %>%
    left_join(
      interview_budget_totals %>% 
        mutate(tier_for_join = as.character(prestige_tier)),
      by = "tier_for_join"
    ) %>%
    mutate(
      utilization_rate = ifelse(budget > 0, total_interviews / budget, NA_real_)
    ) %>%
    dplyr::select(scenario, prestige_tier, total_interviews, budget, utilization_rate)
  
  # Return plot and data
  list(
    plot = p,
    data = heatmap_data,
    candidate_totals = cand_totals,
    interview_budgets = interview_budget_totals,
    utilization_rates = interview_fill_summary
  )
}


(interview_heatmap_results <- make_fig_dept_interview_heatmap(all_sim_results, year_filter = c(1, 10)))

# interview_heatmap_results$plot
# Save with appropriate dimensions for two-panel heatmap
# ggsave("fig_dept_interview_heatmap.pdf", 
#        interview_heatmap_results$plot, 
#        width = 10, 
#        height = 5, 
#        device = cairo_pdf)
cat("\nInterview counts by scenario and tier combination:\n")
print(interview_heatmap_results$data %>% 
        filter(n_interviews > 0) %>%
        arrange(scenario, desc(prestige_tier), quality_tier))


# all_sim_results <- all_sim_results_10L
# all_sim_results <- all_sim_results_20L
# all_sim_results <- all_sim_results_100L

(hiring_heatmap_results <- make_fig_dept_hiring_heatmap(all_sim_results, year_filter = c(1, 10)))

#hiring_heatmap_results$plot
# # Save with appropriate dimensions for two-panel heatmap
ggsave("fig_dept_hiring_heatmap.pdf",
       hiring_heatmap_results$plot,
       width = 10,
       height = 5,
       device = cairo_pdf)


cat("\nFill rates by scenario and department tier:\n")
print(hiring_heatmap_results$fill_rates)

cat("\nHire counts by scenario and tier combination:\n")
print(hiring_heatmap_results$data %>% 
        filter(n_hires > 0) %>%
        arrange(scenario, desc(prestige_tier), quality_tier))

map_dfr(
  c("baseline", "1"),
  function(rate_name) {
    all_sim_results[[rate_name]]$results %>%
      filter(accepted == 1) %>%
      group_by(dept_tier) %>%
      summarise(
        mean_utility = mean(U_true, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(participation = rate_name)
  }
) %>%
  pivot_wider(
    names_from  = participation,
    values_from = mean_utility,
    names_prefix = "mean_"
  ) %>%
  mutate(
    pct_change = 100 * (mean_1 - mean_baseline) / mean_baseline
  ) %>%
  arrange(dept_tier)



# =============================================================================
# INTERVIEW DISTRIBUTION HEATMAP
# =============================================================================

# make_fig_dept_interview_heatmap <- function(all_sim_results, year_filter = c(1, 10)) {
#   
#   # Use baseline (ρ=0) and full participation (ρ=1.0)
#   baseline_results <- all_sim_results[["baseline"]]$results %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2],
#                   strategy == "pairwise", interviewed == 1)
#   
#   full_results <- all_sim_results[["1"]]$results %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2],
#                   strategy == "pairwise", interviewed == 1)
#   
#   # Get department tiers with k_j (interview budget)
#   departments <- all_sim_results[[1]]$departments %>%
#     dplyr::select(dept_id, prestige_tier, k_j)
#   
#   # Get candidate tiers
#   baseline_roster <- all_sim_results[["baseline"]]$cand_roster %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2])
#   
#   full_roster <- all_sim_results[["1"]]$cand_roster %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2])
#   
#   # Calculate total candidates and interview budgets by tier
#   cand_totals <- baseline_roster %>%
#     count(quality_tier, name = "n_cand") %>%
#     mutate(quality_tier = factor(quality_tier, 
#                                  levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
#   
#   interview_budget_totals <- departments %>%
#     group_by(prestige_tier) %>%
#     summarise(budget = sum(k_j) * length(year_filter[1]:year_filter[2]), 
#               .groups = "drop") %>%
#     mutate(prestige_tier = factor(prestige_tier, 
#                                   levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
#   
#   # Combine baseline and full results
#   combined_interviews <- bind_rows(
#     baseline_results %>%
#       left_join(departments, by = "dept_id") %>%
#       left_join(baseline_roster %>% dplyr::select(year, cand_id, quality_tier),
#                 by = c("year", "cand_id")) %>%
#       mutate(scenario = "Baseline"),
#     full_results %>%
#       left_join(departments, by = "dept_id") %>%
#       left_join(full_roster %>% dplyr::select(year, cand_id, quality_tier),
#                 by = c("year", "cand_id")) %>%
#       mutate(scenario = "Questionnaire")
#   )
#   
#   # Handle potential duplicate quality_tier columns from joins
#   if ("quality_tier.y" %in% names(combined_interviews)) {
#     combined_interviews$quality_tier <- combined_interviews$quality_tier.y
#   }
#   
#   # Create heatmap data
#   heatmap_data <- combined_interviews %>%
#     group_by(scenario, prestige_tier, quality_tier) %>%
#     summarise(
#       n_interviews = n(),
#       mean_utility = mean(U_true, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       prestige_tier = factor(prestige_tier, 
#                              levels = c("Tier 4", "Tier 3", "Tier 2", "Tier 1")),
#       quality_tier = factor(quality_tier, 
#                             levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
#       scenario = factor(scenario, levels = c("Baseline", "Questionnaire"))
#     ) %>%
#     complete(scenario, prestige_tier, quality_tier, 
#              fill = list(n_interviews = 0, mean_utility = NA))
#   
#   # Create axis labels with totals
#   x_labels <- cand_totals %>%
#     mutate(label = paste0(quality_tier, "\n(n=", n_cand, ")")) %>%
#     arrange(quality_tier) %>%
#     dplyr::select(quality_tier, label) %>%
#     deframe()
#   
#   y_labels <- interview_budget_totals %>%
#     mutate(label = paste0(prestige_tier, "\n(k=", budget, ")")) %>%
#     arrange(desc(prestige_tier)) %>%
#     dplyr::select(prestige_tier, label) %>%
#     deframe()
#   
#   # Print summary statistics
#   cat("\n=== INTERVIEW DISTRIBUTION SUMMARY ===\n")
#   cat("Candidate totals by tier:\n")
#   print(cand_totals)
#   cat("\nInterview budget totals by department tier:\n")
#   print(interview_budget_totals)
#   cat("\n")
#   
#   # Create plot
#   p <- ggplot(heatmap_data, aes(x = quality_tier, y = prestige_tier, fill = mean_utility)) +
#     geom_tile(color = "white", linewidth = 1) +
#     geom_text(aes(label = ifelse(n_interviews > 0, n_interviews, "")),
#               fontface = "bold", size = 5, vjust = 0.3, color = "white") +
#     geom_text(aes(label = ifelse(is.finite(mean_utility),
#                                  sprintf("bar(U)[ji]==%.3f", mean_utility), "")),
#               size = 3.5, vjust = 1.8, color = "white", parse = TRUE) +
#     facet_wrap(~ scenario, ncol = 2) +
#     scale_fill_viridis_c(
#       name = expression(atop(bar(U)[ji], "Mean Utility")), 
#       limits = c(0, 1),
#       na.value = "grey70", 
#       option = "viridis",
#       breaks = seq(0, 1, 0.25)
#     ) +
#     scale_x_discrete(labels = x_labels) +
#     scale_y_discrete(labels = y_labels) +
#     labs(
#       x = "Candidate Quality Tier",
#       y = "Department Prestige Tier"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.grid = element_blank(),
#       axis.text.x = element_text(size = 11),
#       axis.text.y = element_text(size = 11),
#       axis.title = element_text(size = 14, face = "bold"),
#       strip.text = element_text(size = 13, face = "bold"),
#       strip.background = element_rect(fill = "gray95", color = NA),
#       legend.position = "right",
#       legend.title = element_text(size = 12, face = "bold"),
#       legend.text = element_text(size = 11),
#       panel.spacing = unit(1.5, "lines"),
#       plot.margin = margin(10, 10, 10, 10)
#     )
#   
#   # Return plot and data
#   list(
#     plot = p,
#     data = heatmap_data,
#     candidate_totals = cand_totals,
#     interview_budgets = interview_budget_totals
#   )
# }

# # Generate figure
# interview_heatmap_results <- make_fig_dept_interview_heatmap(all_sim_results, year_filter = c(1, 10))
# 
# interview_heatmap_results$plot
# # Save with appropriate dimensions for two-panel heatmap
# ggsave("fig_dept_interview_heatmap.pdf", 
#        interview_heatmap_results$plot, 
#        width = 10, 
#        height = 5, 
#        device = cairo_pdf)
# 
# 
# cat("\nInterview counts by scenario and tier combination:\n")
# print(interview_heatmap_results$data %>% 
#         filter(n_interviews > 0) %>%
#         arrange(scenario, desc(prestige_tier), quality_tier))




# =============================================================================
# HIRING DISTRIBUTION HEATMAP
# =============================================================================
# make_fig_dept_hiring_heatmap <- function(all_sim_results, year_filter = c(1, 10)) {
#   
#   # Use baseline (ρ=0) and full participation (ρ=1.0)
#   baseline_results <- all_sim_results[["baseline"]]$results %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2],
#                   strategy == "pairwise", accepted == 1)
#   
#   full_results <- all_sim_results[["1"]]$results %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2],
#                   strategy == "pairwise", accepted == 1)
#   
#   # Get department tiers with h_j (hiring quota)
#   departments <- all_sim_results[[1]]$departments %>%
#     dplyr::select(dept_id, prestige_tier, h_j)
#   
#   # Get candidate tiers
#   baseline_roster <- all_sim_results[["baseline"]]$cand_roster %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2])
#   
#   full_roster <- all_sim_results[["1"]]$cand_roster %>%
#     dplyr::filter(year >= year_filter[1], year <= year_filter[2])
#   
#   # Calculate total candidates and hiring quotas by tier
#   cand_totals <- baseline_roster %>%
#     count(quality_tier, name = "n_cand") %>%
#     mutate(quality_tier = factor(quality_tier, 
#                                  levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
#   
#   quota_totals <- departments %>%
#     group_by(prestige_tier) %>%
#     summarise(quota = sum(h_j) * length(year_filter[1]:year_filter[2]), 
#               .groups = "drop") %>%
#     mutate(prestige_tier = factor(prestige_tier, 
#                                   levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
#   
#   # Combine baseline and full results
#   combined_hires <- bind_rows(
#     baseline_results %>%
#       left_join(departments, by = "dept_id") %>%
#       left_join(baseline_roster %>% dplyr::select(year, cand_id, quality_tier),
#                 by = c("year", "cand_id")) %>%
#       mutate(scenario = "Baseline"),
#     full_results %>%
#       left_join(departments, by = "dept_id") %>%
#       left_join(full_roster %>% dplyr::select(year, cand_id, quality_tier),
#                 by = c("year", "cand_id")) %>%
#       mutate(scenario = "Questionnaire")
#   )
#   
#   # Handle potential duplicate quality_tier columns from joins
#   if ("quality_tier.y" %in% names(combined_hires)) {
#     combined_hires$quality_tier <- combined_hires$quality_tier.y
#   }
#   
#   # Create heatmap data
#   heatmap_data <- combined_hires %>%
#     group_by(scenario, prestige_tier, quality_tier) %>%
#     summarise(
#       n_hires = n(),
#       mean_utility = mean(U_true, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       prestige_tier = factor(prestige_tier, 
#                              levels = c("Tier 4", "Tier 3", "Tier 2", "Tier 1")),
#       quality_tier = factor(quality_tier, 
#                             levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
#       scenario = factor(scenario, levels = c("Baseline", "Questionnaire"))
#     ) %>%
#     complete(scenario, prestige_tier, quality_tier, 
#              fill = list(n_hires = 0, mean_utility = NA))
#   
#   # Create axis labels with totals
#   x_labels <- cand_totals %>%
#     mutate(label = paste0(quality_tier, "\n(n=", n_cand, ")")) %>%
#     arrange(quality_tier) %>%
#     dplyr::select(quality_tier, label) %>%
#     deframe()
#   
#   y_labels <- quota_totals %>%
#     mutate(label = paste0(prestige_tier, "\n(h=", quota, ")")) %>%
#     arrange(desc(prestige_tier)) %>%
#     dplyr::select(prestige_tier, label) %>%
#     deframe()
#   
#   # Print summary statistics
#   cat("\n=== HIRING DISTRIBUTION SUMMARY ===\n")
#   cat("Candidate totals by tier:\n")
#   print(cand_totals)
#   cat("\nHiring quota totals by department tier:\n")
#   print(quota_totals)
#   cat("\n")
#   
#   # Create plot
#   p <- ggplot(heatmap_data, aes(x = quality_tier, y = prestige_tier, fill = mean_utility)) +
#     geom_tile(color = "white", linewidth = 1) +
#     geom_text(aes(label = ifelse(n_hires > 0, n_hires, "")),
#               fontface = "bold", size = 5, vjust = 0.3, color = "white") +
#     geom_text(aes(label = ifelse(is.finite(mean_utility),
#                                  sprintf("bar(U)[ji]==%.3f", mean_utility), "")),
#               size = 3.5, vjust = 1.8, color = "white", parse = TRUE) +
#     facet_wrap(~ scenario, ncol = 2) +
#     scale_fill_viridis_c(
#       name = expression(atop(bar(U)[ji], "Mean Utility")), 
#       limits = c(0, 1),
#       na.value = "grey70", 
#       option = "viridis",
#       breaks = seq(0, 1, 0.25)
#     ) +
#     scale_x_discrete(labels = x_labels) +
#     scale_y_discrete(labels = y_labels) +
#     labs(
#       x = "Candidate Quality Tier",
#       y = "Department Prestige Tier"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.grid = element_blank(),
#       axis.text.x = element_text(size = 11),
#       axis.text.y = element_text(size = 11),
#       axis.title = element_text(size = 14, face = "bold"),
#       strip.text = element_text(size = 13, face = "bold"),
#       strip.background = element_rect(fill = "gray95", color = NA),
#       legend.position = "right",
#       legend.title = element_text(size = 12, face = "bold"),
#       legend.text = element_text(size = 11),
#       panel.spacing = unit(1.5, "lines"),
#       plot.margin = margin(10, 10, 10, 10)
#     )
#   
#   # Calculate fill rates
#   fill_rate_summary <- heatmap_data %>%
#     left_join(quota_totals, by = "prestige_tier") %>%
#     group_by(scenario, prestige_tier) %>%
#     summarise(
#       total_hires = sum(n_hires),
#       total_quota = first(quota),
#       fill_rate = total_hires / total_quota,
#       .groups = "drop"
#     )
#   
#   # Return plot and data
#   list(
#     plot = p,
#     data = heatmap_data,
#     candidate_totals = cand_totals,
#     hiring_quotas = quota_totals,
#     fill_rates = fill_rate_summary
#   )
# }

# Generate figure
# hiring_heatmap_results <- make_fig_dept_hiring_heatmap(all_sim_results, year_filter = c(1, 10))
# 
# hiring_heatmap_results$plot
# # Save with appropriate dimensions for two-panel heatmap
# ggsave("fig_dept_hiring_heatmap.pdf", 
#        hiring_heatmap_results$plot, 
#        width = 10, 
#        height = 5, 
#        device = cairo_pdf)
# 
# 
# cat("\nFill rates by scenario and department tier:\n")
# print(hiring_heatmap_results$fill_rates)
# 
# cat("\nHire counts by scenario and tier combination:\n")
# print(hiring_heatmap_results$data %>% 
#         filter(n_hires > 0) %>%
#         arrange(scenario, desc(prestige_tier), quality_tier))






# =============================================================================
# DEPARTMENT METRICS BY TIER (Yield and Utility)
# =============================================================================

make_dept_metrics_by_tier <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Combine baseline and full participation results
  results <- bind_rows(
    all_sim_results[["baseline"]]$results %>%
      mutate(scenario = "Baseline"),
    all_sim_results[["1"]]$results %>%
      mutate(scenario = "Questionnaire")
  )
  
  results <- results %>%
    filter(year >= year_filter[1], year <= year_filter[2],
           strategy == "pairwise")  # Only pairwise strategy now
  
  strategy_labels <- c(
    "Questionnaire" = "Questionnaire",
    "Baseline" = "No Questionnaire (baseline)"
  )
  
  okabe_ito <- c(
    "Questionnaire" = "#0072B2",
    "No Questionnaire (baseline)" = "#D55E00"
  )
  
  dept_tiers <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier)
  
  # Compute metrics
  metrics <- results %>%
    left_join(dept_tiers, by = "dept_id") %>%
    group_by(scenario, prestige_tier) %>%
    summarise(
      n_offers = sum(offered, na.rm = TRUE),
      n_accepts = sum(accepted, na.rm = TRUE),
      yield = n_accepts / n_offers,
      se_yield = sqrt(yield * (1 - yield) / n_offers),
      mean_util = mean(U_true[accepted == 1], na.rm = TRUE),
      se_util = sd(U_true[accepted == 1], na.rm = TRUE) / 
        sqrt(sum(accepted == 1, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      prestige_tier = factor(prestige_tier, 
                             levels = c("Tier 1","Tier 2","Tier 3","Tier 4")),
      scenario = factor(scenario, levels = c("Questionnaire", "Baseline"),
                        labels = strategy_labels)
    )
  
  # Yield plot
  p_yield <- ggplot(metrics, aes(x = prestige_tier, y = yield, 
                                 fill = scenario, group = scenario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(ymin = pmax(0, yield - 1.96*se_yield), 
                      ymax = pmin(1, yield + 1.96*se_yield)),
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = okabe_ito) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(x = "Department Prestige Tier", 
         y = "Offer Acceptance Rate (Yield)",
         title = "Acceptance Yield by Department Tier") +
    theme_jasa()
  
  # # Utility plot
  # p_utility <- ggplot(metrics, aes(x = prestige_tier, y = mean_util, 
  #                                  fill = scenario, group = scenario)) +
  #   geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
  #   geom_errorbar(aes(ymin = mean_util - 1.96*se_util, 
  #                     ymax = mean_util + 1.96*se_util),
  #                 position = position_dodge(width = 0.8), width = 0.25) +
  #   scale_fill_manual(values = okabe_ito) +
  #   labs(x = "Department Prestige Tier", 
  #        y = "Mean Utility of Hires",
  #        title = "Hiring Utility by Department Tier") +
  #   theme_jasa()
  # 
  # Combine
  # combined_plot <- p_yield + p_utility +
  #   plot_layout(guides = "collect") &
  #   theme(legend.position = "bottom")
  
  list(
    plot = p_yield,
    #plot_yield = p_yield,
    #plot_utility = p_utility,
    data = metrics
  )
}

# Generate and save
dept_metrics_results <- make_dept_metrics_by_tier(
  all_sim_results, 
  year_filter = c(1, 10)
)

print(dept_metrics_results$plot)
print(dept_metrics_results$data)
#ggsave("fig_dept_metrics_by_tier.pdf", dept_metrics_results$plot, width = 12, height = 5)

# =============================================================================
# QUALITY-FIT TRADEOFF SCATTER
# =============================================================================

make_quality_fit_scatter <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Combine baseline and full participation results
  results <- bind_rows(
    all_sim_results[["baseline"]]$results %>%
      mutate(scenario = "Baseline"),
    all_sim_results[["1"]]$results %>%
      mutate(scenario = "Questionnaire")
  )
  
  results <- results %>%
    filter(year >= year_filter[1], year <= year_filter[2], 
           accepted == 1,
           strategy == "pairwise")
  
  strategy_labels <- c(
    "Questionnaire" = "Questionnaire",
    "Baseline" = "No Questionnaire (baseline)"
  )
  
  okabe_ito <- c(
    "Questionnaire" = "#0072B2",
    "No Questionnaire (baseline)" = "#D55E00"
  )
  
  dept_tiers <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier)
  
  plot_data <- results %>%
    left_join(dept_tiers, by = "dept_id") %>%
    mutate(
      scenario = factor(scenario, levels = c("Questionnaire", "Baseline"),
                        labels = strategy_labels),
      prestige_tier = factor(prestige_tier, 
                             levels = c("Tier 1","Tier 2","Tier 3","Tier 4"))
    )
  
  p <- ggplot(plot_data, aes(x = v_i_bar, y = f_j, color = scenario)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_density_2d(alpha = 0.6, linewidth = 0.5) +
    facet_wrap(~ prestige_tier, nrow = 2) +
    scale_color_manual(values = okabe_ito) +
    labs(x = "Candidate Quality (v̄ᵢ)", 
         y = "Preference Alignment (f_j)",
         title = "Quality-Fit Profile of Hires by Department Tier",
         subtitle = "Points show individual hires; contours show density",
         color = NULL) +
    theme_jasa() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 11, face = "bold"))
  
  list(
    plot = p,
    data = plot_data
  )
}

# Generate and save
quality_fit_results <- make_quality_fit_scatter(
  all_sim_results, 
  year_filter = c(1, 10)
)

print(quality_fit_results$plot)
#ggsave("fig_quality_fit_scatter.pdf", quality_fit_results$plot, width = 10, height = 8)

# =============================================================================
# INTERVIEW PROBABILITY STRATIFIED BY TIERS
# =============================================================================

make_candidate_interview_prob_stratified <- function(all_sim_results, 
                                                     year_filter = c(1, 10)) {
  
  # Combine baseline and full participation diagnostics
  apps <- bind_rows(
    all_sim_results[["baseline"]]$diagnostics$applicant_level %>%
      mutate(scenario = "Baseline"),
    all_sim_results[["1"]]$diagnostics$applicant_level %>%
      mutate(scenario = "Questionnaire")
  )
  
  strategy_labels <- c(
    "Questionnaire" = "Questionnaire",
    "Baseline" = "No Questionnaire (baseline)"
  )
  
  okabe_ito <- c(
    "Questionnaire" = "#0072B2",
    "No Questionnaire (baseline)" = "#D55E00"
  )
  
  # Filter applications
  apps <- apps %>%
    filter(year >= year_filter[1], year <= year_filter[2],
           !is.na(scenario), 
           considered == 1,
           strategy == "pairwise")
  
  # Get candidate tiers
  cand_roster <- all_sim_results[["baseline"]]$cand_roster %>%
    filter(year >= year_filter[1], year <= year_filter[2])
  
  apps <- apps %>%
    left_join(cand_roster %>% dplyr::select(year, cand_id, quality_tier),
              by = c("year", "cand_id"))
  
  # Get department tiers
  apps <- apps %>%
    left_join(all_sim_results[[1]]$departments %>% 
                dplyr::select(dept_id, prestige_tier),
              by = "dept_id")
  
  # Bin alignment
  breaks_f <- seq(0, 1, by = 0.05)
  
  interview_prob <- apps %>%
    mutate(
      f_bin = cut(f_j, breaks = breaks_f, include.lowest = TRUE),
      scenario = factor(scenario, levels = c("Questionnaire", "Baseline"),
                        labels = strategy_labels),
      quality_tier = factor(quality_tier, 
                            levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
      prestige_tier = factor(prestige_tier, 
                             levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
    ) %>%
    group_by(scenario, quality_tier, prestige_tier, f_bin) %>%
    summarise(
      n_apps = n(),
      p_int = mean(interviewed == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_apps >= 20) %>%
    mutate(
      f_mid = {
        b_id <- as.numeric(f_bin)
        width <- diff(breaks_f)[1]
        breaks_f[1] + (b_id - 0.5) * width
      }
    )
  
  # Create faceted plot
  p <- ggplot(interview_prob, aes(x = f_mid, y = p_int,
                                  color = scenario, 
                                  linetype = scenario, 
                                  shape = scenario)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    facet_grid(quality_tier ~ prestige_tier,
               labeller = labeller(
                 quality_tier = function(x) paste("Cand:", x),
                 prestige_tier = function(x) paste("Dept:", x)
               )) +
    scale_color_manual(values = okabe_ito) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_shape_manual(values = c(16, 17)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    labs(x = "Preference Alignment (f_j)",
         y = "P(interviewed | considered)",
         title = "Interview Probability by Preference Alignment",
         subtitle = "Stratified by Candidate and Department Tiers",
         color = NULL,
         linetype = NULL,
         shape = NULL) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 9, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  
  list(
    plot = p,
    data = interview_prob
  )
}

# Generate and save
interview_prob_results <- make_candidate_interview_prob_stratified(
  all_sim_results, 
  year_filter = c(1, 10)
)

print(interview_prob_results$plot)
# ggsave("fig_interview_prob_stratified.pdf", 
#        interview_prob_results$plot, 
#        width = 12, height = 10)
print(interview_prob_results$data, n = 300)
