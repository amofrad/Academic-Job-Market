# =============================================================================
# =============================================================================
# ACADEMIC JOB MARKET SIMULATION - REVISED WITH ADAPTIVE STRATEGIES
# =============================================================================
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
# UTILS (unchanged)
# =============================================================================

norm01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (any(!is.finite(rng)) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}
`%||%` <- function(x,y) if (is.null(x)) y else x

state_to_region <- function(state_or_location) {
  x <- toupper(state_or_location); x[is.na(x)] <- ""
  abbr <- stringr::str_match(x, ",\\s*([A-Z]{2})(\\b|$)")[, 2]
  token <- ifelse(!is.na(abbr), abbr, x)
  NE <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA","DC",
          "CONNECTICUT","MAINE","MASSACHUSETTS","NEW HAMPSHIRE","RHODE ISLAND","VERMONT",
          "NEW JERSEY","NEW YORK","PENNSYLVANIA","DISTRICT OF COLUMBIA","WASHINGTON DC")
  MW <- c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD",
          "ILLINOIS","INDIANA","MICHIGAN","OHIO","WISCONSIN","IOWA","KANSAS",
          "MINNESOTA","MISSOURI","NEBRASKA","NORTH DAKOTA","SOUTH DAKOTA")
  S  <- c("DE","FL","GA","MD","NC","SC","VA","WV","AL","KY","MS","TN","AR","LA","OK","TX",
          "DELAWARE","FLORIDA","GEORGIA","MARYLAND","NORTH CAROLINA","SOUTH CAROLINA",
          "VIRGINIA","WEST VIRGINIA","ALABAMA","KENTUCKY","MISSISSIPPI","TENNESSEE",
          "ARKANSAS","LOUISIANA","OKLAHOMA","TEXAS")
  W  <- c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA",
          "ARIZONA","COLORADO","IDAHO","MONTANA","NEVADA","NEW MEXICO","UTAH","WYOMING",
          "ALASKA","CALIFORNIA","HAWAII","OREGON","WASHINGTON")
  dplyr::case_when(
    token %in% NE ~ "northeast",
    token %in% MW ~ "midwest",
    token %in% S  ~ "south",
    token %in% W  ~ "west",
    TRUE          ~ "other"
  )
}

assign_tiers_from_quantiles <- function(x, cutpoints = c(0.10, 0.25, 0.50),
                                        tier_labels = c("Tier 1","Tier 2","Tier 3","Tier 4")) {
  stopifnot(length(cutpoints) == 3, length(tier_labels) == 4)
  qq <- stats::quantile(x, probs = 1 - cutpoints, na.rm = TRUE)
  cut(x,
      breaks = c(-Inf, qq[3], qq[2], qq[1], Inf),
      labels = rev(tier_labels), include.lowest = TRUE, right = TRUE)
}

# =============================================================================
# 1) LOAD + COMBINE LINKEDIN DATA (unchanged)
# =============================================================================

job_postings_path    <- "postings.csv"
companies_path       <- "companies.csv"
employee_counts_path <- "employee_counts.csv"
benefits_path        <- "benefits.csv"

load_and_combine_linkedin <- function(job_postings_path,
                                      companies_path,
                                      employee_counts_path,
                                      benefits_path) {
  library(dplyr); library(readr); library(janitor); library(stringr)
  postings   <- read_csv(job_postings_path, show_col_types = FALSE) %>% clean_names()
  companies  <- read_csv(companies_path, show_col_types = FALSE) %>% clean_names()
  emp_counts <- read_csv(employee_counts_path, show_col_types = FALSE) %>% clean_names()
  benefits   <- read_csv(benefits_path, show_col_types = FALSE) %>% clean_names()
  
  ben_agg <- benefits %>%
    group_by(job_id) %>%
    summarise(
      n_benefits    = n(),
      frac_inferred = mean(ifelse(inferred %in% c(TRUE, 1, "TRUE", "True"), 1, 0)),
      .groups = "drop"
    )
  
  emp_latest <- emp_counts %>%
    group_by(company_id) %>%
    arrange(time_recorded) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  df <- postings %>%
    left_join(companies,    by = "company_id", suffix = c(".posting", ".company")) %>%
    left_join(emp_latest,   by = "company_id") %>%
    left_join(ben_agg,      by = "job_id")
  
  if (!"company_size" %in% names(df)) df$company_size <- NA_real_
  if (!"state.company" %in% names(df)) df$state.company <- NA_character_
  if (!"location" %in% names(df)) df$location <- NA_character_
  if (!"remote_allowed" %in% names(df)) df$remote_allowed <- NA
  
  norm_safe <- function(v) {
    vnum <- suppressWarnings(as.numeric(v))
    if (all(is.na(vnum))) return(rep(NA_real_, length(vnum)))
    rng <- range(vnum, na.rm = TRUE); if (diff(rng) == 0) return(rep(0.5, length(vnum)))
    (vnum - rng[1]) / (rng[2] - rng[1])
  }
  
  df %>%
    mutate(
      salary_mid = case_when(
        !is.na(med_salary) ~ suppressWarnings(as.numeric(med_salary)),
        !is.na(min_salary) & !is.na(max_salary) ~
          (suppressWarnings(as.numeric(min_salary)) + suppressWarnings(as.numeric(max_salary))) / 2,
        TRUE ~ NA_real_
      ),
      salary_norm        = norm_safe(salary_mid),
      company_size_norm  = suppressWarnings(as.numeric(company_size)) / 7,
      views_norm         = norm_safe(views),
      applies_norm       = norm_safe(applies),
      followers_norm     = norm_safe(follower_count),
      employees_norm     = norm_safe(employee_count),
      benefits_norm      = norm_safe(n_benefits),
      remote_ratio       = dplyr::case_when(
        is.na(remote_allowed)           ~ 0,
        remote_allowed %in% c(1, TRUE)  ~ 1,
        TRUE                            ~ 0
      ),
      region = dplyr::case_when(
        !is.na(state.company) & nzchar(state.company) ~ state_to_region(state.company),
        !is.na(location)     & nzchar(location)       ~ state_to_region(location),
        TRUE                                            ~ "other"
      ),
      company_size_bucket = case_when(
        is.na(company_size) ~ "unknown",
        suppressWarnings(as.numeric(company_size)) <= 2 ~ "small",
        suppressWarnings(as.numeric(company_size)) <= 5 ~ "medium",
        suppressWarnings(as.numeric(company_size)) >= 6 ~ "large",
        TRUE ~ "unknown"
      ),
      formatted_experience_level = if ("formatted_experience_level" %in% names(.))
        stringr::str_to_lower(as.character(formatted_experience_level)) else NA_character_,
      work_type = if ("work_type" %in% names(.))
        stringr::str_to_lower(as.character(work_type)) else NA_character_,
      application_type = if ("application_type" %in% names(.))
        stringr::str_to_lower(as.character(application_type)) else NA_character_
    )
}

combined_df <- load_and_combine_linkedin(job_postings_path,
                                         companies_path,
                                         employee_counts_path,
                                         benefits_path)

# =============================================================================
# 2) QUESTIONS (unchanged)
# =============================================================================

NUM_POOL <- c("remote_ratio", "salary_norm",
              "company_size_norm", "views_norm", "applies_norm",
              "followers_norm", "employees_norm", "benefits_norm")

CAT_POOL  <- c("formatted_experience_level", "work_type",
               "region", "application_type", "company_size_bucket")

make_questions_from_combined <- function(combined_df, n_numerical = 5, n_categorical = 5) {
  ok_num <- NUM_POOL[NUM_POOL %in% names(combined_df)]
  ok_num <- ok_num[!vapply(ok_num, function(nm) all(is.na(combined_df[[nm]])), logical(1))]
  ok_cat <- CAT_POOL[CAT_POOL %in% names(combined_df)]
  ok_cat <- ok_cat[!vapply(ok_cat, function(nm) all(is.na(combined_df[[nm]])), logical(1))]
  list(
    numerical   = ok_num[seq_len(min(n_numerical, length(ok_num)))],
    categorical = setNames(vector("list", min(n_categorical, length(ok_cat))),
                           ok_cat[seq_len(min(n_categorical, length(ok_cat)))])
  )
}

# =============================================================================
# 3) DEPARTMENTS (unchanged - keeping your revised weight generation)
# =============================================================================

generate_departments_from_combined <- function(combined_df, questions,
                                               n_departments = 20, seed = 123,
                                               prestige_rescale = c("minmax","none","rank_uniform"),
                                               prestige_power   = 1.0,
                                               prestige_eps     = 1e-3,
                                               prestige_weight_on = c("scaled","raw"),
                                               dept_tier_cutpoints = c(0.10, 0.25, 0.50)) {
  prestige_rescale   <- match.arg(prestige_rescale)
  prestige_weight_on <- match.arg(prestige_weight_on)
  set.seed(seed)
  
  df <- combined_df %>%
    mutate(
      s_raw = pmin(1, pmax(0,
                           0.35 * ifelse(is.na(company_size_norm), 0.5, company_size_norm) +
                             0.15 * ifelse(is.na(followers_norm),    0.5, followers_norm) +
                             0.20 * ifelse(is.na(views_norm),        0.5, views_norm) +
                             0.10 * ifelse(is.na(applies_norm),      0.5, applies_norm) +
                             0.10 * ifelse(is.na(salary_norm),       0.5, salary_norm) +
                             0.10 * ifelse(is.na(remote_ratio),      0.3, remote_ratio) +
                             rnorm(n(), 0, 0.05)
      ))) %>%
    filter(!is.na(s_raw))
  
  n_take <- min(n_departments, nrow(df))
  df <- df %>% mutate(s_w_prelim = ifelse(is.finite(s_raw), s_raw, mean(s_raw, na.rm = TRUE)))
  samp <- df %>% dplyr::slice_sample(n = n_take, weight_by = s_w_prelim, replace = FALSE)
  
  rescale_minmax <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (!is.finite(diff(r)) || diff(r) <= 0) return(rep(0.5, length(x)))
    (x - r[1]) / (r[2] - r[1])
  }
  rescale_rank_uniform <- function(x) (rank(x, ties.method = "average") - 0.5) / length(x)
  
  s_scaled <- switch(prestige_rescale,
                     "none"         = samp$s_raw,
                     "minmax"       = rescale_minmax(samp$s_raw),
                     "rank_uniform" = rescale_rank_uniform(samp$s_raw))
  if (is.finite(prestige_power) && prestige_power > 0) s_scaled <- s_scaled ^ prestige_power
  s_scaled <- pmin(1 - prestige_eps, pmax(prestige_eps, s_scaled))
  
  n_depts <- nrow(samp)
  departments <- tibble(
    dept_id = seq_len(n_depts),
    rank    = rank(-s_scaled),
    s_j     = s_scaled
  )
  
  departments <- departments %>%
    mutate(prestige_tier = assign_tiers_from_quantiles(s_j, cutpoints = dept_tier_cutpoints))
  
  # departments <- departments %>%
  #   rowwise() %>%
  #   mutate(
  #     h_j = case_when(
  #       prestige_tier == "Tier 1" ~ sample(c(1L, 2L), 1, prob = c(0.2, 0.8)),
  #       prestige_tier == "Tier 2" ~ sample(c(1L, 2L), 1, prob = c(0.2, 0.8)),
  #       prestige_tier == "Tier 3" ~ sample(c(1L, 2L), 1, prob = c(0.4, 0.6)),
  #       TRUE                      ~ sample(c(1L, 2L), 1, prob = c(0.4, 0.6))
  #     ),
  #     k_j_raw = case_when(
  #       prestige_tier == "Tier 1" ~ sample(10:12, 1),
  #       prestige_tier == "Tier 2" ~ sample(10:12, 1),
  #       prestige_tier == "Tier 3" ~ sample(8:12, 1),
  #       TRUE                      ~ sample(8:12, 1)
  #     ),
  #     k_floor = if_else(h_j == 1L, 3L, 6L),
  #     k_cap   = if_else(h_j == 1L, 5L, 10L),
  #     k_j     = pmin(k_cap, pmax(k_floor, k_j_raw))
  #   ) %>% ungroup()
  
  departments <- departments %>%
    rowwise() %>%
    mutate(
      h_j = case_when(
        prestige_tier == "Tier 1" ~ sample(c(1L, 2L), 1, prob = c(0.2, 0.8)),
        prestige_tier == "Tier 2" ~ sample(c(1L, 2L), 1, prob = c(0.2, 0.8)),
        prestige_tier == "Tier 3" ~ sample(c(1L, 2L), 1, prob = c(0.4, 0.6)),
        TRUE                      ~ sample(c(1L, 2L), 1, prob = c(0.4, 0.6))
      ),
      k_j = if_else(h_j == 1L, 5L, 10L)
    ) %>%
    ungroup()
  
  
  for (q in questions$numerical) {
    if (q %in% names(samp))  departments[[paste0("d_", q)]] <- samp[[q]]
    else                     departments[[paste0("d_", q)]] <- stats::runif(nrow(samp))
  }
  for (q_name in names(questions$categorical)) {
    if (q_name %in% names(samp)) {
      vals <- as.character(samp[[q_name]])
      departments[[paste0("d_", q_name)]] <- vals
      levs <- sort(unique(vals[!is.na(vals)]))
      questions$categorical[[q_name]] <- if (length(levs)) levs else "unknown"
    } else {
      departments[[paste0("d_", q_name)]] <- "unknown"
      questions$categorical[[q_name]] <- "unknown"
    }
  }
  
  num_q <- length(questions$numerical)
  cat_q <- length(questions$categorical)
  n_total_q <- num_q + cat_q
  
  W <- matrix(NA_real_, nrow = nrow(departments), ncol = n_total_q)
  
  for (j in 1:nrow(departments)) {
    dept_type <- sample(1:3, 1, prob = c(0.4, 0.4, 0.2))
    
    if (dept_type == 1) {
      concentration <- runif(1, 0.5, 1.5)
      alpha_j <- rep(concentration, n_total_q)
    } else if (dept_type == 2) {
      concentration <- runif(1, 1.0, 2.0)
      alpha_j <- rep(concentration, n_total_q)
      n_focus <- sample(2:3, 1)
      focus_questions <- sample(1:n_total_q, size = n_focus)
      alpha_j[focus_questions] <- alpha_j[focus_questions] * runif(n_focus, 1.5, 3.0)
    } else {
      concentration <- runif(1, 0.8, 1.5)
      alpha_j <- rep(concentration, n_total_q)
      n_focus <- sample(1:2, 1)
      focus_questions <- sample(1:n_total_q, size = n_focus)
      alpha_j[focus_questions] <- alpha_j[focus_questions] * runif(n_focus, 3.0, 6.0)
    }
    
    w_j <- rgamma(n_total_q, shape = alpha_j, rate = 1)
    W[j, ] <- w_j / sum(w_j)
    
    max_weight <- max(W[j, ])
    if (max_weight > 0.6) {
      excess <- max_weight - 0.6
      which_max <- which.max(W[j, ])
      W[j, which_max] <- 0.6
      W[j, -which_max] <- W[j, -which_max] + excess / (n_total_q - 1)
    }
  }
  
  departments$weight_vector <- split(W, row(W))
  departments$s_j_raw <- samp$s_raw
  
  list(departments = departments, questions = questions)
}

# =============================================================================
# 4) NESTED PARTICIPATION ASSIGNMENTS
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
# 5) ADAPTIVE DEPARTMENT STRATEGIES
# =============================================================================

generate_department_strategies_adaptive <- function(n_departments, 
                                                    n_years,
                                                    participation_rate,
                                                    seed = 456) {
  set.seed(seed + round(participation_rate * 1000))
  
  # Adaptive probability: more S1 (strict) at high participation
  # ρ < 20%: 90% use S2 (imputation is safer)
  # ρ = 50%: 50% use S1, 50% use S2
  # ρ > 80%: 90% use S1 (can afford to be selective)
  
  prob_S1 <- pmin(0.9, pmax(0.1, participation_rate))
  
  # Matrix: rows = departments, cols = years
  # Value: 1 = S1 (strict discounting), 2 = S2 (neutral imputation)
  strategy_matrix <- matrix(
    sample(1:2, n_departments * n_years, replace = TRUE,
           prob = c(prob_S1, 1 - prob_S1)),
    nrow = n_departments,
    ncol = n_years
  )
  
  strategy_matrix
}

# =============================================================================
# 6) CANDIDATE GENERATION (no participation propensity needed anymore)
# =============================================================================

generate_candidates_no_participation <- function(n_candidates, questions, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  candidates <- tibble(
    cand_id = 1:n_candidates,
    v_i1 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5),
    v_i2 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5),
    v_i3 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5)
  ) %>%
    mutate(v_i_bar = (v_i1 * v_i2 * v_i3)^(1/3))
  
  for (q in questions$numerical) {
    candidates[[paste0("q_", q)]] <- runif(n_candidates)
  }
  for (q_name in names(questions$categorical)) {
    levels <- questions$categorical[[q_name]]
    if (is.null(levels) || !length(levels)) levels <- "unknown"
    candidates[[paste0("q_", q_name)]] <- sample(levels, n_candidates, replace = TRUE)
  }
  
  candidates
}

# =============================================================================
# 7) ALIGNMENT FUNCTION f_j (unchanged)
# =============================================================================

calculate_f_j <- function(candidate_row, department_row, questions) {
  if (is.data.frame(candidate_row))  candidate_row  <- as.list(candidate_row[1, ])
  if (is.data.frame(department_row)) department_row <- as.list(department_row[1, ])
  
  weights <- department_row$weight_vector[[1]]
  if (is.null(weights)) stop("department_row$weight_vector is missing.")
  num_q   <- length(questions$numerical)
  cat_q   <- length(questions$categorical)
  total_q <- num_q + cat_q
  if (length(weights) != total_q) {
    stop(sprintf("Weight vector length (%d) != number of questions (%d).", length(weights), total_q))
  }
  
  w_sum <- 0; S_num <- 0; idx <- 1L
  
  for (q in questions$numerical) {
    cand_val <- candidate_row[[paste0("q_", q)]]
    dept_val <- department_row[[paste0("d_", q)]]
    w_k <- weights[idx]; idx <- idx + 1L
    
    if (is.null(cand_val) || is.na(cand_val) || is.null(dept_val) || is.na(dept_val)) {
      s_k <- 0.5
    } else {
      cx <- min(max(as.numeric(cand_val), 0), 1)
      dx <- min(max(as.numeric(dept_val),  0), 1)
      s_k <- 1 - abs(cx - dx)
    }
    S_num <- S_num + w_k * s_k; w_sum <- w_sum + w_k
  }
  
  if (cat_q > 0) {
    for (q_name in names(questions$categorical)) {
      cand_val <- candidate_row[[paste0("q_", q_name)]]
      dept_val <- department_row[[paste0("d_", q_name)]]
      w_k <- weights[idx]; idx <- idx + 1L
      
      if (is.null(cand_val) || is.na(cand_val) || is.null(dept_val) || is.na(dept_val)) {
        s_k <- 0
      } else {
        s_k <- as.numeric(identical(as.character(cand_val), as.character(dept_val)))
      }
      S_num <- S_num + w_k * s_k; w_sum <- w_sum + w_k
    }
  }
  
  S_ij <- if (w_sum > 0) S_num / w_sum else 0
  
  gamma <- 2.0
  f <- (exp(gamma * S_ij) - 1) / (exp(gamma) - 1)
  
  eps <- 1e-6
  as.numeric(pmin(pmax(f, eps), 1 - eps))
}

# =============================================================================
# 8) ACCEPTANCE PROBABILITY MODEL (unchanged)
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

prepare_nn_data <- function(data, questions, include_fit = TRUE, include_questions = TRUE) {
  cont_features <- c("s_j")
  
  # Add v_i_bar (geometric mean) instead of log_v_i
  if ("v_i_bar" %in% colnames(data)) {
    data$log_v_i_bar <- log(data$v_i_bar + 1e-6)
    cont_features <- c(cont_features, "log_v_i_bar")
  }
  
  if (include_fit && "f_j" %in% colnames(data)) {
    data$log_f_j <- log(data$f_j + 1e-6)
    cont_features <- c(cont_features, "log_f_j")
  }
  
  # Department numericals
  for (q in questions$numerical) {
    dcol <- paste0("d_", q)
    if (dcol %in% colnames(data)) cont_features <- c(cont_features, dcol)
  }
  
  # Candidate numericals (if including questions)
  if (include_questions) {
    for (q in questions$numerical) {
      qcol <- paste0("q_", q)
      if (qcol %in% colnames(data)) cont_features <- c(cont_features, qcol)
    }
  }
  
  # Categorical embeddings
  x_cat_list <- list()
  embedding_specs <- list()
  cat_i <- 0L
  if (include_questions && length(questions$categorical) > 0) {
    for (q_name in names(questions$categorical)) {
      # Candidate categorical
      qcol <- paste0("q_", q_name)
      if (qcol %in% colnames(data)) {
        cat_i <- cat_i + 1L
        levels <- questions$categorical[[q_name]]
        if (is.null(levels) || length(levels) == 0) levels <- "unknown"
        x_cat_list[[cat_i]] <- match(as.character(data[[qcol]]), levels)
        embedding_specs[[cat_i]] <- list(n_levels = length(levels), 
                                         embed_dim = min(8, max(2, ceiling(length(levels)/2))))
      }
      # Department categorical
      dcol <- paste0("d_", q_name)
      if (dcol %in% colnames(data)) {
        cat_i <- cat_i + 1L
        levels <- questions$categorical[[q_name]]
        if (is.null(levels) || length(levels) == 0) levels <- "unknown"
        x_cat_list[[cat_i]] <- match(as.character(data[[dcol]]), levels)
        embedding_specs[[cat_i]] <- list(n_levels = length(levels),
                                         embed_dim = min(8, max(2, ceiling(length(levels)/2))))
      }
    }
  }
  
  x_cont_tensor <- torch_tensor(as.matrix(data[, cont_features, drop = FALSE]), dtype = torch_float())
  x_cat_tensors <- lapply(x_cat_list, function(x) torch_tensor(x, dtype = torch_long()))
  list(x_cont = x_cont_tensor, x_cat = x_cat_tensors,
       embedding_specs = embedding_specs, n_cont_features = ncol(x_cont_tensor))
}

initialize_department_model <- function(questions, include_fit = TRUE, include_questions = TRUE) {
  embedding_specs <- list()
  if (include_questions && length(questions$categorical) > 0) {
    for (i in seq_along(questions$categorical)) {
      n_levels <- length(questions$categorical[[i]] %||% character())
      if (n_levels == 0) n_levels <- 1
      embed_dim <- min(8, max(2, ceiling(n_levels/2)))
      embedding_specs[[i]] <- list(n_levels = n_levels, embed_dim = embed_dim)
    }
  }
  n_cont <- 1L + 1L + as.integer(include_fit) + if (include_questions) length(questions$numerical) else 0L
  model <- acceptance_net(n_cont, embedding_specs)
  list(model = model,
       optimizer = optim_adam(model$parameters, lr = 0.001),
       historical_data = tibble(),
       questions = questions,
       is_trained = FALSE,
       include_fit = include_fit,
       include_questions = include_questions)
}

train_department_model <- function(dept_model, train_data, n_epochs = 60,
                                   include_fit = dept_model$include_fit,
                                   include_questions = dept_model$include_questions, 
                                   seed = NULL) {
  # Keep only examples where an offer was extended
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
  
  # Train/val split if enough data
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
  
  # Adaptive learning rate schedule
  optimizer <- optim_adam(dept_model$model$parameters, lr = 0.002)
  scheduler <- lr_multiplicative(optimizer, lr_lambda = function(epoch) 0.98)
  
  # Early stopping tracking
  best_val_loss <- Inf
  patience <- 20
  patience_counter <- 0
  best_state <- NULL
  
  # Training loop
  dept_model$model$train()
  
  for (epoch in 1:n_epochs) {
    # Training step
    log_odds <- dept_model$model(x_train, x_cat_train)
    
    # Focal loss (helps with class imbalance)
    bce_loss <- nn_bce_with_logits_loss(reduction = "none")(log_odds, y_train)
    probs <- torch_sigmoid(log_odds)
    focal_weight <- torch_pow(1 - probs * y_train - (1 - probs) * (1 - y_train), 2)
    loss <- torch_mean(focal_weight * bce_loss)
    
    # L2 regularization
    l2_loss <- 0
    for (param in dept_model$model$parameters) {
      l2_loss <- l2_loss + torch_sum(param^2)
    }
    
    # Adaptive L2 penalty (stronger early, weaker late)
    l2_lambda <- 0.01 * exp(-epoch / 100)
    total_loss <- loss + l2_lambda * l2_loss
    
    optimizer$zero_grad()
    total_loss$backward()
    
    # Gradient clipping
    nn_utils_clip_grad_norm_(dept_model$model$parameters, max_norm = 1.0)
    
    optimizer$step()
    scheduler$step()
    
    # Validation check (if enabled)
    if (use_validation && epoch %% 5 == 0) {
      dept_model$model$eval()
      with_no_grad({
        val_log_odds <- dept_model$model(x_val, x_cat_val)
        val_loss <- as.numeric(nn_bce_with_logits_loss()(val_log_odds, y_val))
      })
      dept_model$model$train()
      
      # Early stopping logic
      if (val_loss < best_val_loss) {
        best_val_loss <- val_loss
        patience_counter <- 0
        # Save best model state
        best_state <- lapply(dept_model$model$parameters, function(p) p$clone()$detach())
      } else {
        patience_counter <- patience_counter + 1
        if (patience_counter >= patience) {
          # Restore best weights
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


predict_acceptance_probability <- function(dept_model, applicant_data, 
                                           n_bootstrap = 200,
                                           include_fit = dept_model$include_fit,
                                           include_questions = dept_model$include_questions,
                                           seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }
  
  # If model not trained, use fallback
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
  
  # BOOTSTRAP APPROACH: Resample historical data and refit
  hist_data <- dept_model$historical_data %>% filter(offered == 1L)
  if (nrow(hist_data) < 10) {
    # Not enough data for bootstrap, use deterministic prediction
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
  
  # Bootstrap resampling
  n_hist <- nrow(hist_data)
  pi_draws_matrix <- matrix(NA_real_, nrow = n_bootstrap, ncol = nrow(applicant_data))
  
  for (b in 1:n_bootstrap) {
    # Resample with replacement
    boot_idx <- sample.int(n_hist, size = n_hist, replace = TRUE)
    boot_data <- hist_data[boot_idx, ]
    
    # Create temporary model
    temp_model <- initialize_department_model(dept_model$questions, 
                                              include_fit, include_questions)
    temp_model$historical_data <- boot_data
    
    # Quick train (fewer epochs for bootstrap)
    temp_model <- train_department_model(temp_model, boot_data, 
                                         n_epochs = 30, 
                                         include_fit = include_fit,
                                         include_questions = include_questions,
                                         seed = seed + b)
    
    # Predict on applicants
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
  
  # Compute mean and variance
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



# [Keep all the acceptance_net, prepare_nn_data, initialize_department_model,
#  train_department_model, and predict_acceptance_probability functions exactly as they are]

# [I'll skip repeating these since they're unchanged - just keep them from your original code]

# =============================================================================
# 9) TRUE UTILITY (unchanged)
# =============================================================================

true_utility <- function(s_j, v_i_bar, f_j, dept_tier, cand_tier, lambda = 1.2) {
  tier_gap <- cand_tier - dept_tier
  U_det    <- exp(s_j * log(v_i_bar + 1e-8) + (1 - s_j) * log(f_j + 1e-8))
  penalty  <- exp(-lambda * pmax(tier_gap, 0))
  U_det * penalty
}

# =============================================================================
# 10) PAIRWISE RANKING SELECTION (unchanged)
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
  
  # Target based on h_j (hiring quota)
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

# BASELINE: No-signal selection (quality only)
select_interviews_no_signal <- function(applicant_data, k_j, ...) {
  # Rank by v_i_bar only
  idx <- order(applicant_data$v_i_bar, decreasing = TRUE)[seq_len(min(k_j, nrow(applicant_data)))]
  applicant_data$cand_id[idx]
}
# [Keep make_repeated_rank_draws, compute_c_alpha_from_draws, 
#  compute_pairwise_lower_ranks, select_interviews_sure_screening exactly as they are]

# =============================================================================
# 11) APPLICATIONS & CANDIDATE UTILITY (unchanged)
# =============================================================================

generate_applications <- function(candidates, departments, questions, ...) {
  tibble(cand_id = candidates$cand_id) %>%
    tidyr::crossing(tibble(dept_id = departments$dept_id))
}

candidate_utility <- function(v_i_bar, s_j, f_j) {
  exp(v_i_bar * log(s_j + 1e-8) + (1 - v_i_bar) * log(f_j + 1e-8))
}

# [Keep resolve_accept_one exactly as is]
resolve_accept_one <- function(results_tbl, seed = NULL, temperature = 0.3, noise_sd = 0.4) {
  if (!is.null(seed)) set.seed(seed)
  if (!nrow(results_tbl)) {
    return(results_tbl %>% dplyr::mutate(accepted = integer()))
  }
  
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
    if (any(!is.finite(p)) || any(p < 0)) p <- rep(1 / length(x), length(x))
    p
  }
  
  results_tbl %>%
    dplyr::mutate(
      cand_util = candidate_utility(v_i_bar, s_j, f_j),
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
# =============================================================================
# 12) MAIN MARKET SIMULATION WITH ADAPTIVE STRATEGIES
# =============================================================================

simulate_market_year_adaptive <- function(candidates, departments, questions, year, 
                                          dept_models_pairwise,
                                          participants_this_year,
                                          dept_strategies,
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
  
  # Add participation status
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
    
    dept_applications <- applications %>%
      dplyr::filter(dept_id == dept$dept_id)
    if (nrow(dept_applications) == 0) next
    
    # Get adaptive strategy for this department-year
    dept_strategy <- if (participation_rate == 0) {
      "S2"  # Baseline: always use S2
    } else {
      ifelse(dept_strategies[j, year] == 1, "S1", "S2")
    }
    
    apps_all <- dept_applications %>%
      dplyr::left_join(candidates, by = "cand_id") %>%
      dplyr::mutate(s_j = dept$s_j)
    
    if (!cand_tier_col %in% names(apps_all)) {
      apps_all[[cand_tier_col]] <- factor("Tier 4", levels = c("Tier 1","Tier 2","Tier 3","Tier 4"))
    } else {
      apps_all[[cand_tier_col]] <- factor(
        as.character(apps_all[[cand_tier_col]]),
        levels = c("Tier 1","Tier 2","Tier 3","Tier 4")
      )
    }
    
    apps_all <- apps_all %>%
      dplyr::mutate(
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
        dplyr::mutate(
          year = year, dept_id = dept$dept_id, strategy = "pairwise",
          pi_pred = NA_real_, U_true = NA_real_, U_hat = NA_real_,
          r_true = NA_real_, r_hat = NA_real_,
          interviewed = 0L, considered = 0L
        )
      diag_list[[length(diag_list) + 1L]] <- diag_df
      next
    }
    
    applicant_data <- applicant_data %>% dplyr::mutate(row_id = dplyr::row_number())
    
    # Apply adaptive strategy
    applicant_data_pw <- applicant_data %>%
      mutate(
        f_j_used = case_when(
          !participates & dept_strategy == "S1" ~ 0.0,
          !participates & dept_strategy == "S2" ~ v_i_bar,
          TRUE ~ f_j
        )
      )
    
    # Predict acceptance probabilities
    applicant_data_pw_temp <- applicant_data_pw
    applicant_data_pw_temp$f_j <- applicant_data_pw$f_j_used
    
    applicant_data_pw <- predict_acceptance_probability(
      dept_models_pairwise[[j]], 
      applicant_data_pw_temp,
      n_bootstrap = L_repeats,
      include_fit = TRUE,
      include_questions = TRUE,
      seed = j * 1000 + year
    )
    
    # U_det using f_j_used (what departments think)
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
    
    # CRITICAL: True utility with ACTUAL f_j
    U_true_pw <- true_utility(
      s_j = applicant_data_pw$s_j,
      v_i_bar = applicant_data_pw$v_i_bar,
      f_j = applicant_data_pw$f_j,  # ACTUAL f_j
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
    
    # Select interviews
    interviewed_ids_pw <- select_interviews_sure_screening(
      applicant_data_pw,
      k_j = dept$k_j, h_j = dept$h_j,
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
            k_j = dept$k_j, h_j = dept$h_j
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
        dplyr::mutate(offered = as.integer(dplyr::row_number() <= dept$h_j))
    } else {
      interviewed_data_pw <- interviewed_data_pw %>% dplyr::mutate(offered = 0L)
    }
    
    interviewed_data_pw <- interviewed_data_pw %>%
      dplyr::mutate(year = year, dept_id = dept$dept_id, strategy = "pairwise")
    
    if (nrow(interviewed_data_pw) > 0) {
      all_results <- dplyr::bind_rows(all_results, interviewed_data_pw)
    }
    
    # Diagnostics
    diag_pw <- apps_all %>%
      dplyr::mutate(
        considered = as.integer(cand_id %in% applicant_data$cand_id),
        interviewed = as.integer(cand_id %in% interviewed_ids_pw)
      ) %>%
      dplyr::left_join(
        applicant_data_pw %>% dplyr::select(cand_id, pi_pred, U_true, U_hat, r_true),
        by = "cand_id"
      ) %>%
      dplyr::transmute(
        year = year, dept_id = dept$dept_id, strategy = "pairwise",
        cand_id, s_j, v_i_bar, f_j, pi_pred, U_true, U_hat, r_true,
        interviewed, considered, participates
      )
    
    diag_list[[length(diag_list) + 1L]] <- diag_pw
  }
  
  # Resolve acceptances
  if (nrow(all_results) > 0) {
    all_results <- resolve_accept_one(all_results, seed = year, temperature = 0.05, noise_sd = 0.01)
  }
  
  # Build learning data
  for (j in 1:nrow(departments)) {
    ld_pw <- all_results %>% 
      dplyr::filter(dept_id == departments$dept_id[j], strategy == "pairwise")
    if (nrow(ld_pw) > 0) {
      learning_data_pairwise[[j]] <- ld_pw %>%
        dplyr::select(
          year, dept_id, cand_id, s_j, v_i_bar, f_j, offered, accepted,
          dplyr::starts_with("q_"), dplyr::starts_with("d_")
        )
    }
  }
  
  diagnostics <- list(applicant_level = dplyr::bind_rows(diag_list))
  
  list(
    results = all_results,
    learning_data_pairwise = learning_data_pairwise,
    rank_panel = rank_panel,
    diagnostics = diagnostics
  )
}

# =============================================================================
# 13) MAIN DRIVER WITH ADAPTIVE STRATEGIES
# =============================================================================

run_job_market_sim_adaptive <- function(combined_df,
                                        n_departments = 20,
                                        n_candidates = 500,
                                        sim_years = 10,
                                        participation_rate,
                                        yearly_candidate_cohorts = NULL,
                                        participation_sets = NULL,
                                        seed = 123,
                                        n_numerical = 5,
                                        n_categorical = 5,
                                        alpha = 0.05,
                                        L_repeats = 200,
                                        tuple_size = NULL,
                                        noise_method = "bootstrap",
                                        noise_scale = 0.15,
                                        prestige_rescale = "minmax",
                                        prestige_power = 1.0,
                                        prestige_eps = 1e-3,
                                        prestige_weight_on = "scaled",
                                        cand_tier_cutpoints = c(0.10, 0.25, 0.50),
                                        dept_tier_cutpoints = c(0.10, 0.25, 0.50)) {
  
  set.seed(seed)
  torch::torch_manual_seed(seed)
  
  questions <- make_questions_from_combined(
    combined_df, n_numerical = n_numerical, n_categorical = n_categorical
  )
  
  # Generate adaptive strategies for THIS participation rate
  dept_strategies <- generate_department_strategies_adaptive(
    n_departments = n_departments,
    n_years = sim_years,
    participation_rate = participation_rate,
    seed = seed + 1000
  )
  
  # Generate cohorts if not provided
  generate_cohorts <- is.null(yearly_candidate_cohorts)
  if (generate_cohorts) {
    yearly_candidate_cohorts <- vector("list", sim_years)
  }
  
  cand_roster_all <- list()
  rank_all <- list()
  diag_all <- list()
  
  kg <- generate_departments_from_combined(
    combined_df, questions, n_departments, seed,
    prestige_rescale, prestige_power, prestige_eps, 
    prestige_weight_on, dept_tier_cutpoints
  )
  departments <- kg$departments
  questions <- kg$questions
  
  # Initialize models
  mdl_pairwise <- purrr::map(1:nrow(departments), 
                             ~ initialize_department_model(questions, 
                                                           include_fit = TRUE, 
                                                           include_questions = TRUE))
  
  res_all <- list()
  
  for (year in 1:sim_years) {
    cat("Simulating year", year, "with participation rate", participation_rate, "...\n")
    
    # Generate or retrieve cohort
    if (generate_cohorts) {
      yearly_candidate_cohorts[[year]] <- generate_candidates_no_participation(
        n_candidates, questions, seed = seed + year
      )
    }
    
    candidates <- yearly_candidate_cohorts[[year]]
    
    # Get participants from nested sets
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
      dplyr::transmute(year = !!year, cand_id, quality_tier, v_i_bar, participates)
    
    out <- simulate_market_year_adaptive(
      candidates, departments, questions, year,
      dept_models_pairwise = mdl_pairwise,
      participants_this_year = participants_this_year,
      dept_strategies = dept_strategies,
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
    results = dplyr::bind_rows(res_all),
    departments = departments,
    questions = questions,
    cand_roster = dplyr::bind_rows(cand_roster_all),
    rank_panel = dplyr::bind_rows(rank_all),
    diagnostics = list(applicant_level = dplyr::bind_rows(diag_all)),
    participation_rate = participation_rate,
    yearly_candidate_cohorts = yearly_candidate_cohorts
  )
}

# =============================================================================
# 14) MAIN EXECUTION
# =============================================================================

questions <- make_questions_from_combined(combined_df, n_numerical = 5, n_categorical = 5)

# Generate nested participation sets ONCE
cat("Generating nested participation sets...\n")
n_candidates <- 500
participation_rates <- c(0.05, 0.20, 0.50, 0.90)
participation_sets <- generate_nested_participation_assignments(
  n_candidates = n_candidates,
  participation_rates = participation_rates,
  seed = 123
)

# Generate yearly cohorts ONCE (baseline run)
cat("Generating yearly candidate cohorts (baseline)...\n")
n_departments <- 20
sim_years <- 10

baseline_sim <- run_job_market_sim_adaptive(
  combined_df,
  n_departments = n_departments,
  n_candidates = n_candidates,
  sim_years = sim_years,
  participation_rate = 0,
  yearly_candidate_cohorts = NULL,
  participation_sets = participation_sets,
  seed = 123,
  n_numerical = 5,
  n_categorical = 5,
  alpha = 0.05,
  L_repeats = 10,
  noise_method = "bootstrap",
  noise_scale = 0.15,
  prestige_rescale = "minmax",
  prestige_power = 1.0,
  prestige_eps = 1e-3,
  prestige_weight_on = "scaled",
  cand_tier_cutpoints = c(0.10, 0.25, 0.50),
  dept_tier_cutpoints = c(0.10, 0.25, 0.50)
)

# Extract generated cohorts
yearly_cohorts <- baseline_sim$yearly_candidate_cohorts

# Run all scenarios with ADAPTIVE strategies
all_sim_results <- list()
all_sim_results[["baseline"]] <- baseline_sim

for (rate in c(0.05, 0.20, 0.50, 0.90, 1.00)) {
  scenario_name <- as.character(rate)
  
  cat("\n========================================\n")
  cat("Running ADAPTIVE simulation with ρ =", rate * 100, "%\n")
  cat("========================================\n")
  
  all_sim_results[[scenario_name]] <- run_job_market_sim_adaptive(
    combined_df,
    n_departments = n_departments,
    n_candidates = n_candidates,
    sim_years = sim_years,
    participation_rate = rate,
    yearly_candidate_cohorts = yearly_cohorts,  # SAME COHORTS
    participation_sets = participation_sets,     # NESTED SETS
    seed = 123,
    n_numerical = 5,
    n_categorical = 5,
    alpha = 0.05,
    L_repeats = 10,
    noise_method = "bootstrap",
    noise_scale = 0.15,
    prestige_rescale = "minmax",
    prestige_power = 1.0,
    prestige_eps = 1e-3,
    prestige_weight_on = "scaled",
    cand_tier_cutpoints = c(0.10, 0.25, 0.50),
    dept_tier_cutpoints = c(0.10, 0.25, 0.50)
  )
}

# Save results
# saveRDS(all_sim_results, "all_participation_results.rds")
# 
# cat("\n✓ All adaptive simulations complete!\n")
# 

# all_sim_results <- readRDS("all_participation_results.rds")
# 
# 
# 
# 
# 















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

# Generate figure
welfare_results <- make_fig_candidate_welfare_gain(all_sim_results, year_filter = c(1, 10))

# Save with appropriate dimensions for two-column layout
ggsave("fig_candidate_welfare_gain.pdf", 
       welfare_results$plot, 
       width = 10, 
       height = 4, 
       device = cairo_pdf)

# =============================================================================
# CANDIDATE WELFARE GAIN STRATIFIED BY TIER
# =============================================================================

make_fig_candidate_welfare_by_tier <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Get candidate roster with tiers
  all_candidates <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    all_sim_results[[rate_name]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2]) %>%
      mutate(participation_rate = rate)
  })
  
  # Calculate candidates per tier
  cand_totals_by_tier <- all_candidates %>%
    filter(participation_rate == 0) %>%
    count(quality_tier, name = "n_cand")
  
  # Extract welfare by tier
  welfare_data_by_tier <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    roster <- all_sim_results[[rate_name]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2])
    
    matches <- all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      mutate(V_ij = candidate_utility(v_i_bar, s_j, f_j))
    
    roster %>%
      left_join(
        matches %>% 
          group_by(year, cand_id) %>%
          summarise(welfare = sum(V_ij), .groups = "drop"),
        by = c("year", "cand_id")
      ) %>%
      mutate(
        welfare = coalesce(welfare, 0),
        participation_rate = rate
      )
  })
  
  # Aggregate welfare by tier
  aggregate_welfare_by_tier <- welfare_data_by_tier %>%
    group_by(participation_rate, quality_tier) %>%
    summarise(
      n_cand = n(),
      total_welfare = sum(welfare, na.rm = TRUE),
      n_matches = sum(welfare > 0),
      mean_welfare_per_candidate = total_welfare / n_cand,
      matching_rate = n_matches / n_cand,
      mean_welfare_per_match = mean(welfare[welfare > 0], na.rm = TRUE),
      se_per_match = sd(welfare[welfare > 0], na.rm = TRUE) / sqrt(n_matches),
      .groups = "drop"
    ) %>%
    mutate(
      quality_tier = factor(quality_tier, 
                            levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
    )
  
  # Statistical tests by tier
  cat("\n=== CANDIDATE WELFARE GAINS BY TIER ===\n")
  
  tier_tests <- list()
  tier_summaries <- list()
  
  for (tier in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
    baseline_tier <- welfare_data_by_tier %>%
      filter(participation_rate == 0, quality_tier == tier) %>%
      pull(welfare)
    
    full_tier <- welfare_data_by_tier %>%
      filter(participation_rate == 1, quality_tier == tier) %>%
      pull(welfare)
    
    if (length(baseline_tier) > 1 && length(full_tier) > 1) {
      test <- t.test(full_tier, baseline_tier, paired = TRUE)
      tier_tests[[tier]] <- test
      
      baseline_match_rate <- mean(baseline_tier > 0)
      full_match_rate <- mean(full_tier > 0)
      
      tier_summaries[[tier]] <- list(
        baseline_mean = mean(baseline_tier),
        full_mean = mean(full_tier),
        gain = mean(full_tier) - mean(baseline_tier),
        baseline_match_rate = baseline_match_rate,
        full_match_rate = full_match_rate,
        p_value = test$p.value
      )
      
      cat(tier, ":\n")
      cat("  Baseline: Welfare/cand = %.5f, Match rate = %.1f%%\n" %>% 
            sprintf(mean(baseline_tier), baseline_match_rate * 100))
      cat("  Full:     Welfare/cand = %.5f, Match rate = %.1f%%\n" %>%
            sprintf(mean(full_tier), full_match_rate * 100))
      cat("  Gain:     %.5f (p = %.4f) %s\n" %>%
            sprintf(mean(full_tier) - mean(baseline_tier), test$p.value,
                    ifelse(test$p.value < 0.05, "✓", "")))
      cat("\n")
    }
  }
  
  # Test differential gains
  gains_by_tier <- tibble(
    tier = names(tier_summaries),
    gain = map_dbl(tier_summaries, "gain"),
    tier_numeric = as.integer(factor(tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  )
  
  if (nrow(gains_by_tier) > 2) {
    gain_corr <- cor.test(gains_by_tier$tier_numeric, gains_by_tier$gain, 
                          method = "spearman")
    
    cat("Correlation(tier, gain):", sprintf("%.3f (p = %.4f)\n", 
                                            gain_corr$estimate, gain_corr$p.value))
  }
  
  # Plot 1: Matching rate by tier
  p2 <- ggplot(aggregate_welfare_by_tier, 
               aes(x = participation_rate * 100, 
                   y = matching_rate,
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
      labels = scales::percent_format(accuracy = 0.1),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = "Matching Rate",
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
  
  # Plot 2: Welfare gains (relative to baseline)
  welfare_gains <- aggregate_welfare_by_tier %>%
    group_by(quality_tier) %>%
    mutate(
      baseline_welfare = mean_welfare_per_candidate[participation_rate == 0],
      welfare_gain = mean_welfare_per_candidate - baseline_welfare
    ) %>%
    ungroup() %>%
    filter(participation_rate > 0)
  
  p4 <- ggplot(welfare_gains, 
               aes(x = participation_rate * 100, 
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
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(
      x = "Market Participation Rate (%)",
      y = "Welfare Gain vs. Baseline",
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
  
  # Combine plots
  combined_plot <- p2 + p4 +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
  
  list(
    plot = combined_plot,
    plot_matching = p2,
    plot_gains = p4,
    tests = tier_tests,
    tier_summaries = tier_summaries,
    gain_correlation = if(exists("gain_corr")) gain_corr else NULL,
    aggregate_data = aggregate_welfare_by_tier,
    candidate_data = welfare_data_by_tier
  )
}

# Generate figure
cand_welfare_by_tier <- make_fig_candidate_welfare_by_tier(all_sim_results, year_filter = c(1, 10))

cand_welfare_by_tier$plot

# Save with appropriate dimensions for two-column layout
ggsave("fig_candidate_welfare_by_tier.pdf", 
       cand_welfare_by_tier$plot, 
       width = 10, 
       height = 4, 
       device = cairo_pdf)


# =============================================================================
# FIGURE 2: Department Welfare by Informativeness (Theorem 4.2)
# =============================================================================

# =============================================================================
# CORRECTED FIGURE 2: Department Welfare by Informativeness (Theorem 4.2)
# =============================================================================

make_fig_department_welfare_by_tier <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Get department quotas (h_j)
  departments <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier, s_j, h_j)
  
  n_years <- length(year_filter[1]:year_filter[2])
  
  # Calculate total quota by tier
  quota_by_tier <- departments %>%
    group_by(prestige_tier) %>%
    summarise(
      total_quota = sum(h_j) * n_years,
      n_depts = n(),
      .groups = "drop"
    )
  
  # Extract hired candidates
  dept_welfare_data <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    all_sim_results[[rate_name]]$results %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             strategy == "pairwise", accepted == 1) %>%
      left_join(departments, by = "dept_id") %>%
      mutate(participation_rate = rate)
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
      mean_utility_per_slot = total_utility / total_quota,
      fill_rate = n_hires / total_quota,
      prestige_tier = factor(prestige_tier,
                             levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
    )
  
  # For statistical testing, we need department-level data
  dept_level_welfare <- map_dfr(names(all_sim_results), function(rate_name) {
    rate <- ifelse(rate_name == "baseline", 0, as.numeric(rate_name))
    
    all_dept_years <- crossing(
      dept_id = departments$dept_id,
      year = year_filter[1]:year_filter[2]
    ) %>%
      left_join(departments, by = "dept_id")
    
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
        welfare_per_slot = total_utility / h_j,
        participation_rate = rate
      )
  })
  
  # Statistical tests by tier
  cat("\n=== DEPARTMENT WELFARE GAINS BY TIER ===\n")
  
  tier_tests <- list()
  tier_summaries <- list()
  
  for (tier in c("Tier 1", "Tier 2", "Tier 3", "Tier 4")) {
    baseline_dept <- dept_level_welfare %>%
      filter(participation_rate == 0, prestige_tier == tier) %>%
      pull(welfare_per_slot)
    
    full_dept <- dept_level_welfare %>%
      filter(participation_rate == 1, prestige_tier == tier) %>%
      pull(welfare_per_slot)
    
    if (length(baseline_dept) > 1 && length(full_dept) > 1) {
      test <- t.test(full_dept, baseline_dept, paired = TRUE)
      tier_tests[[tier]] <- test
      
      baseline_fill <- dept_level_welfare %>%
        filter(participation_rate == 0, prestige_tier == tier) %>%
        summarise(fill_rate = sum(n_hires) / sum(h_j)) %>%
        pull(fill_rate)
      
      full_fill <- dept_level_welfare %>%
        filter(participation_rate == 1, prestige_tier == tier) %>%
        summarise(fill_rate = sum(n_hires) / sum(h_j)) %>%
        pull(fill_rate)
      
      tier_summaries[[tier]] <- list(
        baseline_mean = mean(baseline_dept),
        full_mean = mean(full_dept),
        gain = mean(full_dept) - mean(baseline_dept),
        baseline_fill = baseline_fill,
        full_fill = full_fill,
        p_value = test$p.value
      )
      
      cat(tier, ":\n")
      cat("  Baseline: Welfare/slot = %.4f, Fill rate = %.1f%%\n" %>% 
            sprintf(mean(baseline_dept), baseline_fill * 100))
      cat("  Full:     Welfare/slot = %.4f, Fill rate = %.1f%%\n" %>%
            sprintf(mean(full_dept), full_fill * 100))
      cat("  Gain:     %.4f (p = %.4f) %s\n" %>%
            sprintf(mean(full_dept) - mean(baseline_dept), test$p.value,
                    ifelse(test$p.value < 0.05, "✓", "")))
      cat("\n")
    }
  }
  
  # Test differential gains by prestige
  gains_by_tier <- tibble(
    tier = names(tier_summaries),
    gain = map_dbl(tier_summaries, "gain"),
    s_j_mean = departments %>%
      group_by(prestige_tier) %>%
      summarise(s_j = mean(s_j), .groups = "drop") %>%
      arrange(desc(prestige_tier)) %>%
      pull(s_j)
  ) %>%
    arrange(s_j_mean)
  
  gain_corr <- cor.test(gains_by_tier$s_j_mean, gains_by_tier$gain, 
                        method = "spearman")
  
  cat("Correlation(s_j, gain):", sprintf("%.3f (p = %.4f)\n", 
                                         gain_corr$estimate, gain_corr$p.value))
  
  # Plot 1: Total welfare by tier
  p1 <- ggplot(welfare_by_tier, aes(x = participation_rate * 100, 
                                    y = total_utility,
                                    linetype = prestige_tier, 
                                    shape = prestige_tier,
                                    group = prestige_tier)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +  # circle, triangle, square, diamond
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
    dept_level_data = dept_level_welfare
  )
}

# Generate figure
dept_welfare_results <- make_fig_department_welfare_by_tier(all_sim_results, year_filter = c(1, 10))

dept_welfare_results$plot
# Save with appropriate dimensions for two-column layout
ggsave("fig_department_welfare_combined.pdf", 
       dept_welfare_results$plot, 
       width = 10, 
       height = 4, 
       device = cairo_pdf)

# =============================================================================
# FIGURE 3: Monotonicity of Offer Probability (Lemma 4.1)
# =============================================================================

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
# ggsave("fig_candidate_welfare_by_tier_main.pdf", 
#        welfare_by_tier_results$plot_welfare, 
#        width = 10, height = 6)
# 
# ggsave("fig_candidate_welfare_gains_by_tier.pdf", 
#        welfare_by_tier_results$plot_gains, 
#        width = 10, height = 6)

# Optional: Print summary table
cat("\n=== WELFARE SUMMARY BY TIER ===\n")
print(welfare_by_tier_results$aggregate_data %>%
        filter(participation_rate %in% c(0, 1)) %>%
        dplyr::select(participation_rate, quality_tier, 
               mean_welfare_per_candidate, matching_rate, 
               mean_welfare_per_match) %>%
        arrange(quality_tier, participation_rate))


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
# INTERVIEW DISTRIBUTION HEATMAP
# =============================================================================

make_fig_dept_interview_heatmap <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Use baseline (ρ=0) and full participation (ρ=1.0)
  baseline_results <- all_sim_results[["baseline"]]$results %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                  strategy == "pairwise", interviewed == 1)
  
  full_results <- all_sim_results[["1"]]$results %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                  strategy == "pairwise", interviewed == 1)
  
  # Get department tiers with k_j (interview budget)
  departments <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier, k_j)
  
  # Get candidate tiers
  baseline_roster <- all_sim_results[["baseline"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  full_roster <- all_sim_results[["1"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  # Calculate total candidates and interview budgets by tier
  cand_totals <- baseline_roster %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier, 
                                 levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  
  interview_budget_totals <- departments %>%
    group_by(prestige_tier) %>%
    summarise(budget = sum(k_j) * length(year_filter[1]:year_filter[2]), 
              .groups = "drop") %>%
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
  
  # Create heatmap data
  heatmap_data <- combined_interviews %>%
    group_by(scenario, prestige_tier, quality_tier) %>%
    summarise(
      n_interviews = n(),
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
             fill = list(n_interviews = 0, mean_utility = NA))
  
  # Create axis labels with totals
  x_labels <- cand_totals %>%
    mutate(label = paste0(quality_tier, "\n(n=", n_cand, ")")) %>%
    arrange(quality_tier) %>%
    dplyr::select(quality_tier, label) %>%
    deframe()
  
  y_labels <- interview_budget_totals %>%
    mutate(label = paste0(prestige_tier, "\n(k=", budget, ")")) %>%
    arrange(desc(prestige_tier)) %>%
    dplyr::select(prestige_tier, label) %>%
    deframe()
  
  # Print summary statistics
  cat("\n=== INTERVIEW DISTRIBUTION SUMMARY ===\n")
  cat("Candidate totals by tier:\n")
  print(cand_totals)
  cat("\nInterview budget totals by department tier:\n")
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
  
  # Return plot and data
  list(
    plot = p,
    data = heatmap_data,
    candidate_totals = cand_totals,
    interview_budgets = interview_budget_totals
  )
}

# Generate figure
interview_heatmap_results <- make_fig_dept_interview_heatmap(all_sim_results, year_filter = c(1, 10))

# Save with appropriate dimensions for two-panel heatmap
ggsave("fig_dept_interview_heatmap.pdf", 
       interview_heatmap_results$plot, 
       width = 10, 
       height = 5, 
       device = cairo_pdf)


cat("\nInterview counts by scenario and tier combination:\n")
print(interview_heatmap_results$data %>% 
        filter(n_interviews > 0) %>%
        arrange(scenario, desc(prestige_tier), quality_tier))




# =============================================================================
# HIRING DISTRIBUTION HEATMAP
# =============================================================================
make_fig_dept_hiring_heatmap <- function(all_sim_results, year_filter = c(1, 10)) {
  
  # Use baseline (ρ=0) and full participation (ρ=1.0)
  baseline_results <- all_sim_results[["baseline"]]$results %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                  strategy == "pairwise", accepted == 1)
  
  full_results <- all_sim_results[["1"]]$results %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2],
                  strategy == "pairwise", accepted == 1)
  
  # Get department tiers with h_j (hiring quota)
  departments <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier, h_j)
  
  # Get candidate tiers
  baseline_roster <- all_sim_results[["baseline"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  full_roster <- all_sim_results[["1"]]$cand_roster %>%
    dplyr::filter(year >= year_filter[1], year <= year_filter[2])
  
  # Calculate total candidates and hiring quotas by tier
  cand_totals <- baseline_roster %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier, 
                                 levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")))
  
  quota_totals <- departments %>%
    group_by(prestige_tier) %>%
    summarise(quota = sum(h_j) * length(year_filter[1]:year_filter[2]), 
              .groups = "drop") %>%
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
  
  y_labels <- quota_totals %>%
    mutate(label = paste0(prestige_tier, "\n(h=", quota, ")")) %>%
    arrange(desc(prestige_tier)) %>%
    dplyr::select(prestige_tier, label) %>%
    deframe()
  
  # Print summary statistics
  cat("\n=== HIRING DISTRIBUTION SUMMARY ===\n")
  cat("Candidate totals by tier:\n")
  print(cand_totals)
  cat("\nHiring quota totals by department tier:\n")
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
  
  # Calculate fill rates
  fill_rate_summary <- heatmap_data %>%
    left_join(quota_totals, by = "prestige_tier") %>%
    group_by(scenario, prestige_tier) %>%
    summarise(
      total_hires = sum(n_hires),
      total_quota = first(quota),
      fill_rate = total_hires / total_quota,
      .groups = "drop"
    )
  
  # Return plot and data
  list(
    plot = p,
    data = heatmap_data,
    candidate_totals = cand_totals,
    hiring_quotas = quota_totals,
    fill_rates = fill_rate_summary
  )
}

# Generate figure
hiring_heatmap_results <- make_fig_dept_hiring_heatmap(all_sim_results, year_filter = c(1, 10))

# Save with appropriate dimensions for two-panel heatmap
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
