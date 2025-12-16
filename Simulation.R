
# =============================================================================
# =============================================================================
# ACADEMIC JOB MARKET SIMULATION
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
# UTILS
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
# 1) LOAD + COMBINE LINKEDIN DATA
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
# 3) DEPARTMENTS
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
  
  # Assign prestige tiers
  departments <- departments %>%
    mutate(prestige_tier = assign_tiers_from_quantiles(s_j, cutpoints = dept_tier_cutpoints))
  
  # Assign h_j and k_j by tier (matching paper description)
  departments <- departments %>%
    rowwise() %>%
    mutate(
      h_j = case_when(
        prestige_tier == "Tier 1" ~ sample(c(1L, 2L), 1, prob = c(0.2, 0.8)),
        prestige_tier == "Tier 2" ~ sample(c(1L, 2L), 1, prob = c(0.2, 0.8)),
        prestige_tier == "Tier 3" ~ sample(c(1L, 2L), 1, prob = c(0.4, 0.6)),
        TRUE                      ~ sample(c(1L, 2L), 1, prob = c(0.4, 0.6))
      ),
      k_j_raw = case_when(
        prestige_tier == "Tier 1" ~ sample(10:12, 1),
        prestige_tier == "Tier 2" ~ sample(10:12, 1),
        prestige_tier == "Tier 3" ~ sample(8:12, 1),
        TRUE                      ~ sample(8:12, 1)
      ),
      k_floor = if_else(h_j == 1L, 3L, 6L),
      k_cap   = if_else(h_j == 1L, 5L, 10L),
      k_j     = pmin(k_cap, pmax(k_floor, k_j_raw))
    ) %>% ungroup()
  
  # Add department attributes from LinkedIn data
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
  
  # Weight vectors for alignment calculation
  num_q <- length(questions$numerical)
  cat_q <- length(questions$categorical)
  n_total_q <- num_q + cat_q
  alpha0 <- rep(1, n_total_q)
  num_idx <- seq_len(num_q)
  alpha_bump <- 2 * s_scaled
  Alpha <- matrix(alpha0, nrow = nrow(departments), ncol = n_total_q, byrow = TRUE)
  Alpha[, num_idx] <- Alpha[, num_idx, drop=FALSE] + alpha_bump
  W <- matrix(rgamma(nrow(departments) * n_total_q, shape = Alpha, rate = 1), nrow = nrow(departments))
  W <- W / rowSums(W)
  departments$weight_vector <- split(W, row(W))
  
  departments$s_j_raw <- samp$s_raw
  
  list(departments = departments, questions = questions)
}

# =============================================================================
# 4) CANDIDATES
# =============================================================================

generate_candidates <- function(n_candidates, questions, participation_rate = 1.0, seed = NULL) {
  set.seed(seed)
  
  # Generate multidimensional quality v_i = (v_i1, v_i2, v_i3)
  candidates <- tibble(
    cand_id = 1:n_candidates,
    v_i1 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5),
    v_i2 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5),
    v_i3 = 0.6 * rbeta(n_candidates, 8, 3) + 0.4 * rbeta(n_candidates, 3, 5)
  ) %>%
    mutate(
      # Geometric mean for tier assignment
      v_i_bar = (v_i1 * v_i2 * v_i3)^(1/3),
      # Participation status
      participates = runif(n_candidates) < participation_rate
    )
  
  # Generate questionnaire responses for ALL candidates
  # (even non-participants, for simulation purposes)
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
# 5) ALIGNMENT FUNCTION f_j
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
  
  # Continuous attributes
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
  
  # Categorical attributes
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
  
  # Exponential transformation (gamma = 2)
  gamma <- 2.0
  f <- (exp(gamma * S_ij) - 1) / (exp(gamma) - 1)
  
  eps <- 1e-6
  as.numeric(pmin(pmax(f, eps), 1 - eps))
}

# =============================================================================
# 6) ACCEPTANCE PROBABILITY MODEL
# Bootstrap-based uncertainty
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

# =============================================================================
# PREDICTION AND SELECTION FUNCTIONS
# =============================================================================

# BOOTSTRAP-BASED PREDICTION

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

# =============================================================================
# TRUE UTILITY with tier penalties (for evaluation)
# =============================================================================

true_utility <- function(s_j, v_i_bar, f_j, dept_tier, cand_tier, lambda = 1.2) {
  tier_gap <- cand_tier - dept_tier  # >0: candidate above dept tier
  U_det    <- exp(s_j * log(v_i_bar + 1e-8) + (1 - s_j) * log(f_j + 1e-8))
  penalty  <- exp(-lambda * pmax(tier_gap, 0))
  U_det * penalty
}

# =============================================================================
# PAIRWISE RANKING SELECTION
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
  #target_m <- min(k_j, max(1L, as.integer(h_j)))
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

# =============================================================================
# APPLICATIONS: Everyone applies everywhere
# =============================================================================

generate_applications <- function(candidates, departments, questions, ...) {
  tibble(cand_id = candidates$cand_id) %>%
    tidyr::crossing(tibble(dept_id = departments$dept_id))
}

# =============================================================================
# CANDIDATE UTILITY AND ACCEPTANCE
# =============================================================================

candidate_utility <- function(v_i_bar, s_j, f_j) {
  # V_ij = kappa_ij * phi_i(f_j), where kappa_ij = s_j and phi_i increasing
  exp(v_i_bar * log(s_j + 1e-8) + (1 - v_i_bar) * log(f_j + 1e-8))
}

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
# =============================================================================
# MAIN SIMULATION DRIVER: Pairwise vs No-Signal
# =============================================================================
# =============================================================================

simulate_market_year <- function(candidates, departments, questions, year, 
                                 dept_models_pairwise,
                                 dept_models_nosignal,
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
  
  # Helper to map tier labels to integers
  tier_to_int <- function(x) {
    as.integer(factor(as.character(x), levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  }
  
  applications    <- generate_applications(candidates, departments, questions)
  all_results     <- tibble()
  learning_data_pairwise <- vector("list", length = nrow(departments))
  learning_data_nosignal <- vector("list", length = nrow(departments))
  rank_panel      <- tibble()
  diag_list       <- list()
  
  for (j in 1:nrow(departments)) {
    dept <- departments[j, ]
    
    dept_applications <- applications %>%
      dplyr::filter(dept_id == dept$dept_id)
    if (nrow(dept_applications) == 0) next
    
    # ==================================================
    # All applications to this dept (before shortlist)
    # ==================================================
    apps_all <- dept_applications %>%
      dplyr::left_join(candidates, by = "cand_id") %>%
      dplyr::mutate(s_j = dept$s_j)
    
    # Ensure candidate tiers are defined
    if (!cand_tier_col %in% names(apps_all)) {
      apps_all[[cand_tier_col]] <- factor("Tier 4", levels = c("Tier 1","Tier 2","Tier 3","Tier 4"))
    } else {
      apps_all[[cand_tier_col]] <- factor(
        as.character(apps_all[[cand_tier_col]]),
        levels = c("Tier 1","Tier 2","Tier 3","Tier 4")
      )
    }
    
    # Numeric dept/candidate tiers
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
    
    # ---------- SHORTLIST ----------
    if (shortlist_enabled) {
      allow_map <- list(
        "Tier 1" = c("Tier 1"),
        "Tier 2" = c("Tier 1","Tier 2"),
        "Tier 3" = c("Tier 1","Tier 2","Tier 3"),
        "Tier 4" = c("Tier 1","Tier 2","Tier 3","Tier 4")
      )
      pt      <- as.character(dept$prestige_tier %||% "Tier 4")
      allowed <- allow_map[[pt]] %||% allow_map[["Tier 4"]]
      considered_mask <- as.character(apps_all[[cand_tier_col]]) %in% allowed
    } else {
      considered_mask <- rep(TRUE, nrow(apps_all))
    }
    
    applicant_data <- apps_all[considered_mask, , drop = FALSE]
    
    if (nrow(applicant_data) == 0) {
      diag_df <- apps_all %>%
        dplyr::mutate(
          year        = year,
          dept_id     = dept$dept_id,
          strategy    = NA_character_,
          pi_pred     = NA_real_,
          U_true      = NA_real_,
          U_hat       = NA_real_,
          r_true      = NA_real_,
          r_hat       = NA_real_,
          interviewed = 0L,
          considered  = 0L
        )
      diag_list[[length(diag_list) + 1L]] <- diag_df
      next
    }
    
    applicant_data <- applicant_data %>% dplyr::mutate(row_id = dplyr::row_number())
    
    # ==================================================
    # STRATEGY 1: PAIRWISE (with questionnaire signal)
    # Departments randomly adopt (S1) or (S2) for non-participants
    # ==================================================
    
    # Randomly choose strategy for non-participants (consistent across year)
    dept_nonparticipant_strategy <- sample(c("S1", "S2"), 1)
    
    # Separate participants and non-participants
    applicant_data_pw <- applicant_data %>%
      mutate(
        # For non-participants under S1: set f_j = 0
        # For non-participants under S2: impute f_j = v_i_bar
        f_j_used = case_when(
          !participates & dept_nonparticipant_strategy == "S1" ~ 0.0,
          !participates & dept_nonparticipant_strategy == "S2" ~ v_i_bar,
          TRUE ~ f_j
        )
      )
    
    # Predict acceptance probabilities using model trained with fit
    # Only include fit information for participants
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
    
    # U_det using adjusted f_j
    U_det_pw <- exp(
      dept$s_j * log(applicant_data_pw$v_i_bar + 1e-8) +
        (1 - dept$s_j) * log(applicant_data_pw$f_j + 1e-8)
    )
    
    applicant_data_pw <- applicant_data_pw %>%
      dplyr::mutate(
        U_det = U_det_pw,
        U_hat = U_det * pmin(pmax(pi_pred, 1e-5), 1 - 1e-5)
      )
    
    # True utility with tier penalties (using actual f_j, not adjusted)
    U_true_pw <- true_utility(
      s_j = applicant_data_pw$s_j,
      v_i_bar = applicant_data_pw$v_i_bar,
      f_j = applicant_data_pw$f_j_used,  # Use the adjusted f_j for true utility too
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
    
    # Select interviews using pairwise confidence-calibrated procedure
    interviewed_ids_pw <- select_interviews_sure_screening(
      applicant_data_pw,
      k_j = dept$k_j,
      h_j = dept$h_j,
      alpha = alpha,
      L_repeats = L_repeats,
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
    
    # Extend offers to top h_j by U_hat
    if (nrow(interviewed_data_pw) > 0) {
      interviewed_data_pw <- interviewed_data_pw %>%
        dplyr::arrange(dplyr::desc(U_hat)) %>%
        dplyr::mutate(offered = as.integer(dplyr::row_number() <= dept$h_j))
    } else {
      interviewed_data_pw <- interviewed_data_pw %>% dplyr::mutate(offered = 0L)
    }
    
    interviewed_data_pw <- interviewed_data_pw %>%
      dplyr::mutate(year = year, dept_id = dept$dept_id, strategy = "pairwise")
    
    # ==================================================
    # STRATEGY 2: NO-SIGNAL (baseline, quality only)
    # ==================================================
    
    # Predict using model trained WITHOUT fit
    applicant_data_ns <- predict_acceptance_probability(
      dept_models_nosignal[[j]],
      applicant_data,
      n_bootstrap = L_repeats,
      include_fit = FALSE,
      include_questions = FALSE,
      seed = j * 1000 + year + 500
    )
    
    # U_det = v_i_bar (quality only, no alignment)
    U_det_ns <- applicant_data_ns$v_i_bar
    
    applicant_data_ns <- applicant_data_ns %>%
      dplyr::mutate(
        U_det = U_det_ns,
        U_hat = U_det * pmin(pmax(pi_pred, 1e-5), 1 - 1e-5)
      )
    
    # True utility (still uses f_j for evaluation)
    U_true_ns <- true_utility(
      s_j = applicant_data_ns$s_j,
      v_i_bar = applicant_data_ns$v_i_bar,
      f_j = applicant_data_ns$f_j,
      dept_tier = applicant_data_ns$dept_tier,
      cand_tier = applicant_data_ns$cand_tier
    )
    applicant_data_ns$U_true <- U_true_ns
    applicant_data_ns$r_true <- rank(-U_true_ns, ties.method = "average")
    
    # Select interviews by v_i_bar
    interviewed_ids_ns <- select_interviews_no_signal(
      applicant_data_ns,
      k_j = dept$k_j
    )
    
    applicant_data_ns$interviewed_flag <- as.integer(applicant_data_ns$cand_id %in% interviewed_ids_ns)
    
    # Prepare interviewed data
    interviewed_data_ns <- applicant_data_ns %>%
      dplyr::filter(cand_id %in% interviewed_ids_ns) %>%
      dplyr::mutate(interviewed = 1L)
    
    # Extend offers
    if (nrow(interviewed_data_ns) > 0) {
      interviewed_data_ns <- interviewed_data_ns %>%
        dplyr::arrange(dplyr::desc(U_hat)) %>%
        dplyr::mutate(offered = as.integer(dplyr::row_number() <= dept$h_j))
    } else {
      interviewed_data_ns <- interviewed_data_ns %>% dplyr::mutate(offered = 0L)
    }
    
    interviewed_data_ns <- interviewed_data_ns %>%
      dplyr::mutate(year = year, dept_id = dept$dept_id, strategy = "no_signal")
    
    # ==================================================
    # Combine results from both strategies
    # ==================================================
    if (nrow(interviewed_data_pw) > 0) {
      all_results <- dplyr::bind_rows(all_results, interviewed_data_pw)
    }
    if (nrow(interviewed_data_ns) > 0) {
      all_results <- dplyr::bind_rows(all_results, interviewed_data_ns)
    }
    
    # ==================================================
    # Diagnostics
    # ==================================================
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
        interviewed, considered, participates  # This comes from apps_all (which has it from candidates)
      )
    
    diag_ns <- apps_all %>%
      dplyr::mutate(
        considered = as.integer(cand_id %in% applicant_data$cand_id),
        interviewed = as.integer(cand_id %in% interviewed_ids_ns)
      ) %>%
      dplyr::left_join(
        applicant_data_ns %>% dplyr::select(cand_id, pi_pred, U_true, U_hat, r_true),
        by = "cand_id"
      ) %>%
      dplyr::transmute(
        year = year, dept_id = dept$dept_id, strategy = "no_signal",
        cand_id, s_j, v_i_bar, f_j, pi_pred, U_true, U_hat, r_true,
        interviewed, considered, participates  # This also comes from apps_all
      )
    
    diag_list[[length(diag_list) + 1L]] <- dplyr::bind_rows(diag_pw, diag_ns)
  }
  
  # Resolve acceptances
  if (nrow(all_results) > 0) {
    all_results <- resolve_accept_one(all_results, seed = year, temperature = 0.05, noise_sd = 0.01)
  }
  
  # Build learning data for next year
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
    
    ld_ns <- all_results %>% 
      dplyr::filter(dept_id == departments$dept_id[j], strategy == "no_signal")
    if (nrow(ld_ns) > 0) {
      learning_data_nosignal[[j]] <- ld_ns %>%
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
    learning_data_nosignal = learning_data_nosignal,
    rank_panel = rank_panel,
    diagnostics = diagnostics
  )
}

# =============================================================================
# =============================================================================
# MAIN DRIVER
# =============================================================================
# =============================================================================
run_job_market_sim_two_strategies <- function(combined_df,
                                              n_departments = 20,
                                              n_candidates  = 200,
                                              sim_years     = 10,
                                              participation_rate = 1.0,
                                              seed          = 123,
                                              n_numerical   = 5,
                                              n_categorical = 5,
                                              alpha         = 0.05,
                                              L_repeats     = 200,
                                              tuple_size    = NULL,
                                              noise_method  = "bootstrap",
                                              noise_scale   = 0.15,
                                              prestige_rescale   = "minmax",
                                              prestige_power     = 1.0,
                                              prestige_eps       = 1e-3,
                                              prestige_weight_on = "scaled",
                                              cand_tier_cutpoints = c(0.10, 0.25, 0.50),
                                              dept_tier_cutpoints = c(0.10, 0.25, 0.50)) {
  
  set.seed(seed)
  torch::torch_manual_seed(seed)
  
  questions <- make_questions_from_combined(
    combined_df, n_numerical = n_numerical, n_categorical = n_categorical
  )
  
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
  
  # Initialize two sets of models
  mdl_pairwise <- purrr::map(1:nrow(departments), 
                             ~ initialize_department_model(questions, include_fit = TRUE, include_questions = TRUE))
  mdl_nosignal <- purrr::map(1:nrow(departments), 
                             ~ initialize_department_model(questions, include_fit = FALSE, include_questions = FALSE))
  
  res_all <- list()
  
  for (year in 1:sim_years) {
    cat("Simulating year", year, "with participation rate", participation_rate, "...\n")
    
    
    # Generate candidates with participation rate
    candidates <- generate_candidates(n_candidates, questions, participation_rate = participation_rate, seed = year)
    candidates <- candidates %>%
      mutate(quality_tier = assign_tiers_from_quantiles(v_i_bar, cutpoints = cand_tier_cutpoints))
    
    cand_roster_all[[year]] <- candidates %>%
      dplyr::transmute(year = !!year, cand_id, quality_tier, v_i_bar, participates)
    
    out <- simulate_market_year(
      candidates, departments, questions, year,
      dept_models_pairwise = mdl_pairwise,
      dept_models_nosignal = mdl_nosignal,
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
    
    # Train models for next year
    for (j in 1:nrow(departments)) {
      # Update pairwise models
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
      
      # Update no-signal models
      if (!is.null(out$learning_data_nosignal[[j]]) && 
          nrow(out$learning_data_nosignal[[j]]) > 0) {
        mdl_nosignal[[j]]$historical_data <- bind_rows(
          mdl_nosignal[[j]]$historical_data,
          out$learning_data_nosignal[[j]]
        )
        if (year >= 2 && nrow(mdl_nosignal[[j]]$historical_data) >= 30) {
          mdl_nosignal[[j]] <- train_department_model(
            mdl_nosignal[[j]], mdl_nosignal[[j]]$historical_data,
            include_fit = FALSE, include_questions = FALSE, seed = year * j + 1000
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
    participation_rate = participation_rate
  )
}


# =============================================================================
# =============================================================================
# =============================================================================
# SIMULATION RUNNER
# =============================================================================
# =============================================================================
# =============================================================================



# =============================================================================
# =============================================================================
# SIMULATION RUNNER (Full)
# =============================================================================
# =============================================================================
cat("Starting pairwise vs no-signal comparison...\n")
start.time <- Sys.time()

sim_results <- run_job_market_sim_two_strategies(
  combined_df,
  n_departments = 20,
  n_candidates = 200,
  sim_years = 15,
  participation_rate = 1.0, # Full Participation
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

end.time <- Sys.time()
time.taken <- end.time - start.time
cat("Runtime:", as.numeric(time.taken, units = "mins"), "minutes.\n")
cat("Simulation complete.\n")


# Save simulation results
saveRDS(sim_results, "sim_results.rds")
# sim_results <- readRDS("sim_results.rds")

# =============================================================================
# SIMULATION RESULTS ANALYSIS AND FIGURES
# =============================================================================

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# JASA-style theme for publication-quality figures
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

# Color palette (Okabe-Ito colorblind-friendly)
okabe_ito <- c(
  "pairwise" = "#0072B2",   # blue
  "no_signal" = "#D55E00"   # vermillion
)

strategy_labels <- c(
  "pairwise" = "Questionnaire",
  "no_signal" = "No Questionnaire (baseline)"
)

okabe_ito <- c(
  "Questionnaire" = "#0072B2",   # blue
  "No Questionnaire (baseline)" = "#D55E00"   # vermillion
)


tier_colors <- c(
  "Tier 1" = "#1b2838", 
  "Tier 2" = "#3c6e71",  
  "Tier 3" = "#a64b29",  
  "Tier 4" = "#7a4f82"
)

# =============================================================================
# 1. DEPARTMENT PERSPECTIVE: SUMMARY STATISTICS
# =============================================================================
dept_summary_stats <- function(sim_results, year_filter = NULL) {
  results <- sim_results$results
  
  if (!is.null(year_filter)) {
    results <- results %>% filter(year >= year_filter[1], year <= year_filter[2])
  }
  
  # Overall statistics by strategy
  overall <- results %>%
    group_by(strategy) %>%
    summarise(
      n_offers = sum(offered, na.rm = TRUE),
      n_accepts = sum(accepted, na.rm = TRUE),
      yield = n_accepts / n_offers,
      mean_interviewed_util = mean(U_true[interviewed == 1], na.rm = TRUE),
      mean_hired_util = mean(U_true[accepted == 1], na.rm = TRUE),
      mean_quality_interviewed = mean(v_i_bar[interviewed == 1], na.rm = TRUE),
      mean_quality_hired = mean(v_i_bar[accepted == 1], na.rm = TRUE),
      mean_fit_interviewed = mean(f_j[interviewed == 1], na.rm = TRUE),
      mean_fit_hired = mean(f_j[accepted == 1], na.rm = TRUE),
      .groups = "drop"
    )
  
  # By department tier
  dept_tiers <- sim_results$departments %>%
    dplyr::select(dept_id, prestige_tier, s_j)
  
  by_tier <- results %>%
    left_join(dept_tiers, by = "dept_id") %>%
    group_by(strategy, prestige_tier) %>%
    summarise(
      n_offers = sum(offered, na.rm = TRUE),
      n_accepts = sum(accepted, na.rm = TRUE),
      yield = n_accepts / n_offers,
      mean_interviewed_util = mean(U_true[interviewed == 1], na.rm = TRUE),
      mean_hired_util = mean(U_true[accepted == 1], na.rm = TRUE),
      mean_quality_hired = mean(v_i_bar[accepted == 1], na.rm = TRUE),
      mean_fit_hired = mean(f_j[accepted == 1], na.rm = TRUE),
      .groups = "drop"
    )
  
  list(overall = overall, by_tier = by_tier)
}

# =============================================================================
# 2. FIGURE: WHO HIRES WHOM (HEATMAPS)
# =============================================================================
make_hiring_heatmap <- function(sim_results, year_filter = c(5, 10)) {
  
  results <- sim_results$results %>%
    filter(year >= year_filter[1], year <= year_filter[2], accepted == 1)
  
  dept_tiers <- sim_results$departments %>%
    dplyr::select(dept_id, prestige_tier, h_j)
  
  cand_roster <- sim_results$cand_roster
  
  # Calculate total candidates and quotas by tier
  cand_totals <- cand_roster %>%
    filter(year >= year_filter[1], year <= year_filter[2]) %>%
    count(quality_tier, name = "n_cand") %>%
    mutate(quality_tier = factor(quality_tier, 
                                 levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  quota_totals <- dept_tiers %>%
    group_by(prestige_tier) %>%
    summarise(quota = sum(h_j) * length(year_filter[1]:year_filter[2]), .groups = "drop") %>%
    mutate(prestige_tier = factor(prestige_tier, 
                                  levels = c("Tier 1","Tier 2","Tier 3","Tier 4")))
  
  # Join and compute heatmap data
  heatmap_data <- results %>%
    left_join(dept_tiers, by = "dept_id") %>%
    left_join(cand_roster %>% dplyr::select(year, cand_id, quality_tier), 
              by = c("year", "cand_id")) %>%
    group_by(strategy, prestige_tier, quality_tier.y) %>%
    summarise(
      n_hires = n(),
      mean_utility = mean(U_true, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prestige_tier = factor(prestige_tier, 
                             levels = c("Tier 4","Tier 3","Tier 2","Tier 1")),
      quality_tier = factor(quality_tier.y, 
                            levels = c("Tier 1","Tier 2","Tier 3","Tier 4"))
    ) %>%
    complete(strategy, prestige_tier, quality_tier, 
             fill = list(n_hires = 0, mean_utility = NA))
  
  # Create axis labels with totals
  x_labels <- cand_totals %>%
    mutate(label = paste0(quality_tier, "\n(n=", n_cand, ")")) %>%
    dplyr::select(quality_tier, label) %>%
    deframe()
  
  y_labels <- quota_totals %>%
    mutate(label = paste0(prestige_tier, "\n(quota=", quota, ")")) %>%
    dplyr::select(prestige_tier, label) %>%
    deframe()
  
  # Create plots for each strategy
  p_pairwise <- heatmap_data %>%
    filter(strategy == "pairwise") %>%
    ggplot(aes(x = quality_tier, y = prestige_tier, fill = mean_utility)) +
    geom_tile() +
    geom_text(aes(label = ifelse(n_hires > 0, n_hires, "")), 
              fontface = "bold", size = 4.5, vjust = 0.2) +
    geom_text(aes(label = ifelse(is.finite(mean_utility), 
                                 sprintf("μ=%.3f", mean_utility), "")),
              size = 3, vjust = 1.6) +
    scale_fill_viridis(name = "Mean dept utility", limits = c(0, 1), 
                       na.value = "grey50", option = "viridis") +
    scale_x_discrete(labels = x_labels) +
    scale_y_discrete(labels = y_labels) +
    labs(title = NULL, subtitle = "Pairwise (with signal)",
         x = "Candidate Quality Tier", y = "Department Prestige Tier") +
    theme_jasa() +
    theme(legend.position = "right",
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9))
  
  p_nosignal <- heatmap_data %>%
    filter(strategy == "no_signal") %>%
    ggplot(aes(x = quality_tier, y = prestige_tier, fill = mean_utility)) +
    geom_tile() +
    geom_text(aes(label = ifelse(n_hires > 0, n_hires, "")), 
              fontface = "bold", size = 4.5, vjust = 0.2) +
    geom_text(aes(label = ifelse(is.finite(mean_utility), 
                                 sprintf("μ=%.3f", mean_utility), "")),
              size = 3, vjust = 1.6) +
    scale_fill_viridis(name = "Mean dept utility", limits = c(0, 1), 
                       na.value = "grey50", option = "viridis") +
    scale_x_discrete(labels = x_labels) +
    scale_y_discrete(labels = y_labels) +
    labs(title = NULL, subtitle = "No signal (baseline)",
         x = "Candidate Quality Tier", y = "Department Prestige Tier") +
    theme_jasa() +
    theme(legend.position = "right",
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9))
  
  # Combine
  p_pairwise + p_nosignal +
    plot_annotation(
      title = "Who Hires Whom",
      subtitle = paste0("Years ", year_filter[1], "-", year_filter[2], 
                        "; cell values show hire counts and mean department utility"),
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# =============================================================================
# 3. FIGURE: YIELD AND UTILITY BY DEPARTMENT TIER
# =============================================================================
make_dept_metrics_by_tier <- function(sim_results, year_filter = c(5, 10)) {
  results <- sim_results$results %>%
    filter(year >= year_filter[1], year <= year_filter[2])
  
  dept_tiers <- sim_results$departments %>%
    dplyr::select(dept_id, prestige_tier)
  
  # Compute metrics
  metrics <- results %>%
    left_join(dept_tiers, by = "dept_id") %>%
    group_by(strategy, prestige_tier) %>%
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
      strategy = factor(strategy, levels = c("pairwise", "no_signal"),
                        labels = strategy_labels)
    )
  
  # Yield plot
  p_yield <- ggplot(metrics, aes(x = prestige_tier, y = yield, 
                                 fill = strategy, group = strategy)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(ymin = pmax(0, yield - 1.96*se_yield), 
                      ymax = pmin(1, yield + 1.96*se_yield)),
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = okabe_ito) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(x = "Department Prestige Tier", y = "Offer acceptance rate (yield)",
         title = "Acceptance Yield by Department Tier") +
    theme_jasa()
  
  # Utility plot
  p_utility <- ggplot(metrics, aes(x = prestige_tier, y = mean_util, 
                                   fill = strategy, group = strategy)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(ymin = mean_util - 1.96*se_util, 
                      ymax = mean_util + 1.96*se_util),
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = okabe_ito) +
    labs(x = "Department Prestige Tier", y = "Mean utility of hires",
         title = "Hiring Utility by Department Tier") +
    theme_jasa()
  
  # Combine
  p_yield + p_utility +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# =============================================================================
# 4. FIGURE: QUALITY-FIT TRADEOFF IN HIRES
# =============================================================================
make_quality_fit_scatter <- function(sim_results, year_filter = c(5, 10)) {
  
  results <- sim_results$results %>%
    filter(year >= year_filter[1], year <= year_filter[2], accepted == 1)
  
  dept_tiers <- sim_results$departments %>%
    dplyr::select(dept_id, prestige_tier)
  
  plot_data <- results %>%
    left_join(dept_tiers, by = "dept_id") %>%
    mutate(
      strategy = factor(strategy, levels = c("pairwise", "no_signal"),
                        labels = strategy_labels),
      prestige_tier = factor(prestige_tier, 
                             levels = c("Tier 1","Tier 2","Tier 3","Tier 4"))
    )
  
  ggplot(plot_data, aes(x = v_i_bar, y = f_j, color = strategy)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_density_2d(alpha = 0.6, linewidth = 0.5) +
    facet_wrap(~ prestige_tier, nrow = 2) +
    scale_color_manual(values = okabe_ito) +
    labs(x = "Candidate Quality (v̄ᵢ)", y = "Preference Alignment (f_j)",
         title = "Quality-Fit Profile of Hires by Department Tier",
         subtitle = "Points show individual hires; contours show density") +
    theme_jasa() +
    theme(legend.position = "bottom")
}

# =============================================================================
# 5. CANDIDATE PERSPECTIVE: INTERVIEW PROBABILITIES
# =============================================================================
make_candidate_interview_prob <- function(sim_results, year_filter = c(5, 10)) {
  
  apps <- sim_results$diagnostics$applicant_level %>%
    filter(year >= year_filter[1], year <= year_filter[2],
           !is.na(strategy), considered == 1)
  
  cand_roster <- sim_results$cand_roster %>%
    filter(year >= year_filter[1], year <= year_filter[2])
  
  apps <- apps %>%
    left_join(cand_roster %>% dplyr::select(year, cand_id, quality_tier), 
              by = c("year", "cand_id"))
  
  # Bin alignment
  breaks_f <- seq(0, 1, by = 0.05)
  
  interview_prob <- apps %>%
    mutate(
      f_bin = cut(f_j, breaks = breaks_f, include.lowest = TRUE),
      strategy = factor(strategy, levels = c("pairwise", "no_signal"),
                        labels = strategy_labels)
    ) %>%
    group_by(strategy, f_bin) %>%
    summarise(
      n_apps = n(),
      p_int = mean(interviewed == 1, na.rm = TRUE),
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
  
  ggplot(interview_prob, aes(x = f_mid, y = p_int, 
                             color = strategy, linetype = strategy, shape = strategy)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    scale_color_manual(values = okabe_ito) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_shape_manual(values = c(16, 17)) +
    scale_y_continuous(labels = percent_format(), limits = c(0, NA)) +
    labs(x = "Preference Alignment (f_j)", 
         y = "P(interviewed | considered)",
         title = "Interview Probability by Preference Alignment",
         subtitle = "Among considered applicants (after shortlisting)") +
    theme_jasa()
}


# =============================================================================
# 5.1. CANDIDATE PERSPECTIVE: INTERVIEW PROBABILITIES, STRATIFIED
# =============================================================================
make_candidate_interview_prob_stratified <- function(sim_results, year_filter = c(5, 10)) {
  # Get applications data
  apps <- sim_results$diagnostics$applicant_level %>%
    filter(year >= year_filter[1], year <= year_filter[2],
           !is.na(strategy), considered == 1)
  
  # Get candidate tiers
  cand_roster <- sim_results$cand_roster %>%
    filter(year >= year_filter[1], year <= year_filter[2])
  
  apps <- apps %>%
    left_join(cand_roster %>% dplyr::select(year, cand_id, quality_tier),
              by = c("year", "cand_id"))
  
  # Get department tiers
  apps <- apps %>%
    left_join(sim_results$departments %>% dplyr::select(dept_id, prestige_tier),
              by = "dept_id")
  
  # Bin alignment
  breaks_f <- seq(0, 1, by = 0.05)
  
  interview_prob <- apps %>%
    mutate(
      f_bin = cut(f_j, breaks = breaks_f, include.lowest = TRUE),
      strategy = factor(strategy, levels = c("pairwise", "no_signal"),
                        labels = c("Questionnaire", "No Questionnaire (baseline)")),
      quality_tier = factor(quality_tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
      prestige_tier = factor(prestige_tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
    ) %>%
    group_by(strategy, quality_tier, prestige_tier, f_bin) %>%
    summarise(
      n_apps = n(),
      p_int = mean(interviewed == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_apps >= 20) %>%  # Lower threshold due to stratification
    mutate(
      f_mid = {
        b_id <- as.numeric(f_bin)
        width <- diff(breaks_f)[1]
        breaks_f[1] + (b_id - 0.5) * width
      }
    )
  
  # Create faceted plot
  p <- ggplot(interview_prob, aes(x = f_mid, y = p_int,
                                  color = strategy, linetype = strategy, shape = strategy)) +
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
         color = "Strategy",
         linetype = "Strategy",
         shape = "Strategy") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 9, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  
  p
}

# =============================================================================
# 6. CANDIDATE PERSPECTIVE: BY QUALITY TIER
# =============================================================================
make_candidate_metrics_by_tier <- function(sim_results, year_filter = c(5, 10)) {
  apps <- sim_results$diagnostics$applicant_level %>%
    filter(year >= year_filter[1], year <= year_filter[2],
           !is.na(strategy), considered == 1)
  
  cand_roster <- sim_results$cand_roster %>%
    filter(year >= year_filter[1], year <= year_filter[2])
  
  # Interview counts per candidate
  cand_interviews <- apps %>%
    left_join(cand_roster %>% dplyr::select(year, cand_id, quality_tier), 
              by = c("year", "cand_id")) %>%
    group_by(year, strategy, cand_id, quality_tier) %>%
    summarise(
      n_interviews = sum(interviewed == 1, na.rm = TRUE),
      any_interview = as.integer(n_interviews > 0),
      .groups = "drop"
    )
  
  # Summary by tier
  summary <- cand_interviews %>%
    group_by(strategy, quality_tier) %>%
    summarise(
      mean_interviews = mean(n_interviews),
      prop_any = mean(any_interview),
      se_mean = sd(n_interviews) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      quality_tier = factor(quality_tier, 
                            levels = c("Tier 1","Tier 2","Tier 3","Tier 4")),
      strategy = factor(strategy, levels = c("pairwise", "no_signal"),
                        labels = strategy_labels)
    )
  
  # Mean interviews plot
  p1 <- ggplot(summary, aes(x = quality_tier, y = mean_interviews, 
                            fill = strategy)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(ymin = mean_interviews - 1.96*se_mean,
                      ymax = mean_interviews + 1.96*se_mean),
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = okabe_ito) +
    labs(x = "Candidate Quality Tier", y = "Mean interviews per candidate",
         title = "Interview Opportunities by Quality Tier") +
    theme_jasa()
  
  # Probability of any interview
  p2 <- ggplot(summary, aes(x = quality_tier, y = prop_any, 
                            fill = strategy)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    scale_fill_manual(values = okabe_ito) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(x = "Candidate Quality Tier", y = "P(≥1 interview)",
         title = "Probability of Receiving Interview") +
    theme_jasa()
  
  p1 + p2 +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# =============================================================================
# 7. TABLE: SUMMARY STATISTICS
# =============================================================================
make_summary_table <- function(sim_results, year_filter = c(5, 10)) {
  results <- sim_results$results %>%
    filter(year >= year_filter[1], year <= year_filter[2])
  
  overall <- results %>%
    group_by(strategy) %>%
    summarise(
      `Interviews` = sum(interviewed, na.rm = TRUE),
      `Offers` = sum(offered, na.rm = TRUE),
      `Accepts` = sum(accepted, na.rm = TRUE),
      `Yield` = sprintf("%.1f%%", 100 * Accepts / Offers),
      `Mean Quality (hired)` = sprintf("%.3f", mean(v_i_bar[accepted==1], na.rm=TRUE)),
      `Mean Fit (hired)` = sprintf("%.3f", mean(f_j[accepted==1], na.rm=TRUE)),
      `Mean Utility (hired)` = sprintf("%.3f", mean(U_true[accepted==1], na.rm=TRUE)),
      .groups = "drop"
    ) %>%
    mutate(Strategy = recode(strategy, !!!strategy_labels)) %>%
    dplyr::select(-strategy) %>%
    dplyr::select(Strategy, everything())
  
  overall
}

# =============================================================================
# RESULTS SUMMARY GENERATOR
# =============================================================================
generate_all_results <- function(sim_results, year_filter = c(5, 10), 
                                 output_dir = "simulation_results") {
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  cat("Generating simulation results and figures...\n")
  
  # 1. Summary statistics
  cat("  1. Computing summary statistics...\n")
  stats <- dept_summary_stats(sim_results, year_filter)
  
  # Print to console
  cat("\n=== OVERALL SUMMARY ===\n")
  print(stats$overall, n = Inf)
  cat("\n=== BY DEPARTMENT TIER ===\n")
  print(stats$by_tier, n = Inf)
  
  # Save table
  write.csv(stats$overall, 
            file.path(output_dir, "summary_overall.csv"), 
            row.names = FALSE)
  write.csv(stats$by_tier, 
            file.path(output_dir, "summary_by_tier.csv"), 
            row.names = FALSE)
  
  summary_table <- make_summary_table(sim_results, year_filter)
  write.csv(summary_table, 
            file.path(output_dir, "summary_table.csv"), 
            row.names = FALSE)
  cat("\n=== SUMMARY TABLE ===\n")
  print(summary_table)
  
  # 2. Hiring heatmap
  cat("\n  2. Creating hiring heatmaps...\n")
  p_heatmap <- make_hiring_heatmap(sim_results, year_filter)
  ggsave(file.path(output_dir, "fig_hiring_heatmap.pdf"), 
         p_heatmap, width = 12, height = 6)
  ggsave(file.path(output_dir, "fig_hiring_heatmap.png"), 
         p_heatmap, width = 12, height = 6, dpi = 300)
  print(p_heatmap)
  
  # 3. Department metrics by tier
  cat("\n  3. Creating department metrics plots...\n")
  p_dept_metrics <- make_dept_metrics_by_tier(sim_results, year_filter)
  ggsave(file.path(output_dir, "fig_dept_metrics.pdf"), 
         p_dept_metrics, width = 10, height = 5)
  ggsave(file.path(output_dir, "fig_dept_metrics.png"), 
         p_dept_metrics, width = 10, height = 5, dpi = 300)
  print(p_dept_metrics)
  
  # 4. Quality-fit scatter
  cat("\n  4. Creating quality-fit scatter plot...\n")
  p_scatter <- make_quality_fit_scatter(sim_results, year_filter)
  ggsave(file.path(output_dir, "fig_quality_fit_scatter.pdf"), 
         p_scatter, width = 10, height = 8)
  ggsave(file.path(output_dir, "fig_quality_fit_scatter.png"), 
         p_scatter, width = 10, height = 8, dpi = 300)
  print(p_scatter)
  
  # 5. Candidate interview probability
  cat("\n  5. Creating candidate interview probability plot...\n")
  p_cand_int <- make_candidate_interview_prob(sim_results, year_filter)
  ggsave(file.path(output_dir, "fig_candidate_interview_prob.pdf"), 
         p_cand_int, width = 8, height = 6)
  ggsave(file.path(output_dir, "fig_candidate_interview_prob.png"), 
         p_cand_int, width = 8, height = 6, dpi = 300)
  print(p_cand_int)
  
  
  cat("\n  5.1 Creating stratified candidate interview probability plot...\n")
  p_cand_int_strat <- make_candidate_interview_prob_stratified(sim_results, year_filter)
  ggsave(file.path(output_dir, "fig_candidate_interview_strat.pdf"), 
         p_cand_int_strat, width = 8, height = 6)
  ggsave(file.path(output_dir, "fig_candidate_interview_prob_strat.png"), 
         p_cand_int_strat, width = 8, height = 6, dpi = 300)
  print(p_cand_int_strat)
  
  
  # 6. Candidate metrics by tier
  cat("\n  6. Creating candidate metrics by tier plot...\n")
  p_cand_tier <- make_candidate_metrics_by_tier(sim_results, year_filter)
  ggsave(file.path(output_dir, "fig_candidate_metrics.pdf"), 
         p_cand_tier, width = 10, height = 5)
  ggsave(file.path(output_dir, "fig_candidate_metrics.png"), 
         p_cand_tier, width = 10, height = 5, dpi = 300)
  print(p_cand_tier)
  
  cat("\n✓ All results generated and saved to:", output_dir, "\n")
  
  invisible(list(
    stats = stats,
    summary_table = summary_table
  ))
}


results_output <- generate_all_results(
  sim_results,
  year_filter = c(5, 15),
  output_dir = "simulation_results"
)


# =============================================================================
# =============================================================================
# =============================================================================









# =============================================================================
# =============================================================================
# SIMULATION RUNNER (Participation Rates)
# =============================================================================
# =============================================================================
participation_rates <- c(0.05, 0.20, 0.50, 0.90)
all_sim_results <- list()

for (rate in participation_rates) {
  cat("\n========================================\n")
  cat("Running simulation with", rate * 100, "% participation\n")
  cat("========================================\n")
  
  all_sim_results[[as.character(rate)]] <- run_job_market_sim_two_strategies(
    combined_df,
    n_departments = 20,
    n_candidates = 200,
    sim_years = 10,
    participation_rate = rate,
    seed = 123,
    n_numerical = 5,
    n_categorical = 5,
    alpha = 0.05,
    L_repeats = 100,
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

# Save all results
saveRDS(all_sim_results, "all_participation_results.rds")
# all_sim_results <- readRDS("all_participation_results.rds")

# =============================================================================
# =============================================================================




# =============================================================================
# make_participation_comparison_plot <- function(all_sim_results, year_filter = c(5, 15)) {
#   
#   # Combine diagnostics from all participation rates
#   combined_diag <- map_dfr(names(all_sim_results), function(rate_str) {
#     rate <- as.numeric(rate_str)
#     all_sim_results[[rate_str]]$diagnostics$applicant_level %>%
#       filter(year >= year_filter[1], year <= year_filter[2],
#              !is.na(strategy), considered == 1, strategy == "pairwise") %>%
#       mutate(participation_rate = rate * 100)
#   })
#   
#   # Get candidate rosters
#   combined_roster <- map_dfr(names(all_sim_results), function(rate_str) {
#     rate <- as.numeric(rate_str)
#     all_sim_results[[rate_str]]$cand_roster %>%
#       filter(year >= year_filter[1], year <= year_filter[2]) %>%
#       mutate(participation_rate = rate * 100)
#   })
#   
#   # Join with roster to get participation status
#   combined_diag <- combined_diag %>%
#     left_join(combined_roster %>% dplyr::select(year, cand_id, quality_tier, participates, participation_rate),
#               by = c("year", "cand_id", "participation_rate"))
#   
#   # Calculate interview probabilities by participation status and rate
#   interview_stats <- combined_diag %>%
#     group_by(participation_rate, quality_tier, participates.y) %>%
#     summarise(
#       n_apps = n(),
#       p_interviewed = mean(interviewed == 1, na.rm = TRUE),
#       se = sqrt(p_interviewed * (1 - p_interviewed) / n_apps),
#       .groups = "drop"
#     ) %>%
#     filter(n_apps >= 11) %>%
#     mutate(
#       participant_label = ifelse(participates.y, "Participates", "Does not participate"),
#       quality_tier = factor(quality_tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"))
#     )
#   
#   # Plot
#   ggplot(interview_stats, aes(x = participation_rate, y = p_interviewed,
#                               color = participant_label, linetype = participant_label)) +
#     geom_line(linewidth = 1) +
#     geom_point(size = 2.5) +
#     geom_errorbar(aes(ymin = pmax(0, p_interviewed - 1.96*se),
#                       ymax = pmin(1, p_interviewed + 1.96*se)),
#                   width = 2) +
#     facet_wrap(~ quality_tier, nrow = 2) +
#     scale_color_manual(values = c("Participates" = "#0072B2", 
#                                   "Does not participate" = "#D55E00")) +
#     scale_linetype_manual(values = c("Participates" = "solid",
#                                      "Does not participate" = "dashed")) +
#     scale_x_continuous(breaks = c(5, 20, 50, 90),
#                        labels = c("5%", "20%", "50%", "90%")) +
#     scale_y_continuous(labels = percent_format(), limits = c(0, NA)) +
#     labs(x = "Market Participation Rate",
#          y = "P(interviewed | considered)",
#          title = "Interview Probability by Market Participation Rate",
#          subtitle = "Stratified by candidate quality tier and individual participation status",
#          color = "Candidate Status",
#          linetype = "Candidate Status") +
#     theme_jasa() +
#     theme(legend.position = "bottom")
# }
# 
# # Generate the plot
# (p_participation <- make_participation_comparison_plot(all_sim_results, year_filter = c(1, 15)))
# ggsave("fig_participation_comparison.pdf", p_participation, width = 10, height = 8)
# print(p_participation)


make_participation_comparison_stratified <- function(all_sim_results, year_filter = c(5, 10)) {
  
  # Combine diagnostics from all participation rates
  combined_diag <- map_dfr(names(all_sim_results), function(rate_str) {
    rate <- as.numeric(rate_str)
    all_sim_results[[rate_str]]$diagnostics$applicant_level %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             !is.na(strategy), considered == 1, strategy == "pairwise") %>%
      mutate(participation_rate = rate)
  })
  
  # Get candidate rosters
  combined_roster <- map_dfr(names(all_sim_results), function(rate_str) {
    rate <- as.numeric(rate_str)
    all_sim_results[[rate_str]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2]) %>%
      mutate(participation_rate = rate)
  })
  
  # Get department info (same across all participation rates, so just use first)
  departments <- all_sim_results[[1]]$departments %>%
    dplyr::select(dept_id, prestige_tier)
  
  # Join with roster to get participation status
  combined_diag <- combined_diag %>%
    left_join(combined_roster %>% dplyr::select(year, cand_id, quality_tier, participates, participation_rate),
              by = c("year", "cand_id", "participation_rate"))
  
  # Join with departments to get prestige tier
  combined_diag <- combined_diag %>%
    left_join(departments, by = "dept_id")
  
  # Bin alignment
  breaks_f <- seq(0, 1, by = 0.1)  # Wider bins due to multiple stratifications
  
  # Calculate interview probabilities by participation status, rate, and tiers
  interview_prob <- combined_diag %>%
    mutate(
      f_bin = cut(f_j, breaks = breaks_f, include.lowest = TRUE),
      quality_tier = factor(quality_tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
      prestige_tier = factor(prestige_tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
      participation_pct = participation_rate * 100,
      participant_label = ifelse(participates.y, "Participates", "Does not participate")
    ) %>%
    group_by(participation_pct, participant_label, quality_tier, prestige_tier, f_bin) %>%
    summarise(
      n_apps = n(),
      p_int = mean(interviewed == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_apps >= 10) %>%  # Lower threshold due to heavy stratification
    mutate(
      f_mid = {
        b_id <- as.numeric(f_bin)
        width <- diff(breaks_f)[1]
        breaks_f[1] + (b_id - 0.5) * width
      }
    )
  
  # Create faceted plot
  p <- ggplot(interview_prob, aes(x = f_mid, y = p_int,
                                  color = factor(participation_pct),
                                  linetype = participant_label,
                                  group = interaction(participation_pct, participant_label))) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.5) +
    facet_grid(quality_tier ~ prestige_tier,
               labeller = labeller(
                 quality_tier = function(x) paste("Cand:", x),
                 prestige_tier = function(x) paste("Dept:", x)
               )) +
    scale_color_viridis_d(
      name = "Market Participation Rate",
      labels = c("5%", "20%", "50%", "90%"),
      option = "D",
      begin = 0.0,
      end = 0.8,
      direction = -1
    ) +
    scale_linetype_manual(
      name = "Individual Status",
      values = c("Participates" = "solid", "Does not participate" = "dashed")
    ) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    labs(x = "Preference Alignment (f_j)",
         y = "P(interviewed | considered)",
         title = "Interview Probability by Market Participation Rate and Preference Alignment",
         subtitle = "Stratified by Candidate and Department Tiers") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      axis.text = element_text(size = 7)
    ) +
    guides(
      color = guide_legend(order = 1, nrow = 1),
      linetype = guide_legend(order = 2, nrow = 1)
    )
  
  p
}


(p_participation_stratified <- make_participation_comparison_stratified(all_sim_results, year_filter = c(5, 15)))
ggsave("fig_participation_stratified.pdf", p_participation_stratified, width = 12, height = 10)



# =============================================================================
# =============================================================================


make_participation_comparison_by_candidate_tier <- function(all_sim_results, year_filter = c(5, 10)) {
  
  # Combine diagnostics from all participation rates
  combined_diag <- map_dfr(names(all_sim_results), function(rate_str) {
    rate <- as.numeric(rate_str)
    all_sim_results[[rate_str]]$diagnostics$applicant_level %>%
      filter(year >= year_filter[1], year <= year_filter[2],
             !is.na(strategy), considered == 1, strategy == "pairwise") %>%
      mutate(participation_rate = rate)
  })
  
  # Get candidate rosters
  combined_roster <- map_dfr(names(all_sim_results), function(rate_str) {
    rate <- as.numeric(rate_str)
    all_sim_results[[rate_str]]$cand_roster %>%
      filter(year >= year_filter[1], year <= year_filter[2]) %>%
      mutate(participation_rate = rate)
  })
  
  # Join with roster to get participation status
  combined_diag <- combined_diag %>%
    left_join(combined_roster %>% dplyr::select(year, cand_id, quality_tier, participates, participation_rate),
              by = c("year", "cand_id", "participation_rate"))
  
  # Bin alignment
  breaks_f <- seq(0, 1, by = 0.05)  # Finer bins since we have fewer stratifications
  
  # Calculate interview probabilities by participation status, rate, and candidate tier only
  interview_prob <- combined_diag %>%
    mutate(
      f_bin = cut(f_j, breaks = breaks_f, include.lowest = TRUE),
      quality_tier = factor(quality_tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4")),
      participation_pct = participation_rate * 100,
      participant_label = ifelse(participates.y, "Participates", "Does not participate")
    ) %>%
    group_by(participation_pct, participant_label, quality_tier, f_bin) %>%
    summarise(
      n_apps = n(),
      p_int = mean(interviewed == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_apps >= 20) %>%  # Higher threshold since we have more data per bin
    mutate(
      f_mid = {
        b_id <- as.numeric(f_bin)
        width <- diff(breaks_f)[1]
        breaks_f[1] + (b_id - 0.5) * width
      }
    )
  
  # Create faceted plot (only by candidate tier)
  p <- ggplot(interview_prob, aes(x = f_mid, y = p_int,
                                  color = factor(participation_pct),
                                  linetype = participant_label,
                                  group = interaction(participation_pct, participant_label))) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~ quality_tier, nrow = 2, ncol = 2,
               labeller = labeller(quality_tier = function(x) paste("Candidate:", x))) +
    scale_color_viridis_d(
      name = "Market Participation Rate",
      labels = c("5%", "20%", "50%", "90%"),
      option = "D",
      begin = 0.0,
      end = 0.8,
      direction = -1
    ) +
    scale_linetype_manual(
      name = "Individual Status",
      values = c("Participates" = "solid", "Does not participate" = "dashed")
    ) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    labs(x = "Preference Alignment (f_j)",
         y = "P(interviewed | considered)",
         title = "Interview Probability by Market Participation Rate and Preference Alignment",
         subtitle = "Stratified by Candidate Quality Tier") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    ) +
    guides(
      color = guide_legend(order = 1, nrow = 1),
      linetype = guide_legend(order = 2, nrow = 1)
    )
  
  p
}

(p_participation_by_cand <- make_participation_comparison_by_candidate_tier(
  all_sim_results, 
  year_filter = c(5, 15)
))
ggsave("fig_participation_by_candidate_tier.pdf", p_participation_by_cand, width = 10, height = 8)
print(p_participation_by_cand)
