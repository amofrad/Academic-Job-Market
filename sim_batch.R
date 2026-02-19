#!/usr/bin/env Rscript
# =============================================================================
# STAGE 2: SIM BATCH (runs as a job array task)
# Usage: Rscript run_sim_batch.R <TASK_ID>
#   TASK_ID 1 -> replicates 1-20
#   TASK_ID 2 -> replicates 21-40
#   ...
#   TASK_ID 5 -> replicates 81-100
# =============================================================================
source("Sim_Functions.R")

# ── Parse task ID ──
args    <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])
if (is.na(task_id)) task_id <- 1L
reps_per_task <- 20L

rep_start <- (task_id - 1L) * reps_per_task + 1L
rep_end   <- task_id * reps_per_task

cat(sprintf("=== SIM BATCH: Task %d | Replicates %d–%d ===\n",
            task_id, rep_start, rep_end))
cat("Time:", format(Sys.time()), "\n\n")

# ── Load burn-in artifacts ──
cat("Loading burn_in_artifacts.rds...\n")
artifacts <- readRDS("burn_in_artifacts.rds")

departments                    <- artifacts$departments
burn_in_historical             <- artifacts$burn_in_historical
yearly_hiring_schedule_sim     <- artifacts$yearly_hiring_schedule_sim
cfg                            <- artifacts$config

base_seed           <- cfg$base_seed
n_candidates        <- cfg$n_candidates
burn_in_years       <- cfg$burn_in_years
sim_years           <- cfg$sim_years
alpha               <- cfg$alpha
L_repeats           <- cfg$L_repeats
max_offer_rounds    <- cfg$max_offer_rounds
noise_method        <- cfg$noise_method
noise_scale         <- cfg$noise_scale
cand_tier_cutpoints <- cfg$cand_tier_cutpoints
participation_rates <- cfg$participation_rates

n_departments <- nrow(departments)
burn_in_seed  <- base_seed

# ── Reconstruct learned prior models from burn-in historical data ──
# (torch models can't be serialized via saveRDS, so we retrain from data)
cat("Reconstructing learned prior models from burn-in data...\n")
learned_prior_models <- reconstruct_learned_prior_models(
  burn_in_historical = burn_in_historical,
  questions          = questions,
  seed               = burn_in_seed
)
cat("Done.\n\n")

# ── Parallel setup: 1 worker per replicate ──
n_workers <- reps_per_task
cat(sprintf("Setting up %d workers for %d replicates\n", n_workers, reps_per_task))

old_plan <- future::plan()
on.exit(future::plan(old_plan), add = TRUE)
future::plan(future::multisession, workers = n_workers)

# ── Replicate function (same logic as run_multi_replicate_simulation) ──
run_one_replicate <- function(rep) {
  rep_seed <- base_seed + rep * 10000
  set.seed(rep_seed); torch::torch_manual_seed(rep_seed)
  try(torch::torch_set_num_threads(1L), silent = TRUE)
  try(torch::torch_set_num_interop_threads(1L), silent = TRUE)

  # Fresh candidate cohorts for sim phase
  yearly_candidate_cohorts_sim <- vector("list", sim_years)
  for (year in 1:sim_years)
    yearly_candidate_cohorts_sim[[year]] <- generate_candidates_new(
      n_candidates, questions, seed = rep_seed + burn_in_years + year)

  # Strategic participation sets per year
  positive_rates <- participation_rates[participation_rates > 0 &
                                        participation_rates < 1]
  yearly_participation_sets_sim <- vector("list", sim_years)
  for (year in 1:sim_years) {
    cohort <- add_participation_signals(
      candidates  = yearly_candidate_cohorts_sim[[year]],
      departments = departments,
      questions   = questions,
      dept_weights = yearly_hiring_schedule_sim[, year]
    )
    yearly_candidate_cohorts_sim[[year]] <- cohort
    yearly_participation_sets_sim[[year]] <- generate_nested_participation_assignments(
      candidates         = cohort,
      participation_rates = positive_rates
    )
  }

  # Reconstruct priors (each worker needs its own torch models)
  lpm <- reconstruct_learned_prior_models(
    burn_in_historical = burn_in_historical,
    questions          = questions,
    seed               = burn_in_seed
  )

  # Run each participation rate
  sim_results_rep <- list()
  for (rate in participation_rates) {
    rate_chr <- as.character(rate)
    sr <- run_job_market_sim_with_learned_prior(
      departments                  = departments,
      questions                    = questions,
      n_candidates                 = n_candidates,
      burn_in_years                = burn_in_years,
      sim_years                    = sim_years,
      participation_rate           = rate,
      yearly_candidate_cohorts_sim = yearly_candidate_cohorts_sim,
      participation_sets           = NULL,
      yearly_participation_sets_sim = yearly_participation_sets_sim,
      yearly_hiring_schedule_sim   = yearly_hiring_schedule_sim,
      learned_prior_models         = lpm,
      seed             = rep_seed,
      alpha            = alpha,
      L_repeats        = L_repeats,
      noise_method     = noise_method,
      noise_scale      = noise_scale,
      cand_tier_cutpoints = cand_tier_cutpoints,
      max_offer_rounds = max_offer_rounds,
      print_diagnostics      = FALSE,
      return_dept_models     = FALSE,
      return_yearly_results  = FALSE,
      collect_ranking_panel  = FALSE,
      keep_diagnostics       = FALSE
    )
    sim_results_rep[[rate_chr]] <- sr
  }

  rm(yearly_candidate_cohorts_sim, yearly_participation_sets_sim, lpm)
  invisible(gc(verbose = FALSE))

  list(replicate = rep, seed = rep_seed, sim_results = sim_results_rep)
}

# ── Run all replicates in this batch ──
start_time <- Sys.time()
reps <- rep_start:rep_end

rep_outputs <- future.apply::future_lapply(
  reps,
  run_one_replicate,
  future.seed       = TRUE,
  future.scheduling  = 1,
  future.globals     = TRUE,
  future.packages    = c("torch", "dplyr", "tidyr", "purrr", "tibble")
)

elapsed <- round(difftime(Sys.time(), start_time, units = "hours"), 2)
cat(sprintf("\nBatch %d completed in %s hours\n", task_id, elapsed))

# ── Aggregate into same structure as run_multi_replicate_simulation ──
rate_keys <- as.character(participation_rates)
sim_acc <- setNames(
  lapply(rate_keys, function(x) list(
    results = list(), rank_panel = list(),
    cand_roster = list(), diagnostics = list()
  )),
  rate_keys
)

for (rep_out in rep_outputs) {
  rep <- rep_out$replicate
  for (rate_chr in names(rep_out$sim_results)) {
    sr     <- rep_out$sim_results[[rate_chr]]
    bucket <- sim_acc[[rate_chr]]
    if (!is.null(sr$results) && nrow(sr$results) > 0)
      bucket$results[[length(bucket$results) + 1L]] <-
        sr$results %>% dplyr::mutate(replicate = rep)
    if (!is.null(sr$cand_roster) && nrow(sr$cand_roster) > 0)
      bucket$cand_roster[[length(bucket$cand_roster) + 1L]] <-
        sr$cand_roster %>% dplyr::mutate(replicate = rep)
    sim_acc[[rate_chr]] <- bucket
  }
}

batch_results <- list(
  task_id    = task_id,
  replicates = reps,
  sim_results = setNames(
    lapply(rate_keys, function(rc) list(
      results     = dplyr::bind_rows(sim_acc[[rc]]$results),
      rank_panel  = dplyr::bind_rows(sim_acc[[rc]]$rank_panel),
      cand_roster = dplyr::bind_rows(sim_acc[[rc]]$cand_roster),
      diagnostics = dplyr::bind_rows(sim_acc[[rc]]$diagnostics)
    )),
    rate_keys
  )
)

out_file <- sprintf("sim_batch_%d.rds", task_id)
saveRDS(batch_results, out_file, compress = "gzip")
cat(sprintf("Saved %s (%.1f MB)\n", out_file, file.size(out_file) / 1e6))
cat("Done.\n")