#!/usr/bin/env Rscript
# =============================================================================
# sim_batch.R — Stage 2: Parallel simulation batch (runs as a job array task)
#
# Each task processes a batch of 20 replicates across all participation rates.
# Designed for HPC job arrays: 10 tasks yield 200 total replicates.
#
# Usage (from project root):
#   Rscript code/sim_batch.R <TASK_ID>
#
# Arguments:
#   TASK_ID — integer (1-10); determines replicate range:
#     Task 1 -> replicates 1-20, Task 2 -> 21-40, ..., Task 10 -> 181-200
#
# Input:
#   output/burn_in_artifacts.rds — output from BurnIn.R (Stage 1)
#
# Output:
#   output/sim_batch_<TASK_ID>.rds — batch results for this task's replicates
# =============================================================================
# Ensure working directory is the project root (parent of code/)
if (file.exists("Sim_Functions.R") && !file.exists("code/Sim_Functions.R")) setwd("..")

source("code/Sim_Functions.R")

# Parse task ID
args    <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])
if (is.na(task_id)) task_id <- 1L
reps_per_task <- 20L

rep_start <- (task_id - 1L) * reps_per_task + 1L
rep_end   <- task_id * reps_per_task

cat(sprintf("Sim batch: task %d, replicates %d-%d\n",
            task_id, rep_start, rep_end))

# Load burn-in artifacts
artifacts <- readRDS("output/burn_in_artifacts.rds")

departments                <- artifacts$departments
burn_in_historical         <- artifacts$burn_in_historical
yearly_hiring_schedule_sim <- artifacts$yearly_hiring_schedule_sim
cfg                        <- artifacts$config

base_seed           <- cfg$base_seed
n_candidates        <- cfg$n_candidates
burn_in_years       <- cfg$burn_in_years
sim_years           <- cfg$sim_years
alpha               <- cfg$alpha
n_bootstrap         <- cfg$n_bootstrap
max_offer_rounds    <- cfg$max_offer_rounds
cand_tier_cutpoints <- cfg$cand_tier_cutpoints
participation_rates <- cfg$participation_rates

n_departments <- nrow(departments)
burn_in_seed  <- base_seed

# Reconstruct learned prior models from burn-in historical data
# (torch models cannot be serialized via saveRDS; retrain from data)
learned_prior_models <- reconstruct_learned_prior_models(
  burn_in_historical = burn_in_historical,
  questions          = questions,
  seed               = burn_in_seed
)

# Parallel setup
n_workers <- reps_per_task

old_plan <- future::plan()
on.exit(future::plan(old_plan), add = TRUE)
future::plan(future::multisession, workers = n_workers)

# Single-replicate function
run_one_replicate <- function(rep) {
  rep_seed <- base_seed + rep * 10000
  set.seed(rep_seed)
  torch::torch_manual_seed(rep_seed)
  try(torch::torch_set_num_threads(1L), silent = TRUE)
  try(torch::torch_set_num_interop_threads(1L), silent = TRUE)

  # Generate candidate cohorts
  yearly_candidate_cohorts_sim <- vector("list", sim_years)
  for (year in 1:sim_years)
    yearly_candidate_cohorts_sim[[year]] <- generate_candidates(
      n_candidates, questions, seed = rep_seed + burn_in_years + year)

  # Compute participation assignments per year
  positive_rates <- participation_rates[participation_rates > 0 &
                                          participation_rates < 1]
  yearly_participation_sets_sim <- vector("list", sim_years)
  for (year in 1:sim_years) {
    cohort <- add_participation_signals(
      candidates   = yearly_candidate_cohorts_sim[[year]],
      departments  = departments,
      questions    = questions,
      dept_weights = yearly_hiring_schedule_sim[, year]
    )
    yearly_candidate_cohorts_sim[[year]] <- cohort
    yearly_participation_sets_sim[[year]] <- generate_nested_participation_assignments(
      candidates          = cohort,
      participation_rates = positive_rates
    )
  }

  # Reconstruct priors (each worker needs its own torch models)
  lpm <- reconstruct_learned_prior_models(
    burn_in_historical = burn_in_historical,
    questions          = questions,
    seed               = burn_in_seed
  )

  # Run simulation for each participation rate
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
      seed                = rep_seed,
      alpha               = alpha,
      n_bootstrap         = n_bootstrap,
      cand_tier_cutpoints = cand_tier_cutpoints,
      max_offer_rounds    = max_offer_rounds,
      print_diagnostics        = FALSE,
      return_dept_models       = FALSE,
      return_yearly_results    = FALSE,
      collect_ranking_panel    = FALSE,
      keep_diagnostics         = FALSE
    )
    sim_results_rep[[rate_chr]] <- sr
  }

  rm(yearly_candidate_cohorts_sim, yearly_participation_sets_sim, lpm)
  invisible(gc(verbose = FALSE))

  list(replicate = rep, seed = rep_seed, sim_results = sim_results_rep)
}

# Run all replicates in this batch
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
cat(sprintf("Batch %d completed in %s hours\n", task_id, elapsed))

# Aggregate results
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

out_file <- sprintf("output/sim_batch_%d.rds", task_id)
saveRDS(batch_results, out_file, compress = "gzip")
cat(sprintf("Saved %s (%.1f MB)\n", out_file, file.size(out_file) / 1e6))

# Save session info to requirements.txt
if (task_id == 1L)
  writeLines(capture.output(sessionInfo()), "output/requirements.txt")
