#!/usr/bin/env Rscript
# =============================================================================
# BurnIn.R — Stage 1: Burn-in phase (run once before simulation batches)
#
# Runs the burn-in phase of the academic job market simulation to establish
# department learned priors. Saves all artifacts needed by sim_batch.R.
#
# Usage (from project root):
#   Rscript code/BurnIn.R
#
# Output:
#   output/burn_in_artifacts.rds — serialized list containing departments,
#     historical data, hiring schedules, and configuration parameters.
# =============================================================================

# Ensure working directory is the project root (parent of code/)
if (file.exists("Sim_Functions.R") && !file.exists("code/Sim_Functions.R")) setwd("..")

source("code/Sim_Functions.R")

dir.create("output", showWarnings = FALSE)

# Configuration
base_seed           <- 14
n_candidates        <- 300
burn_in_years       <- 20
sim_years           <- 10
alpha               <- 0.05
n_bootstrap           <- 100
max_offer_rounds    <- 2
cand_tier_cutpoints <- c(0.10, 0.25, 0.50)
participation_rates <- c(0, 0.05, 0.2, 0.5, 0.9, 1.00)

set.seed(base_seed)
torch::torch_manual_seed(base_seed)

# Load and prepare departments
departments_final <- read_csv("data/departments_dataset.csv") %>% filter(rank <= 100)
departments_raw   <- departments_final %>% mutate(dept_id = row_number())
departments       <- prepare_departments(departments_raw, questions, seed = base_seed)
n_departments     <- nrow(departments)

# Generate deterministic hiring schedules
yearly_hiring_schedule_burn_in <- generate_yearly_hiring_schedule(
  n_departments, burn_in_years, departments, base_seed + 500)
yearly_hiring_schedule_sim <- generate_yearly_hiring_schedule(
  n_departments, sim_years, departments, base_seed + 600)


start_time <- Sys.time()

yearly_candidate_cohorts_burn_in <- vector("list", burn_in_years)
for (year in 1:burn_in_years)
  yearly_candidate_cohorts_burn_in[[year]] <- generate_candidates(
    n_candidates, questions, seed = base_seed + year)

burn_in <- run_burn_in_phase(
  departments              = departments,
  questions                = questions,
  n_candidates             = n_candidates,
  burn_in_years            = burn_in_years,
  yearly_candidate_cohorts = yearly_candidate_cohorts_burn_in,
  yearly_hiring_schedule   = yearly_hiring_schedule_burn_in,
  seed                     = base_seed,
  alpha                    = alpha,
  n_bootstrap              = n_bootstrap,
  cand_tier_cutpoints      = cand_tier_cutpoints,
  max_offer_rounds         = max_offer_rounds,
  return_yearly_objects     = FALSE)

elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
cat(sprintf("\nBurn-in completed in %s minutes\n", elapsed))

# Save artifacts
burn_in_artifacts <- list(
  departments                    = departments,
  burn_in_historical             = burn_in$burn_in_historical,
  burn_in_results                = burn_in$burn_in_results,
  burn_in_cand_roster            = burn_in$cand_roster,
  yearly_hiring_schedule_burn_in = yearly_hiring_schedule_burn_in,
  yearly_hiring_schedule_sim     = yearly_hiring_schedule_sim,
  config = list(
    base_seed           = base_seed,
    n_candidates        = n_candidates,
    burn_in_years       = burn_in_years,
    sim_years           = sim_years,
    alpha               = alpha,
    n_bootstrap         = n_bootstrap,
    max_offer_rounds    = max_offer_rounds,
    cand_tier_cutpoints = cand_tier_cutpoints,
    participation_rates = participation_rates
  )
)

saveRDS(burn_in_artifacts, "output/burn_in_artifacts.rds", compress = "gzip")
