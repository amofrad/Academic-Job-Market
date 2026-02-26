#!/usr/bin/env Rscript
# =============================================================================
# STAGE 1: BURN-IN (run once, saves artifacts for sim array jobs)
# =============================================================================
source("Sim_Functions.R")

cat("=== BURN-IN STAGE ===\n")
cat("Time:", format(Sys.time()), "\n\n")

# ── Configuration (must match sim stage) ──
base_seed        <- 14
n_candidates     <- 300
burn_in_years    <- 20
sim_years        <- 10
alpha            <- 0.05
L_repeats        <- 20   # reduced from 20: 20 replicates provide sufficient averaging
max_offer_rounds <- 2
noise_method     <- "bootstrap"
noise_scale      <- NULL
cand_tier_cutpoints <- c(0.10, 0.25, 0.50)
participation_rates <- c(0, 0.05, 0.2, 0.5, 0.9, 1.00)#c(0, 0.05, 0.2, 0.5, 0.9, 1.00)

set.seed(base_seed); torch::torch_manual_seed(base_seed)

# ── Load and prepare departments ──
departments_final <- read_csv("departments_dataset.csv")
departments_raw   <- departments_final %>% mutate(dept_id = row_number())
departments       <- prepare_departments(departments_raw, questions, seed = base_seed)
n_departments     <- nrow(departments)

# ── Generate FIXED hiring schedules (deterministic from seed) ──
yearly_hiring_schedule_burn_in <- generate_yearly_hiring_schedule(
  n_departments, burn_in_years, departments, base_seed + 500)
yearly_hiring_schedule_sim <- generate_yearly_hiring_schedule(
  n_departments, sim_years, departments, base_seed + 600)

# ── Run burn-in ──
start_time <- Sys.time()

yearly_candidate_cohorts_burn_in <- vector("list", burn_in_years)
for (year in 1:burn_in_years)
  yearly_candidate_cohorts_burn_in[[year]] <- generate_candidates_new(
    n_candidates, questions, seed = base_seed + year)

burn_in <- run_burn_in_phase(
  departments = departments, questions = questions,
  n_candidates = n_candidates, burn_in_years = burn_in_years,
  yearly_candidate_cohorts = yearly_candidate_cohorts_burn_in,
  yearly_hiring_schedule = yearly_hiring_schedule_burn_in,
  seed = base_seed, alpha = alpha, L_repeats = L_repeats,
  noise_method = noise_method, noise_scale = noise_scale,
  cand_tier_cutpoints = cand_tier_cutpoints,
  max_offer_rounds = max_offer_rounds,
  return_yearly_objects = FALSE)

elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
cat(sprintf("\nBurn-in completed in %s minutes\n", elapsed))

# ── Save artifacts ──
# NOTE: We save burn_in_historical (not the torch models themselves)
# because torch nn_module objects don't serialize reliably with saveRDS.
# Each sim job will call reconstruct_learned_prior_models() to rebuild them.
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
    L_repeats           = L_repeats,
    max_offer_rounds    = max_offer_rounds,
    noise_method        = noise_method,
    noise_scale         = noise_scale,
    cand_tier_cutpoints = cand_tier_cutpoints,
    participation_rates = participation_rates
  )
)

saveRDS(burn_in_artifacts, "burn_in_artifacts.rds", compress = "gzip")
cat(sprintf("Saved burn_in_artifacts.rds (%.1f MB)\n",
            file.size("burn_in_artifacts.rds") / 1e6))
cat("Burn-in stage complete.\n")