#!/usr/bin/env Rscript
# =============================================================================
# Result_processer.R — Stage 3: Combine batch results and generate figures
#
# Merges all sim_batch_*.rds files into a single all_sim_results structure
# and produces publication-quality PDF figures.
#
# Usage (from project root):
#   Rscript code/Result_processer.R
#
# Input:
#   output/burn_in_artifacts.rds        — output from BurnIn.R (Stage 1)
#   output/sim_batch_1.rds ... sim_batch_10.rds — output from sim_batch.R (Stage 2)
#
# Output:
#   output/all_sim_results.rds          — combined simulation results
#   manuscript/fig/fig_dept_interview_heatmap.pdf
#   manuscript/fig/fig_dept_hiring_heatmap.pdf
#   manuscript/fig/fig_department_welfare.pdf
#   manuscript/fig/fig_candidate_welfare_by_tier.pdf
#   manuscript/fig/fig_candidate_by_participation.pdf
# =============================================================================
source("code/Sim_Functions.R")

cat("=== COMBINING RESULTS ===\n")

n_batches <- 10L

# ── Load burn-in artifacts ──
artifacts   <- readRDS("output/burn_in_artifacts.rds")
departments <- artifacts$departments
cfg         <- artifacts$config

# ── Load and merge all batch files ──
rate_keys <- as.character(cfg$participation_rates)
merged <- setNames(
  lapply(rate_keys, function(x) list(results = list(), cand_roster = list(),
                                     rank_panel = list(), diagnostics = list())),
  rate_keys
)

for (b in 1:n_batches) {
  f <- sprintf("output/sim_batch_%d.rds", b)
  if (!file.exists(f)) { cat(sprintf("WARNING: %s not found, skipping\n", f)); next }
  cat(sprintf("Loading %s...\n", f))
  batch <- readRDS(f)
  for (rc in rate_keys) {
    sr <- batch$sim_results[[rc]]
    if (!is.null(sr$results) && nrow(sr$results) > 0)
      merged[[rc]]$results[[length(merged[[rc]]$results) + 1L]] <- sr$results
    if (!is.null(sr$cand_roster) && nrow(sr$cand_roster) > 0)
      merged[[rc]]$cand_roster[[length(merged[[rc]]$cand_roster) + 1L]] <- sr$cand_roster
    if (!is.null(sr$rank_panel) && nrow(sr$rank_panel) > 0)
      merged[[rc]]$rank_panel[[length(merged[[rc]]$rank_panel) + 1L]] <- sr$rank_panel
    if (!is.null(sr$diagnostics) && nrow(sr$diagnostics) > 0)
      merged[[rc]]$diagnostics[[length(merged[[rc]]$diagnostics) + 1L]] <- sr$diagnostics
  }
}

# ── Assemble final results structure ──
all_sim_results <- list(
  departments = departments,
  questions   = questions,
  participation_sets = NULL,
  config = list(
    n_candidates          = cfg$n_candidates,
    burn_in_years         = cfg$burn_in_years,
    sim_years             = cfg$sim_years,
    participation_rates   = cfg$participation_rates,
    base_seed             = cfg$base_seed,
    n_replicates          = n_batches * 20L,
    alpha                 = cfg$alpha,
    L_repeats             = cfg$L_repeats,
    print_sim_diagnostics = FALSE,
    collect_rank_panel    = FALSE,
    keep_diagnostics      = FALSE
  ),
  sim_results = setNames(
    lapply(rate_keys, function(rc) list(
      results     = dplyr::bind_rows(merged[[rc]]$results),
      rank_panel  = dplyr::bind_rows(merged[[rc]]$rank_panel),
      cand_roster = dplyr::bind_rows(merged[[rc]]$cand_roster),
      diagnostics = dplyr::bind_rows(merged[[rc]]$diagnostics)
    )),
    rate_keys
  ),
  burn_in = list(
    burn_in_results = artifacts$burn_in_results %>% dplyr::mutate(replicate = 1L),
    cand_roster     = artifacts$burn_in_cand_roster %>% dplyr::mutate(replicate = 1L)
  )
)

# ── Summary ──
cat("\n=== COMBINED RESULTS SUMMARY ===\n")
for (rc in rate_keys) {
  nr    <- nrow(all_sim_results$sim_results[[rc]]$results)
  n_rep <- length(unique(all_sim_results$sim_results[[rc]]$results$replicate))
  cat(sprintf("  Rate %5s: %d result rows, %d replicates\n", rc, nr, n_rep))
}

saveRDS(all_sim_results, "output/all_sim_results.rds", compress = "gzip")
cat(sprintf("\nSaved output/all_sim_results.rds (%.1f MB)\n",
            file.size("output/all_sim_results.rds") / 1e6))

# ── Generate figures ──
cat("\nGenerating figures...\n")

interview_heatmap  <- fig_interview_heatmap(all_sim_results,
                        year_filter = c(1, 10), include_scramble = FALSE)
hiring_heatmap     <- fig_hiring_heatmap(all_sim_results,
                        year_filter = c(1, 10), include_scramble = TRUE, hire_rounds = 2)
dept_welfare       <- fig_department_welfare(all_sim_results,
                        year_filter = c(1, 10), include_scramble = TRUE, hire_rounds = 2)
cand_welfare       <- fig_candidate_welfare(all_sim_results,
                        year_filter = c(1, 10), include_scramble = TRUE, hire_rounds = 2)
cand_participation <- fig_participation_welfare(all_sim_results,
                        year_filter = c(1, 10), include_scramble = TRUE, hire_rounds = 2)


ggsave("manuscript/fig/fig_dept_interview_heatmap.pdf",     interview_heatmap$plot,
       width = 10, height = 5, device = cairo_pdf)
ggsave("manuscript/fig/fig_dept_hiring_heatmap.pdf",        hiring_heatmap$plot,
       width = 10, height = 5, device = cairo_pdf)
ggsave("manuscript/fig/fig_department_welfare.pdf",         dept_welfare$plot,
       width = 7,  height = 5, device = cairo_pdf)
ggsave("manuscript/fig/fig_candidate_welfare_by_tier.pdf",  cand_welfare$plot,
       width = 7,  height = 5, device = cairo_pdf)
ggsave("manuscript/fig/fig_candidate_by_participation.pdf", cand_participation$plot,
       width = 10, height = 4, device = cairo_pdf)

saveRDS(interview_heatmap,  "output/interview_heatmap.rds")
saveRDS(hiring_heatmap,     "output/hiring_heatmap.rds")
saveRDS(dept_welfare,       "output/dept_welfare.rds")
saveRDS(cand_welfare,       "output/cand_welfare.rds")
saveRDS(cand_participation, "output/cand_participation.rds")
