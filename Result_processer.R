#!/usr/bin/env Rscript
# =============================================================================
# STAGE 3: COMBINE batch results into final all_sim_results structure
# Usage: Rscript Result_processer.R
# =============================================================================
source("Sim_Functions.R")

cat("=== COMBINING RESULTS ===\n")

n_batches <- 10L

# ── Load burn-in artifacts (for departments, config, burn-in results) ──
artifacts <- readRDS("burn_in_artifacts.rds")
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
  f <- sprintf("sim_batch_%d.rds", b)
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

# ── Assemble into the same structure as run_multi_replicate_simulation ──
all_sim_results <- list(
  departments = departments,
  questions   = questions,
  participation_sets = NULL,
  config = list(
    n_candidates        = cfg$n_candidates,
    burn_in_years       = cfg$burn_in_years,
    sim_years           = cfg$sim_years,
    participation_rates = cfg$participation_rates,
    base_seed           = cfg$base_seed,
    n_replicates        = n_batches * 20L,
    alpha               = cfg$alpha,
    L_repeats           = cfg$L_repeats,
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
  nr <- nrow(all_sim_results$sim_results[[rc]]$results)
  n_rep <- length(unique(all_sim_results$sim_results[[rc]]$results$replicate))
  cat(sprintf("  Rate %5s: %d result rows, %d replicates\n", rc, nr, n_rep))
}


#all_sim_results <- readRDS("all_sim_results_200.rds")
saveRDS(all_sim_results, "all_sim_results.rds", compress = "gzip")
cat(sprintf("\nSaved all_sim_results.rds (%.1f MB)\n",
            file.size("all_sim_results.rds") / 1e6))

# ── Generate figures ──
cat("\nGenerating figures...\n")

(interview_heatmap  <- make_fig_dept_interview_heatmap(all_sim_results, year_filter = c(1, 10), include_scramble = FALSE))$plot
(hiring_heatmap     <- make_fig_dept_hiring_heatmap(all_sim_results, year_filter = c(1, 10), include_scramble = T, hire_rounds = 2))$plot
(dept_welfare       <- make_fig_department_welfare_by_tier_normalized(all_sim_results, year_filter = c(1, 10), include_scramble = T, hire_rounds = 2))$plot_welfare_per_slot
(cand_welfare       <- make_fig_candidate_welfare_by_tier_revised(all_sim_results, year_filter = c(1, 10), include_scramble = T, hire_rounds = 2))$plot_conditional
(cand_participation <- make_fig_candidate_by_participation(all_sim_results, year_filter = c(1, 10), include_scramble = T, hire_rounds = 2))$plot

#cand_welfare$plot_unconditional_gains




ggsave("fig_dept_interview_heatmap.pdf",  interview_heatmap$plot,              width = 10, height = 5, device = cairo_pdf)
ggsave("fig_dept_hiring_heatmap.pdf",     hiring_heatmap$plot,                 width = 10, height = 5, device = cairo_pdf)
ggsave("fig_department_welfare.pdf",      dept_welfare$plot_welfare_per_slot,   width = 7,  height = 5, device = cairo_pdf)
ggsave("fig_candidate_welfare_by_tier.pdf", cand_welfare$plot_conditional,     width = 7,  height = 5, device = cairo_pdf)
ggsave("fig_candidate_by_participation.pdf", cand_participation$plot,          width = 10, height = 4, device = cairo_pdf)

cat("All figures saved. Done.\n")


saveRDS(interview_heatmap, "interview_heatmap.rds")
saveRDS(hiring_heatmap, "hiring_heatmap.rds")
saveRDS(dept_welfare, "dept_welfare.rds")
saveRDS(cand_welfare, "cand_welfare.rds")
saveRDS(cand_participation, "cand_participation.rds")












###### DIAGNOSTICS ########


total_change <- hiring_heatmap$data %>%
  group_by(scenario, prestige_tier) %>%
  summarise(total_mean_n = sum(mean_n), .groups = "drop")%>%
  pivot_wider(
    names_from  = prestige_tier,
    values_from = total_mean_n,
    values_fill = 0
  ) %>%
  arrange(scenario)


pct_change <- hiring_heatmap$data %>%
  group_by(scenario, prestige_tier) %>%
  summarise(total_mean_n = sum(mean_n, na.rm = TRUE), .groups = "drop") %>%
  left_join(hiring_heatmap$hiring_quotas, by = "prestige_tier") %>%
  mutate(mean_n_per_slot = total_mean_n / quota) %>%
  dplyr::select(scenario, prestige_tier, mean_n_per_slot) %>%
  pivot_wider(
    names_from  = prestige_tier,
    values_from = mean_n_per_slot
  )


total_change; pct_change
