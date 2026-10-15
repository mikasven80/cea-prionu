rm(list = ls())

# Install missing packages if needed
required_packages <- c("dplyr", "heemod", "tibble", "tidyr", "ggplot2",
                       "readxl", "writexl", "stringr", "geomtextpath")
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

# Load all packages (centralized here - not in individual scripts)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(heemod)
library(readxl)
library(writexl)
library(stringr)
library(geomtextpath)

# Source modules (folder is "scripts")
source("scripts/config_basecase.R")
source("scripts/read_inputs.R")
source("scripts/helpers.R")
source("scripts/build_models.R")
source("scripts/extract_outputs.R")
source("scripts/psa.R")
source("scripts/dsa.R")
source("scripts/plots.R")
source("scripts/export.R")
source("scripts/scenarios.R")
source("scripts/subgroup_analysis.R")
source("scripts/equity_analysis.R")

#-----------------------------
# 1) Load config + inputs
#-----------------------------
cfg <- get_config_basecase()
tables <- read_model_inputs(cfg$xlsx_path, cfg$age_start, cfg$cycles)
disc_tbl <- make_disc_tbl(cfg$age_start, cfg$cycles, cfg$disc)

#-----------------------------
# 2) Deterministic base case
#-----------------------------
res_soc <- build_and_run_soc(cfg, tables)
res_int <- build_and_run_intervention(cfg, tables)

tot_soc <- extract_discounted_totals_pp(res_soc, disc_tbl, cfg$cohort_size) %>% mutate(strategy = "SoC")
tot_int <- extract_discounted_totals_pp(res_int, disc_tbl, cfg$cohort_size) %>% mutate(strategy = "Intervention")

det_summary <- bind_rows(tot_soc, tot_int) %>%
  select(strategy, dcost_pp, dqaly_pp)

inc_cost_pp <- tot_int$dcost_pp - tot_soc$dcost_pp
inc_qaly_pp <- tot_int$dqaly_pp - tot_soc$dqaly_pp

det_icer <- tibble(
  inc_cost_pp = inc_cost_pp,
  inc_qaly_pp = inc_qaly_pp,
  icer = calculate_icer(inc_cost_pp, inc_qaly_pp),
  p_scan  = attr(res_int, "p_scan"),
  p_treat = attr(res_int, "p_treat"),
  c_program_0 = attr(res_int, "c_program_0"),
  HR_tx = cfg$HR_tx
)

# Cost per cycle per strategy (cohort + per-person; undiscounted + discounted)
cost_cycle_soc <- extract_cost_per_cycle(res_soc, "SoC", disc_tbl, cfg$cohort_size)
cost_cycle_int <- extract_cost_per_cycle(res_int, "Intervention", disc_tbl, cfg$cohort_size)
cost_per_cycle <- bind_rows(cost_cycle_soc, cost_cycle_int) %>%
  arrange(strategy, model_time)

# State membership table (cycle on horizontal, states on vertical for both arms)
state_membership <- extract_state_membership(res_soc, res_int, cfg$cohort_size)

# Cost disaggregation per person, in EUR (reviewer 2, comment 6)
cost_components <- bind_rows(
  extract_cost_components(res_soc, "Standard of care", disc_tbl, cfg$cohort_size, cfg$fx_sek_per_eur),
  extract_cost_components(res_int, "PRIO-Now",         disc_tbl, cfg$cohort_size, cfg$fx_sek_per_eur)
) %>% tidyr::pivot_wider(names_from = strategy, values_from = cost_pp) %>%
  mutate(Incremental = `PRIO-Now` - `Standard of care`)
print(cost_components)

#-----------------------------
# 3) DSA (One-way sensitivity analysis)
#-----------------------------
dsa_res <- run_dsa(cfg, tables, disc_tbl, pct_change = 0.20)
dsa_summary <- summarize_dsa(dsa_res)
p_tornado <- plot_tornado(dsa_res)

# HR threshold analysis
hr_icer_tbl <- run_hr_threshold_analysis(cfg, tables, disc_tbl,
                                          hr_range = seq(0.30, 0.95, by = 0.01))
threshold_hr <- find_threshold_hr(hr_icer_tbl, target_icer = 500000)
message("Threshold HR for ICER = 500,000: ", round(threshold_hr, 4))
p_hr_icer <- plot_hr_vs_icer(hr_icer_tbl, target_icer = 500000, base_hr = cfg$HR_tx)

#-----------------------------
# 4) Scenario analyses (observed funnel; treatment uptake)
#-----------------------------
scenario_res <- run_scenario_analysis(cfg, tables, disc_tbl)
print(scenario_res)

#-----------------------------
# 5) PSA
#-----------------------------
psa_res <- run_psa(cfg, tables, disc_tbl)

# CEAC
wtp_grid <- seq(0, cfg$psa$wtp_max, by = cfg$psa$wtp_by)
ceac_tbl <- compute_ceac(psa_res, wtp_grid)

# Plots
p_ceplane      <- plot_ce_plane(psa_res)
p_ceac         <- plot_ceac(ceac_tbl)
p_states       <- plot_state_membership(state_membership)
p_states_facet <- plot_state_membership_faceted(state_membership)
p_states_diff  <- plot_state_membership_diff(state_membership)

#-----------------------------
# 6) Export outputs
#-----------------------------
export_xlsx(
  named_sheets = list(
    Deterministic_Summary = det_summary,
    Deterministic_ICER    = det_icer,
    Cost_per_cycle        = cost_per_cycle,
    State_membership      = state_membership,
    Cost_components       = cost_components,
    DSA_Results           = dsa_summary,
    HR_threshold_analysis = hr_icer_tbl,
    Scenario_Analyses     = scenario_res,
    PSA_draws_and_outcomes = psa_res,
    CEAC = ceac_tbl
  ),
  path = "outputs/PrioNU_CEA_outputs.xlsx"
)

save_plot_png(p_tornado,      "outputs/Tornado_DSA.png", width = 10, height = 7)
save_plot_png(p_hr_icer,      "outputs/HR_vs_ICER.png", width = 8, height = 6)
save_plot_png(p_ceplane,      "outputs/CE_plane.png", width = 8, height = 6)
save_plot_png(p_ceac,         "outputs/CEAC.png",     width = 8, height = 6)

# Submission figures: 600 dpi PNG (+ vector where available) in outputs/figures/
save_figure(p_tornado,     "fig2",  width = 10, height = 7)
save_figure(p_ceplane,     "fig3",  width = 8,  height = 6)
save_figure(p_ceac,        "figs1", width = 8,  height = 6)
save_figure(p_hr_icer,     "figs2", width = 8,  height = 6)
save_figure(p_states_diff, "figs3", width = 10, height = 5)
save_plot_png(p_states,       "outputs/State_membership.png", width = 10, height = 6)
save_plot_png(p_states_facet, "outputs/State_membership_faceted.png", width = 10, height = 5)
save_plot_png(p_states_diff,  "outputs/State_membership_diff.png", width = 10, height = 5)

# Save R objects for reproducibility
saveRDS(
  list(
    cfg = cfg,
    tables = tables,
    disc_tbl = disc_tbl,
    res_soc = res_soc,
    res_int = res_int,
    dsa_res = dsa_res,
    scenario_res = scenario_res,
    psa_res = psa_res,
    ceac_tbl = ceac_tbl
  ),
  file = "outputs/model_objects.rds"
)

#-----------------------------
# 7) Subgroup analysis (socioeconomic areas)
#-----------------------------
subgroup_xlsx <- "data/subgroup funnel.xlsx"
if (file.exists(subgroup_xlsx)) {
  message("Running subgroup analysis...")
  subgroup_res <- run_subgroup_analysis(cfg, tables, disc_tbl, subgroup_xlsx)
  print(subgroup_res)
  write_xlsx(list(Subgroup_Results = subgroup_res),
             path = "outputs/PrioNU_Subgroup_Results.xlsx")
  message("Subgroup results written to outputs/PrioNU_Subgroup_Results.xlsx")
} else {
  message("Subgroup file not found at ", subgroup_xlsx, " -- skipping subgroup analysis.")
}

#-----------------------------
# 8) Equity analysis (participation gradients by socioeconomic area)
#-----------------------------
if (file.exists(subgroup_xlsx)) {
  message("Running equity analysis...")
  equity_res <- run_equity_analysis(cfg, tables, disc_tbl, subgroup_xlsx)
  print(as.data.frame(equity_res$by_group), row.names = FALSE)
  print(as.data.frame(equity_res$decomposition), row.names = FALSE)
  print(as.data.frame(equity_res$gaps), row.names = FALSE)
  write_xlsx(list(By_Group      = equity_res$by_group,
                  Decomposition = equity_res$decomposition,
                  Headline_Gaps = equity_res$gaps),
             path = "outputs/PrioNU_Equity_Results.xlsx")
  message("Equity results written to outputs/PrioNU_Equity_Results.xlsx")
} else {
  message("Subgroup file not found at ", subgroup_xlsx, " -- skipping equity analysis.")
}

message("Done. Outputs written to outputs/ folder.")
