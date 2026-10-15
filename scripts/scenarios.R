# Scenario analyses
# -------------------------------------------------------
# Deterministic re-runs of the base-case model under
# alternative assumptions reported in the manuscript:
#   (1) the screening funnel as observed in the PRIO-Now
#       evaluation study, instead of the rounded base-case
#       values (8,000 invited -> 2,377 completed the
#       questionnaire -> 824 classified high-risk -> 563
#       completed the CT examination)
#   (2) treatment uptake below 100%
#
# Each scenario overrides one or more cfg entries and is
# otherwise identical to the base case.

# Funnel observed in the PRIO-Now evaluation study
PRIONOW_OBSERVED_FUNNEL <- list(
  p_resp     = 2377 / 8000,   # 29.7% completed the questionnaire
  p_highrisk = 824 / 2377,    # 34.7% above the 15.3% risk threshold
  p_attendCT = 563 / 824,     # 68.3% of those invited completed the CT
  p_cac100   = 0.31           # CAC >= 100 among those scanned
)

get_scenarios <- function(cfg) {
  scen <- list(
    list(label = "Base case", overrides = list()),
    list(label = "Observed PRIO-Now funnel", overrides = PRIONOW_OBSERVED_FUNNEL)
  )

  for (u in c(0.90, 0.80, 0.70, 0.60)) {
    scen[[length(scen) + 1]] <- list(
      label     = sprintf("Treatment uptake %d%%", round(u * 100)),
      overrides = list(p_uptake = u)
    )
  }

  # High-cost incidental findings: the previous base-case assumption, retained to
  # cover less well-defined downstream use such as reassurance consultations and
  # additional primary-care contacts, for which no data are available.
  scen[[length(scen) + 1]] <- list(
    label     = "High-cost incidental findings (15,000 SEK per finding)",
    overrides = list(c_incidental_follow = 15000)
  )

  # Alternative treatment-effect estimates. The base case is the Gallone adjusted
  # pooled estimate for CAC >100, converted from an odds ratio to a hazard ratio.
  eff <- list(
    c(0.46,  "SCAPIS, CAC >100 (HR 0.46)"),
    c(0.617, "CTT, 1.85 mmol/L LDL-C reduction (HR 0.62)"),
    c(0.679, "CTT, 1.48 mmol/L LDL-C reduction (HR 0.68)"),
    c(0.700, "Gallone unadjusted, CAC >100 (HR 0.70)")
  )
  for (e in eff) {
    scen[[length(scen) + 1]] <- list(
      label     = sprintf("Effect estimate: %s", e[2]),
      overrides = list(HR_tx = as.numeric(e[1]))
    )
  }

  # Alternative anchors for the MI risk of the CAC >=100 stratum
  risk <- list(
    c(1.00, "cohort-average risk (unstratified)"),
    c(0.905, "observed CAC >100 risk, 10-year 5.0%"),
    c(1.452, "untreated CAC >100 risk age-adjusted, 10-year 7.9%")
  )
  for (r in risk) {
    scen[[length(scen) + 1]] <- list(
      label     = sprintf("Baseline risk: %s", r[2]),
      overrides = list(cac100_risk_mult = as.numeric(r[1]))
    )
  }

  scen
}

run_scenario_analysis <- function(cfg, tables, disc_tbl, scenarios = NULL) {

  if (is.null(scenarios)) scenarios <- get_scenarios(cfg)

  results <- vector("list", length(scenarios))

  for (i in seq_along(scenarios)) {
    scen <- scenarios[[i]]
    cfg_i <- cfg

    for (nm in names(scen$overrides)) {
      if (!nm %in% names(cfg_i)) {
        stop("Unknown config parameter in scenario '", scen$label, "': ", nm)
      }
      cfg_i[[nm]] <- scen$overrides[[nm]]
    }

    # Run both arms
    soc_i <- build_and_run_soc(cfg_i, tables)
    int_i <- build_and_run_intervention(cfg_i, tables)

    # Extract discounted totals per person
    tot_soc <- extract_discounted_totals_pp(soc_i, disc_tbl, cfg_i$cohort_size)
    tot_int <- extract_discounted_totals_pp(int_i, disc_tbl, cfg_i$cohort_size)

    inc_cost <- tot_int$dcost_pp - tot_soc$dcost_pp
    inc_qaly <- tot_int$dqaly_pp - tot_soc$dqaly_pp
    icer <- suppressWarnings(calculate_icer(inc_cost, inc_qaly))

    # Screening funnel summary
    p_treat <- cfg_i$p_resp * cfg_i$p_highrisk * cfg_i$p_attendCT *
      cfg_i$p_cac100 * cfg_i$p_uptake

    fx <- cfg_i$fx_sek_per_eur

    results[[i]] <- tibble(
      scenario   = scen$label,
      p_resp     = cfg_i$p_resp,
      p_highrisk = cfg_i$p_highrisk,
      p_attendCT = cfg_i$p_attendCT,
      p_cac100   = cfg_i$p_cac100,
      p_uptake   = cfg_i$p_uptake,
      HR_tx      = cfg_i$HR_tx,
      cac100_risk_mult = cfg_i$cac100_risk_mult,
      n_treated  = round(cfg_i$cohort_size * p_treat, 1),
      inc_cost   = round(inc_cost, 0),
      inc_qaly   = round(inc_qaly, 5),
      icer       = round(icer, 0),
      inc_cost_eur = round(inc_cost / fx, 0),
      icer_eur     = round(icer / fx, 0)
    )
  }

  bind_rows(results)
}
