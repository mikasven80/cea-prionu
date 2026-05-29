# Subgroup analysis by socioeconomic geographic area
# -------------------------------------------------------
# Reads an Excel file with screening funnel parameters for
# each subgroup, runs the deterministic base-case model for
# each, and produces a summary table.
#
# Expected Excel format (one sheet, one row per subgroup):
#   subgroup | p_resp | p_highrisk | p_attendCT | p_cac100 | p_uptake
#   Area_1   | 0.35   | 0.25       | 0.92       | 0.28     | 1.00
#   Area_2   | 0.28   | 0.32       | 0.88       | 0.31     | 1.00
#   ...
#
# Any column not present in the Excel file keeps its base-case value.
# p_uptake column is optional (defaults to base-case if absent).

run_subgroup_analysis <- function(cfg, tables, disc_tbl, subgroup_xlsx_path) {

  stopifnot(file.exists(subgroup_xlsx_path))

  sg_raw <- read_excel(subgroup_xlsx_path, sheet = 1)

  # Clean column names: lowercase, trim whitespace
  names(sg_raw) <- names(sg_raw) %>%
    str_trim() %>%
    str_replace_all("[^A-Za-z0-9_]+", "_") %>%
    str_to_lower()

  stopifnot("subgroup" %in% names(sg_raw))

  # Parameters that can be overridden
  funnel_params <- c("p_resp", "p_highrisk", "p_attendct", "p_cac100", "p_uptake")

  # Map lowercase column names to cfg parameter names
  col_to_cfg <- c(
    p_resp     = "p_resp",
    p_highrisk = "p_highrisk",
    p_attendct = "p_attendCT",
    p_cac100   = "p_cac100",
    p_uptake   = "p_uptake"
  )

  results <- vector("list", nrow(sg_raw))

  for (i in seq_len(nrow(sg_raw))) {
    sg_name <- as.character(sg_raw$subgroup[i])
    cfg_i <- cfg

    # Override funnel parameters from Excel
    for (col in names(col_to_cfg)) {
      if (col %in% names(sg_raw)) {
        val <- as.numeric(sg_raw[[col]][i])
        if (!is.na(val)) {
          cfg_i[[ col_to_cfg[col] ]] <- val
        }
      }
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
    p_scan  <- cfg_i$p_resp * cfg_i$p_highrisk * cfg_i$p_attendCT
    p_treat <- p_scan * cfg_i$p_cac100 * cfg_i$p_uptake
    n_treated <- round(cfg_i$cohort_size * p_treat, 1)

    results[[i]] <- tibble(
      subgroup       = sg_name,
      p_resp         = cfg_i$p_resp,
      p_highrisk     = cfg_i$p_highrisk,
      p_attendCT     = cfg_i$p_attendCT,
      p_cac100       = cfg_i$p_cac100,
      p_uptake       = cfg_i$p_uptake,
      n_treated      = n_treated,
      pct_treated    = round(p_treat * 100, 2),
      cost_soc       = round(tot_soc$dcost_pp, 0),
      cost_int       = round(tot_int$dcost_pp, 0),
      qaly_soc       = round(tot_soc$dqaly_pp, 5),
      qaly_int       = round(tot_int$dqaly_pp, 5),
      inc_cost       = round(inc_cost, 0),
      inc_qaly       = round(inc_qaly, 5),
      icer           = round(icer, 0)
    )
  }

  bind_rows(results)
}
