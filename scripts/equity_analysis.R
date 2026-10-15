# Equity analysis by socioeconomic geographic area
# -------------------------------------------------------
# Extends the subgroup analysis from quartile-specific ICERs to
# quantities that are sensitive to differential participation:
#
#   * outcomes per 1,000 people INVITED (not per person treated)
#   * net health benefit, NHB = dQALY - dCost / lambda
#   * a stage-wise counterfactual decomposition of the funnel,
#     giving the most-deprived group the most-affluent group's
#     value at one stage at a time
#
# Reads the same Excel file as run_subgroup_analysis(). The first
# row is taken as the most deprived group and the last row as the
# most affluent; order the sheet accordingly.

# Run the full model for one funnel configuration and return
# per-1,000-invited outcomes alongside the ICER.
run_equity_arm <- function(cfg, tables, disc_tbl, label,
                           p_resp, p_highrisk, p_attendCT, p_cac100,
                           lambda_sek) {

  cfg_i <- cfg
  cfg_i$p_resp     <- p_resp
  cfg_i$p_highrisk <- p_highrisk
  cfg_i$p_attendCT <- p_attendCT
  cfg_i$p_cac100   <- p_cac100

  soc_i <- build_and_run_soc(cfg_i, tables)
  int_i <- build_and_run_intervention(cfg_i, tables)

  tot_soc <- extract_discounted_totals_pp(soc_i, disc_tbl, cfg_i$cohort_size)
  tot_int <- extract_discounted_totals_pp(int_i, disc_tbl, cfg_i$cohort_size)

  inc_cost <- tot_int$dcost_pp - tot_soc$dcost_pp   # SEK per person invited
  inc_qaly <- tot_int$dqaly_pp - tot_soc$dqaly_pp

  p_scan  <- p_resp * p_highrisk * p_attendCT
  p_treat <- p_scan * cfg_i$p_cac100 * cfg_i$p_uptake

  tibble(
    label           = label,
    p_resp          = p_resp,
    p_highrisk      = p_highrisk,
    p_attendCT      = p_attendCT,
    p_cac100        = p_cac100,
    scanned_per1000 = round(p_scan  * 1000, 1),
    treated_per1000 = round(p_treat * 1000, 1),
    # per 1,000 invited; costs converted to EUR for reporting
    inc_cost_eur_per1000 = round(inc_cost / cfg_i$fx_sek_per_eur * 1000, 0),
    qaly_per1000         = round(inc_qaly * 1000, 3),
    # net health benefit, QALYs per 1,000 invited
    nhb_per1000          = round((inc_qaly - inc_cost / lambda_sek) * 1000, 3),
    icer_eur             = round(
      suppressWarnings(calculate_icer(inc_cost, inc_qaly)) / cfg_i$fx_sek_per_eur, 0)
  )
}

run_equity_analysis <- function(cfg, tables, disc_tbl, subgroup_xlsx_path,
                                lambda_sek = 500000) {

  stopifnot(file.exists(subgroup_xlsx_path))

  sg <- read_excel(subgroup_xlsx_path, sheet = 1)
  names(sg) <- names(sg) %>%
    str_trim() %>%
    str_replace_all("[^A-Za-z0-9_]+", "_") %>%
    str_to_lower()

  needed <- c("subgroup", "p_resp", "p_highrisk", "p_attendct", "p_cac100")
  stopifnot(all(needed %in% names(sg)))

  # keep complete rows only (the sheet carries trailing blanks)
  sg <- sg %>%
    filter(!is.na(subgroup), !is.na(p_resp), !is.na(p_highrisk),
           !is.na(p_attendct), !is.na(p_cac100))
  stopifnot(nrow(sg) >= 2)

  one <- function(label, p_resp, p_highrisk, p_attendCT, p_cac100) {
    run_equity_arm(cfg, tables, disc_tbl, label,
                   p_resp, p_highrisk, p_attendCT, p_cac100, lambda_sek)
  }

  # 1) observed funnel in each group, plus the pooled base case
  by_group <- bind_rows(lapply(seq_len(nrow(sg)), function(i)
    one(as.character(sg$subgroup[i]),
        sg$p_resp[i], sg$p_highrisk[i], sg$p_attendct[i], sg$p_cac100[i])))

  pooled <- one("Pooled base case",
                cfg$p_resp, cfg$p_highrisk, cfg$p_attendCT, cfg$p_cac100)

  # 2) stage-wise counterfactuals: most-deprived funnel, one stage
  #    at a time replaced by the most-affluent value
  lo <- sg[1, ]              # most deprived
  hi <- sg[nrow(sg), ]       # most affluent
  tag <- function(stage) paste0(lo$subgroup, " + ", hi$subgroup, " ", stage)

  decomp <- bind_rows(
    one(tag("letter response"),
        hi$p_resp, lo$p_highrisk, lo$p_attendct, lo$p_cac100),
    one(tag("high-risk share"),
        lo$p_resp, hi$p_highrisk, lo$p_attendct, lo$p_cac100),
    one(tag("CT attendance"),
        lo$p_resp, lo$p_highrisk, hi$p_attendct, lo$p_cac100),
    one(tag("CAC>=100 share"),
        lo$p_resp, lo$p_highrisk, lo$p_attendct, hi$p_cac100),
    one(tag("participation (both stages)"),
        hi$p_resp, lo$p_highrisk, hi$p_attendct, lo$p_cac100)
  )

  # 3) headline gaps, and the outreach budget that the participation
  #    gap would justify at the cost-effectiveness threshold
  lambda_eur <- lambda_sek / cfg$fx_sek_per_eur
  nhb_gain   <- decomp$nhb_per1000[5] - by_group$nhb_per1000[1]

  gaps <- tibble(
    measure = c(
      "ICER spread across groups (%)",
      "QALYs per 1,000 invited, most deprived vs most affluent (%)",
      "QALYs per 1,000 invited, most deprived vs best group (%)",
      "NHB per 1,000 invited, most deprived vs most affluent (%)",
      "NHB gain per 1,000 invited if participation were equalised",
      "Break-even outreach spend per invitee (EUR)"
    ),
    value = c(
      round(100 * (max(by_group$icer_eur) - min(by_group$icer_eur)) /
              min(by_group$icer_eur), 1),
      round(100 * by_group$qaly_per1000[1] / by_group$qaly_per1000[nrow(by_group)], 0),
      round(100 * by_group$qaly_per1000[1] / max(by_group$qaly_per1000), 0),
      round(100 * by_group$nhb_per1000[1] / by_group$nhb_per1000[nrow(by_group)], 0),
      round(nhb_gain, 3),
      round(nhb_gain / 1000 * lambda_eur, 0)
    )
  )

  list(by_group = bind_rows(by_group, pooled),
       decomposition = decomp,
       gaps = gaps)
}
