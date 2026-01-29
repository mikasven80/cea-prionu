# Define parameters to vary in DSA (same as PSA)
get_dsa_params <- function(cfg) {
  list(
    list(name = "HR_tx", label = "Hazard ratio (treatment)", base = cfg$HR_tx),
    list(name = "c_MI", label = "MI event cost", base = cfg$c_MI),
    list(name = "c_PostMI", label = "Post-MI annual cost", base = cfg$c_PostMI),
    list(name = "c_tx_annual", label = "Annual treatment cost", base = cfg$c_tx_annual),
    list(name = "c_survey_per_person", label = "Survey cost per person", base = cfg$c_survey_per_person),
    list(name = "c_ct", label = "CT scan cost", base = cfg$c_ct),
    list(name = "c_incidental_follow", label = "Incidental finding follow-up cost", base = cfg$c_incidental_follow),
    list(name = "p_resp", label = "Survey response rate", base = cfg$p_resp),
    list(name = "p_highrisk", label = "Proportion high-risk", base = cfg$p_highrisk),
    list(name = "p_attendCT", label = "CT attendance rate", base = cfg$p_attendCT),
    list(name = "p_cac100", label = "Proportion CAC>100", base = cfg$p_cac100),
    list(name = "p_incidental", label = "Incidental finding rate", base = cfg$p_incidental)
  )
}

run_dsa <- function(cfg, tables, disc_tbl, pct_change = 0.20) {
  params <- get_dsa_params(cfg)

  # Get base case ICER
  soc_base <- build_and_run_soc(cfg, tables)
  int_base <- build_and_run_intervention(cfg, tables)
  tot_soc_base <- extract_discounted_totals_pp(soc_base, disc_tbl, cfg$cohort_size)
  tot_int_base <- extract_discounted_totals_pp(int_base, disc_tbl, cfg$cohort_size)

  inc_cost_base <- tot_int_base$dcost_pp - tot_soc_base$dcost_pp
  inc_qaly_base <- tot_int_base$dqaly_pp - tot_soc_base$dqaly_pp
  icer_base <- calculate_icer(inc_cost_base, inc_qaly_base)

  results <- vector("list", length(params))

  for (i in seq_along(params)) {
    param <- params[[i]]
    param_name <- param$name
    base_val <- param$base

    # Calculate low and high values
    # For probabilities, ensure bounds [0, 1]
    # For HR, cap at 1.0 on high end
    low_val <- base_val * (1 - pct_change)
    high_val <- base_val * (1 + pct_change)

    if (grepl("^p_", param_name)) {
      low_val <- max(0, low_val)
      high_val <- min(1, high_val)
    }
    if (param_name == "HR_tx") {
      high_val <- min(1, high_val)
    }

    # Run model with low value
    cfg_low <- cfg
    cfg_low[[param_name]] <- low_val

    soc_low <- build_and_run_soc(cfg_low, tables)
    int_low <- build_and_run_intervention(cfg_low, tables)
    tot_soc_low <- extract_discounted_totals_pp(soc_low, disc_tbl, cfg_low$cohort_size)
    tot_int_low <- extract_discounted_totals_pp(int_low, disc_tbl, cfg_low$cohort_size)

    inc_cost_low <- tot_int_low$dcost_pp - tot_soc_low$dcost_pp
    inc_qaly_low <- tot_int_low$dqaly_pp - tot_soc_low$dqaly_pp
    icer_low <- suppressWarnings(calculate_icer(inc_cost_low, inc_qaly_low))

    # Run model with high value
    cfg_high <- cfg
    cfg_high[[param_name]] <- high_val

    soc_high <- build_and_run_soc(cfg_high, tables)
    int_high <- build_and_run_intervention(cfg_high, tables)
    tot_soc_high <- extract_discounted_totals_pp(soc_high, disc_tbl, cfg_high$cohort_size)
    tot_int_high <- extract_discounted_totals_pp(int_high, disc_tbl, cfg_high$cohort_size)

    inc_cost_high <- tot_int_high$dcost_pp - tot_soc_high$dcost_pp
    inc_qaly_high <- tot_int_high$dqaly_pp - tot_soc_high$dqaly_pp
    icer_high <- suppressWarnings(calculate_icer(inc_cost_high, inc_qaly_high))

    results[[i]] <- tibble(
      parameter = param_name,
      label = param$label,
      base_value = base_val,
      low_value = low_val,
      high_value = high_val,
      icer_base = icer_base,
      icer_low = icer_low,
      icer_high = icer_high
    )
  }

  bind_rows(results)
}

plot_tornado <- function(dsa_res, title = "Tornado Diagram: One-Way Sensitivity Analysis") {
  plot_data <- dsa_res %>%
    mutate(
      icer_min = pmin(icer_low, icer_high),
      icer_max = pmax(icer_low, icer_high),
      spread = icer_max - icer_min
    ) %>%
    arrange(desc(spread)) %>%
    mutate(label = factor(label, levels = rev(label)))

  icer_base <- plot_data$icer_base[1]

  # Create tornado using geom_segment
  p <- ggplot(plot_data, aes(y = label)) +
    geom_segment(
      aes(x = icer_min, xend = icer_max, yend = label),
      linewidth = 8,
      color = "grey70"
    ) +
    geom_point(
      aes(x = icer_low, color = "Low (-20%)"),
      size = 3
    ) +
    geom_point(
      aes(x = icer_high, color = "High (+20%)"),
      size = 3
    ) +
    geom_vline(xintercept = icer_base, linetype = "dashed", color = "black", linewidth = 0.8) +
    annotate(
      "text",
      x = icer_base,
      y = 0.5,
      label = paste0("Base case\n", format(round(icer_base), big.mark = ",")),
      hjust = 0.5,
      vjust = 1,
      size = 3
    ) +
    scale_color_manual(
      values = c("Low (-20%)" = "#2166AC", "High (+20%)" = "#B2182B"),
      name = "Parameter value"
    ) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(
      title = title,
      x = "ICER (SEK per QALY gained)",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.y = element_text(size = 10)
    )

  p
}

# Summary table for export
summarize_dsa <- function(dsa_res) {
  dsa_res %>%
    mutate(
      spread = abs(icer_high - icer_low),
      pct_change_from_base = spread / icer_base * 100
    ) %>%
    arrange(desc(spread)) %>%
    select(
      Parameter = label,
      `Base Value` = base_value,
      `Low Value (-20%)` = low_value,
      `High Value (+20%)` = high_value,
      `ICER at Low` = icer_low,
      `ICER at High` = icer_high,
      `ICER Spread` = spread,
      `% Change from Base` = pct_change_from_base
    )
}
