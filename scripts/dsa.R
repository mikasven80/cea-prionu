# Define parameters to vary in DSA (same as PSA)
get_dsa_params <- function(cfg) {
  list(
    list(name = "HR_tx", label = "Hazard ratio (treatment)", base = cfg$HR_tx),
    list(name = "c_MI", label = "MI event cost", base = cfg$c_MI),
    list(name = "c_PostMI", label = "Post-MI annual cost", base = cfg$c_PostMI),
    list(name = "c_tx_annual", label = "Annual statin treatment cost", base = cfg$c_tx_annual),
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

plot_tornado <- function(dsa_res, title = "Tornado Diagram: One-Way Sensitivity Analysis",
                         fx = 10.8, currency = "EUR") {
  plot_data <- dsa_res %>%
    mutate(
      icer_low  = icer_low / fx,
      icer_high = icer_high / fx,
      icer_base = icer_base / fx,
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
    coord_cartesian(clip = "off") +
    scale_color_manual(
      values = c("Low (-20%)" = "#2166AC", "High (+20%)" = "#B2182B"),
      name = "Parameter value"
    ) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(
      title = title,
      subtitle = paste0("Base case ICER: ", format(round(icer_base), big.mark = ","), " ", currency, "/QALY"),
      x = paste0("ICER (", currency, " per QALY gained)"),
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
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

# Threshold analysis: ICER as a function of HR
run_hr_threshold_analysis <- function(cfg, tables, disc_tbl,
                                       hr_range = seq(0.1, 0.9, by = 0.05)) {
  results <- vector("list", length(hr_range))

  for (i in seq_along(hr_range)) {
    cfg_i <- cfg
    cfg_i$HR_tx <- hr_range[i]

    soc <- build_and_run_soc(cfg_i, tables)
    int <- build_and_run_intervention(cfg_i, tables)

    tot_soc <- extract_discounted_totals_pp(soc, disc_tbl, cfg_i$cohort_size)
    tot_int <- extract_discounted_totals_pp(int, disc_tbl, cfg_i$cohort_size)

    inc_cost <- tot_int$dcost_pp - tot_soc$dcost_pp
    inc_qaly <- tot_int$dqaly_pp - tot_soc$dqaly_pp
    icer <- suppressWarnings(inc_cost / inc_qaly)

    results[[i]] <- tibble(
      HR_tx = hr_range[i],
      inc_cost = inc_cost,
      inc_qaly = inc_qaly,
      icer = icer
    )
  }

  bind_rows(results)
}

find_threshold_hr <- function(hr_icer_tbl, target_icer = 500000) {
  # Linear interpolation to find HR where ICER = target
  df <- hr_icer_tbl %>% arrange(HR_tx)

  # Find interval containing target

  below <- df %>% filter(icer <= target_icer) %>% slice_max(HR_tx, n = 1)
  above <- df %>% filter(icer > target_icer) %>% slice_min(HR_tx, n = 1)


  if (nrow(below) == 0 || nrow(above) == 0) {
    warning("Target ICER outside range of HR values tested.")
    return(NA_real_)
  }

  # Linear interpolation
  hr1 <- below$HR_tx
  hr2 <- above$HR_tx
  icer1 <- below$icer
  icer2 <- above$icer

  hr_threshold <- hr1 + (target_icer - icer1) * (hr2 - hr1) / (icer2 - icer1)
  hr_threshold
}

plot_hr_vs_icer <- function(hr_icer_tbl, target_icer = 500000, base_hr = NULL,
                            fx = 10.8, currency = "EUR") {
  threshold_hr <- find_threshold_hr(hr_icer_tbl, target_icer)
  hr_icer_tbl  <- hr_icer_tbl %>% mutate(icer = icer / fx)
  target_icer_disp <- target_icer / fx

  p <- ggplot(hr_icer_tbl, aes(x = HR_tx, y = icer)) +
    geom_line(linewidth = 1, color = "#2166AC") +
    geom_hline(yintercept = target_icer_disp, linetype = "dashed", color = "#B2182B", linewidth = 0.8) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(
      x = "Hazard Ratio (treatment effect)",
      y = paste0("ICER (", currency, " per QALY gained)"),
      title = "ICER vs Treatment Hazard Ratio"
    ) +
    theme_bw()

  # Add threshold annotation if found

  if (!is.na(threshold_hr)) {
    p <- p +
      geom_vline(xintercept = threshold_hr, linetype = "dotted", color = "#B2182B", linewidth = 0.6) +
      annotate("point", x = threshold_hr, y = target_icer_disp, color = "#B2182B", size = 3) +
      annotate("label", x = threshold_hr, y = target_icer_disp,
               label = paste0("HR = ", round(threshold_hr, 3), "\nICER = ", format(round(target_icer_disp), big.mark = ",")),
               hjust = -0.1, vjust = 0.5, size = 3.5, fill = "white")
  }

  # Add base case marker if provided

  if (!is.null(base_hr)) {
    base_icer <- hr_icer_tbl %>% filter(HR_tx == base_hr) %>% pull(icer)
    if (length(base_icer) == 1) {
      p <- p +
        annotate("point", x = base_hr, y = base_icer, color = "#4D4D4D", size = 3, shape = 17) +
        annotate("label", x = base_hr, y = base_icer,
                 label = paste0("Base case\nHR = ", base_hr),
                 hjust = 1.1, vjust = 0.5, size = 3.5, fill = "white")
    }
  }

  p
}

# Generic parameter threshold analysis
run_param_threshold_analysis <- function(cfg, tables, disc_tbl,
                                          param_name, param_range) {
  results <- vector("list", length(param_range))

  for (i in seq_along(param_range)) {
    cfg_i <- cfg
    cfg_i[[param_name]] <- param_range[i]

    soc <- build_and_run_soc(cfg_i, tables)
    int <- build_and_run_intervention(cfg_i, tables)

    tot_soc <- extract_discounted_totals_pp(soc, disc_tbl, cfg_i$cohort_size)
    tot_int <- extract_discounted_totals_pp(int, disc_tbl, cfg_i$cohort_size)

    inc_cost <- tot_int$dcost_pp - tot_soc$dcost_pp
    inc_qaly <- tot_int$dqaly_pp - tot_soc$dqaly_pp
    icer <- suppressWarnings(inc_cost / inc_qaly)

    results[[i]] <- tibble(
      param_value = param_range[i],
      inc_cost = inc_cost,
      inc_qaly = inc_qaly,
      icer = icer
    )
  }

  bind_rows(results)
}

find_threshold_param <- function(param_icer_tbl, target_icer = 500000) {
  df <- param_icer_tbl %>% arrange(param_value)

  below <- df %>% filter(icer <= target_icer) %>% slice_max(param_value, n = 1)
  above <- df %>% filter(icer > target_icer) %>% slice_min(param_value, n = 1)

  if (nrow(below) == 0 || nrow(above) == 0) {
    warning("Target ICER outside range of parameter values tested.")
    return(NA_real_)
  }

  # Linear interpolation
  v1 <- below$param_value
  v2 <- above$param_value
  icer1 <- below$icer
  icer2 <- above$icer

  threshold <- v1 + (target_icer - icer1) * (v2 - v1) / (icer2 - icer1)
  threshold
}

plot_param_vs_icer <- function(param_icer_tbl, target_icer = 500000, base_value = NULL,
                                x_label = "Parameter value", title = "ICER vs Parameter",
                                x_format = "number") {
  threshold_val <- find_threshold_param(param_icer_tbl, target_icer)

  p <- ggplot(param_icer_tbl, aes(x = param_value, y = icer)) +
    geom_line(linewidth = 1, color = "#2166AC") +
    geom_hline(yintercept = target_icer, linetype = "dashed", color = "#B2182B", linewidth = 0.8) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(
      x = x_label,
      y = "ICER (SEK per QALY gained)",
      title = title
    ) +
    theme_bw()

  # Format x-axis based on type
  if (x_format == "currency") {
    p <- p + scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))
  }

  # Add threshold annotation if found
  if (!is.na(threshold_val)) {
    if (x_format == "currency") {
      threshold_label <- paste0(format(round(threshold_val), big.mark = ","), " SEK\nICER = ", format(target_icer, big.mark = ","))
    } else {
      threshold_label <- paste0(round(threshold_val, 3), "\nICER = ", format(target_icer, big.mark = ","))
    }
    p <- p +
      geom_vline(xintercept = threshold_val, linetype = "dotted", color = "#B2182B", linewidth = 0.6) +
      annotate("point", x = threshold_val, y = target_icer, color = "#B2182B", size = 3) +
      annotate("label", x = threshold_val, y = target_icer,
               label = threshold_label,
               hjust = -0.1, vjust = 0.5, size = 3.5, fill = "white")
  }

  # Add base case marker if provided
  if (!is.null(base_value)) {
    base_icer <- param_icer_tbl %>% filter(param_value == base_value) %>% pull(icer)
    if (length(base_icer) == 1) {
      if (x_format == "currency") {
        base_label <- paste0("Base case\n", format(base_value, big.mark = ","), " SEK")
      } else {
        base_label <- paste0("Base case\n", base_value)
      }
      p <- p +
        annotate("point", x = base_value, y = base_icer, color = "#4D4D4D", size = 3, shape = 17) +
        annotate("label", x = base_value, y = base_icer,
                 label = base_label,
                 hjust = 1.1, vjust = 0.5, size = 3.5, fill = "white")
    }
  }

  p
}
