plot_ce_plane <- function(psa_res, wtp = 500000, level = 0.95, n_ellipse = 200,
                          ell_lab_offset = 0.015, fx = 10.8, currency = "EUR") {

  # Convert incremental costs and WTP to display currency (model is in SEK)
  psa_res <- psa_res
  psa_res$inc_cost <- psa_res$inc_cost / fx
  wtp <- wtp / fx

  # Symmetric limits to show all quadrants
  x_max <- max(abs(psa_res$inc_qaly), na.rm = TRUE)
  y_max <- max(abs(psa_res$inc_cost), na.rm = TRUE)
  
  # Mean vector and covariance matrix of increments
  X  <- psa_res[, c("inc_qaly", "inc_cost")]
  mu <- colMeans(X, na.rm = TRUE)
  S  <- stats::cov(X, use = "complete.obs")
  
  # Chi-square radius for coverage (df=2)
  r2 <- stats::qchisq(level, df = 2)
  
  # Parametric ellipse points
  eig <- eigen(S)
  angles <- seq(0, 2*pi, length.out = n_ellipse)
  circle <- cbind(cos(angles), sin(angles))
  
  sqrtS <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0))) %*% t(eig$vectors)
  ell <- sweep((sqrt(r2) * circle) %*% sqrtS, 2, mu, FUN = "+")
  
  ellipse_df <- data.frame(
    inc_qaly = ell[, 1],
    inc_cost = ell[, 2]
  )
  
  # WTP line segment within visible y-range
  wtp_line_df <- data.frame(
    inc_qaly = c(-y_max / wtp,  y_max / wtp),
    inc_cost = c(-y_max,       y_max)
  )
  
  wtp_label <- paste0(
    "Cost-Effectiveness Threshold\n(",
    formatC(wtp, format = "f", big.mark = ",", digits = 0),
    " ", currency, " per QALY)"
  )
  
  # Ellipse label positioning (tune ell_lab_offset)
  idx_right <- which.max(ellipse_df$inc_qaly)
  x_ell_lab <- ellipse_df$inc_qaly[idx_right] + ell_lab_offset * x_max
  y_ell_lab <- ellipse_df$inc_cost[idx_right] + ell_lab_offset * y_max
  
  ggplot(psa_res, aes(x = inc_qaly, y = inc_cost)) +
    geom_point(alpha = 0.25, size = 0.8) +
    
    # 95% ellipse
    geom_path(
      data = ellipse_df,
      aes(x = inc_qaly, y = inc_cost),
      linewidth = 0.8
    ) +
    
    # Quadrant lines
    geom_hline(yintercept = 0, color = "grey50") +
    geom_vline(xintercept = 0, color = "grey50") +
    
    # Dashed WTP line (visible)
    geom_line(
      data = wtp_line_df,
      aes(x = inc_qaly, y = inc_cost),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.6,
      color = "black"
    ) +
    
    # Text on the WTP line (no box; and path hidden so dashed line remains)
    geom_textpath(
      data = wtp_line_df,
      aes(x = inc_qaly, y = inc_cost, label = wtp_label),
      inherit.aes = FALSE,
      hjust = 0.78,
      vjust = -0.8,
      size = 3.8,
      fontface = "bold",
      color = "black",
      linewidth = 0,
      linetype = 0
    ) +
    
    # Ellipse label
    annotate(
      "label",
      x = x_ell_lab, y = y_ell_lab,
      label = "95% CI",
      hjust = 0, vjust = 0,
      size = 3.6,
      fontface = "bold",
      fill = "white"
    ) +
    
    coord_cartesian(xlim = c(-x_max, x_max), ylim = c(-y_max, y_max)) +
    
    labs(
      x = "Incremental QALYs (Intervention − SoC)",
      y = paste0("Incremental cost, ", currency, " (Intervention − SoC)"),
      title = "Cost-effectiveness plane"
    ) +
    theme_bw()
}

compute_ceac <- function(psa_res, wtp_grid) { 
  tibble(wtp = wtp_grid) %>% 
    rowwise() %>%
    mutate(p_ce = mean(wtp * psa_res$inc_qaly - psa_res$inc_cost >
    0, na.rm = TRUE)) %>%
    ungroup()
} 

plot_ceac <- function(ceac_tbl, fx = 10.8, currency = "EUR") {
  ceac_tbl <- ceac_tbl %>% mutate(wtp = wtp / fx)
  ggplot(ceac_tbl, aes(x = wtp, y = p_ce)) +
    geom_line() +
    ylim(0, 1) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(
      x = paste0("WTP threshold (", currency, " per QALY)"),
      y = "Pr(Cost-effective)",
      title = "CEAC"
    ) + theme_bw()
}

plot_state_membership <- function(state_membership) {
  # Reshape to long format for plotting
  plot_data <- state_membership %>%
    pivot_longer(
      cols = -Cycle,
      names_to = c("state", "arm"),
      names_pattern = "(.+)_(SoC|Int)",
      values_to = "count"
    ) %>%
    mutate(
      arm = factor(arm, levels = c("SoC", "Int"), labels = c("Standard of Care", "Intervention")),
      state = factor(state, levels = c("Event_free", "MI", "Post_MI", "Dead"),
                     labels = c("Event-free", "MI", "Post-MI", "Dead"))
    )

  ggplot(plot_data, aes(x = Cycle, y = count, color = state, linetype = arm)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(
      values = c("Event-free" = "#2166AC", "MI" = "#D6604D", "Post-MI" = "#F4A582", "Dead" = "#4D4D4D"),
      name = "State"
    ) +
    scale_linetype_manual(
      values = c("Standard of Care" = "solid", "Intervention" = "dashed"),
      name = "Arm"
    ) +
    scale_x_continuous(breaks = seq(0, max(state_membership$Cycle), by = 5)) +
    labs(
      x = "Cycle (years)",
      y = "Number of persons",
      title = "State membership over time"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    guides(
      color = guide_legend(order = 1),
      linetype = guide_legend(order = 2)
    )
}

plot_state_membership_faceted <- function(state_membership) {
  # Reshape to long format for plotting
  plot_data <- state_membership %>%
    pivot_longer(
      cols = -Cycle,
      names_to = c("state", "arm"),
      names_pattern = "(.+)_(SoC|Int)",
      values_to = "count"
    ) %>%
    mutate(
      arm = factor(arm, levels = c("SoC", "Int"), labels = c("Standard of Care", "Intervention")),
      state = factor(state, levels = c("Event_free", "MI", "Post_MI", "Dead"),
                     labels = c("Event-free", "MI", "Post-MI", "Dead"))
    )

  ggplot(plot_data, aes(x = Cycle, y = count, color = state)) +
    geom_line(linewidth = 1) +
    facet_wrap(~ arm) +
    scale_color_manual(
      values = c("Event-free" = "#2166AC", "MI" = "#D6604D", "Post-MI" = "#F4A582", "Dead" = "#4D4D4D"),
      name = "State"
    ) +
    scale_x_continuous(breaks = seq(0, max(state_membership$Cycle), by = 5)) +
    labs(
      x = "Cycle (years)",
      y = "Number of persons",
      title = "State membership over time"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey90"),
      strip.text = element_text(face = "bold")
    )
}

plot_state_membership_diff <- function(state_membership) {
  # Calculate differences (Intervention - SoC)
  diff_data <- state_membership %>%
    transmute(
      Cycle,
      `Event-free` = Event_free_Int - Event_free_SoC,
      MI = MI_Int - MI_SoC,
      `Post-MI` = Post_MI_Int - Post_MI_SoC,
      Dead = Dead_Int - Dead_SoC
    ) %>%
    pivot_longer(
      cols = -Cycle,
      names_to = "state",
      values_to = "difference"
    ) %>%
    mutate(
      state = factor(state, levels = c("Event-free", "MI", "Post-MI", "Dead"))
    )

  ggplot(diff_data, aes(x = Cycle, y = difference, color = state)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = c("Event-free" = "#2166AC", "MI" = "#D6604D", "Post-MI" = "#F4A582", "Dead" = "#4D4D4D"),
      name = "State"
    ) +
    scale_x_continuous(breaks = seq(0, max(state_membership$Cycle), by = 5)) +
    labs(
      x = "Cycle (years)",
      y = "Difference in persons (Intervention - SoC)",
      title = "Difference in state membership over time"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
}