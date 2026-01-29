plot_ce_plane <- function(psa_res, wtp = 500000, level = 0.95, n_ellipse = 200,
                          ell_lab_offset = 0.015) {
  
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
    " SEK per QALY)"
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
      y = "Incremental cost, SEK (Intervention − SoC)",
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

plot_ceac <- function(ceac_tbl) { 
  ggplot(ceac_tbl, aes(x = wtp, y = p_ce)) +
    geom_line() +
    ylim(0, 1) + 
    labs(
      x = "WTP threshold (SEK per QALY)",
      y = "Pr(Cost-effective)",
      title = "CEAC"
    ) + theme_bw()
}