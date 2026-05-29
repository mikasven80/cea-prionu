# scripts/plot_model_structure.R
# Figure: (A) Screening decision tree  (B) Markov state-transition model
# Dependencies: ggplot2, patchwork

library(ggplot2)

# ── Helpers ──────────────────────────────────────────────────────────

make_ellipse <- function(cx, cy, a, b, n = 100) {
  t <- seq(0, 2 * pi, length.out = n + 1)
  data.frame(x = cx + a * cos(t), y = cy + b * sin(t))
}

ellipse_border <- function(cx, cy, a, b, tx, ty) {
  alpha <- atan2(ty - cy, tx - cx)
  r <- 1 / sqrt((cos(alpha) / a)^2 + (sin(alpha) / b)^2)
  c(cx + r * cos(alpha), cy + r * sin(alpha))
}

make_triangles <- function(positions, s = 0.010) {
  do.call(rbind, lapply(seq_len(nrow(positions)), function(i) {
    tx <- positions$x[i]; ty <- positions$y[i]
    data.frame(
      x  = c(tx, tx + s * 1.8, tx),
      y  = c(ty - s, ty, ty + s),
      id = i
    )
  }))
}

# ── Panel A: Screening Decision Tree ────────────────────────────────

plot_decision_tree <- function() {

  # Terminal y-positions (top to bottom)
  # No-show for CT is now a terminal (no CAC split)
  yt <- c(init     = 0.97, reject   = 0.93,
          cac_lt_s = 0.86, noshow   = 0.76,
          nothr    = 0.62, notest   = 0.50,
          hr_ni    = 0.18, nothr_ni = 0.06)

  # Internal y-positions (midpoint of children)
  y_cac_gt  <- mean(yt[c("init", "reject")])
  y_shows   <- mean(c(y_cac_gt, yt["cac_lt_s"]))
  y_hr      <- mean(c(y_shows, yt["noshow"]))
  y_conduct <- mean(c(y_hr, yt["nothr"]))
  y_int     <- mean(c(y_conduct, yt["notest"]))
  y_noint   <- mean(yt[c("hr_ni", "nothr_ni")])
  y_pop     <- mean(c(y_int, y_noint))

  # X-positions (fork / chance-node levels)
  x0 <- 0.05; x1 <- 0.18; x2 <- 0.32; x3 <- 0.46
  x4 <- 0.60; x5 <- 0.76; xt <- 0.97

  # ── Segments ──
  s <- function(a, b, c, d) data.frame(x = a, y = b, xend = c, yend = d)

  segs <- do.call(rbind, list(
    # Population fork
    s(x0, y_int,           x0, y_noint),
    s(x0, y_int,           x1, y_int),
    s(x0, y_noint,         x2, y_noint),
    # x1: Conducts test / Does not test
    s(x1, y_conduct,       x1, yt["notest"]),
    s(x1, y_conduct,       x2, y_conduct),
    s(x1, yt["notest"],    xt, yt["notest"]),
    # x2 (Int): High-risk / Not high-risk
    s(x2, y_hr,            x2, yt["nothr"]),
    s(x2, y_hr,            x3, y_hr),
    s(x2, yt["nothr"],     xt, yt["nothr"]),
    # x2 (No int): High-risk / Not high-risk
    s(x2, yt["hr_ni"],     x2, yt["nothr_ni"]),
    s(x2, yt["hr_ni"],     xt, yt["hr_ni"]),
    s(x2, yt["nothr_ni"],  xt, yt["nothr_ni"]),
    # x3: Shows up for CT / No-show for CT (terminal)
    s(x3, y_shows,         x3, yt["noshow"]),
    s(x3, y_shows,         x4, y_shows),
    s(x3, yt["noshow"],    xt, yt["noshow"]),
    # x4 (shows up): CAC > / <
    s(x4, y_cac_gt,        x4, yt["cac_lt_s"]),
    s(x4, y_cac_gt,        x5, y_cac_gt),
    s(x4, yt["cac_lt_s"],  xt, yt["cac_lt_s"]),
    # x5: Initiates / Rejects
    s(x5, yt["init"],      x5, yt["reject"]),
    s(x5, yt["init"],      xt, yt["init"]),
    s(x5, yt["reject"],    xt, yt["reject"])
  ))

  # ── Nodes ──
  chance_nodes <- data.frame(
    x = c(x1,    x2,        x2,      x3,   x4,      x5),
    y = c(y_int, y_conduct, y_noint, y_hr, y_shows, y_cac_gt)
  )

  # Split segments: terminal branches (ending at xt) get open arrowheads
  is_terminal <- abs(segs$xend - xt) < 1e-6
  segs_internal <- segs[!is_terminal, ]
  segs_terminal <- segs[is_terminal, ]

  # ── Branch labels ──
  off <- 0.016
  lbl <- data.frame(
    x = c(x0 + 0.02, x0 + 0.02,
          x1 + 0.02, x1 + 0.02,
          x2 + 0.02, x2 + 0.02,
          x3 + 0.02, x3 + 0.02,
          x4 + 0.02, x4 + 0.02,
          x5 + 0.02, x5 + 0.02,
          x2 + 0.02, x2 + 0.02),
    y = c(y_int + off,           y_noint - off,
          y_conduct + off,       yt["notest"] - off,
          y_hr + off,            yt["nothr"] - off,
          y_shows + off,         yt["noshow"] - off,
          y_cac_gt + off,        yt["cac_lt_s"] - off,
          yt["init"] + off,      yt["reject"] - off,
          yt["hr_ni"] + off,     yt["nothr_ni"] - off),
    label = c("Intervention",       "No intervention",
              "Conducts test",      "Does not test",
              "High-risk",          "Not high-risk",
              "Shows up for CT",    "No-show for CT",
              "CAC > threshold",    "CAC < threshold",
              "Initiates treatment","Rejects treatment",
              "High-risk",          "Not high-risk"),
    vjust = rep(c(0, 1), 7),
    stringsAsFactors = FALSE
  )

  arw_open <- arrow(length = unit(0.15, "cm"), type = "open")

  ggplot() +
    # Internal segments (no arrowhead)
    geom_segment(data = segs_internal,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = 0.4) +
    # Terminal segments (open arrowhead)
    geom_segment(data = segs_terminal,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = 0.4, arrow = arw_open) +
    geom_point(data = chance_nodes, aes(x = x, y = y),
               shape = 16, size = 2.5) +
    geom_point(aes(x = x0, y = y_pop), shape = 15, size = 3.5) +
    geom_text(data = lbl,
              aes(x = x, y = y, label = label, vjust = vjust),
              hjust = 0, size = 2.8) +
    annotate("text", x = x0 - 0.01, y = y_pop,
             label = "Population", hjust = 1, size = 3.5, fontface = "bold") +
    annotate("text", x = xt + 0.005, y = y_pop,
             label = "All branches enter\nMarkov model (Panel B)",
             hjust = 0, size = 3, fontface = "italic") +
    ggtitle("Panel A") +
    coord_cartesian(xlim = c(-0.08, 1.18), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

# ── Panel B: Markov State-Transition Model ──────────────────────────

plot_markov_model <- function() {

  # State positions
  states <- data.frame(
    label = c("Event-free", "MI", "Post-MI", "Death"),
    cx    = c(0.25, 0.75, 0.25, 0.75),
    cy    = c(0.70, 0.70, 0.30, 0.30),
    stringsAsFactors = FALSE
  )
  # Semi-axes tuned for ~1.5:1 visual aspect at 10" x 4.4" output
  a <- 0.12; b <- 0.14

  # Ellipse outlines
  ell_df <- do.call(rbind, lapply(seq_len(nrow(states)), function(i) {
    df <- make_ellipse(states$cx[i], states$cy[i], a, b)
    df$id <- states$label[i]
    df
  }))

  # Transition arrows (from -> to indices)
  pairs <- list(c(1, 2), c(1, 4), c(2, 3), c(2, 4), c(3, 4))
  arrows_df <- do.call(rbind, lapply(pairs, function(p) {
    from <- ellipse_border(states$cx[p[1]], states$cy[p[1]], a, b,
                           states$cx[p[2]], states$cy[p[2]])
    to   <- ellipse_border(states$cx[p[2]], states$cy[p[2]], a, b,
                           states$cx[p[1]], states$cy[p[1]])
    data.frame(x = from[1], y = from[2], xend = to[1], yend = to[2])
  }))

  arw  <- arrow(length = unit(0.25, "cm"), type = "closed")
  acol <- "black"

  ggplot() +
    # Ellipses
    geom_polygon(data = ell_df, aes(x = x, y = y, group = id),
                 fill = "white", color = "black", linewidth = 0.7) +
    # State labels
    geom_text(data = states, aes(x = cx, y = cy, label = label), size = 5.5) +
    # Transition arrows
    geom_segment(data = arrows_df,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arw, color = acol, linewidth = 0.8) +
    # Self-loop: Event-free (arc above)
    geom_curve(aes(x = 0.20, y = 0.84, xend = 0.30, yend = 0.84),
               curvature = -1.3, arrow = arw,
               color = acol, linewidth = 0.8) +
    # Self-loop: Post-MI (arc to left)
    geom_curve(aes(x = 0.13, y = 0.36, xend = 0.13, yend = 0.24),
               curvature = 1.3, arrow = arw,
               color = acol, linewidth = 0.8) +
    ggtitle("Panel B") +
    coord_cartesian(xlim = c(-0.05, 1.05), ylim = c(0.05, 0.98)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

# ── Combined Figure ─────────────────────────────────────────────────

plot_model_structure <- function(filename = "outputs/Figure_Model_Structure.png",
                                 width = 10, height = 11, dpi = 300) {
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Install patchwork: install.packages('patchwork')")

  library(patchwork)

  p_a <- plot_decision_tree()
  p_b <- plot_markov_model()

  combined <- p_a / p_b +
    plot_layout(heights = c(1.5, 1))

  ggsave(filename, combined, width = width, height = height, dpi = dpi,
         bg = "white")
  message("Saved: ", filename)
  invisible(combined)
}
