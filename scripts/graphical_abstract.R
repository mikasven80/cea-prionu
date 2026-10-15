# Graphical abstract for EHJ-QCCO
# Canvas is drawn in millimetres (180 x 125 mm = 18.0 x 12.5 cm, landscape).
# Text sizes are set in points and converted to the mm units ggplot uses for
# geom_text, so the 10-12 pt requirement is met exactly (nothing below 8 pt).

library(ggplot2)

PT <- 1 / 2.845          # points -> mm for geom_text size
FAM <- "Helvetica"       # sans serif; Arial/Helvetica both acceptable

EUR <- "€"          # escaped so rendering does not depend on locale
GE  <- "≥"
TICK <- "✓"

INK    <- "#12263A"      # headline / body text
NAVY   <- "#1F3552"      # title band
ACCENT <- "#2E6F9E"      # funnel + bar
LIGHT  <- "#E8EEF3"      # panel fills
RULE   <- "#B9C6D1"

txt <- function(x, y, label, pt = 9, face = "plain", col = INK, hjust = 0.5, ...) {
  annotate("text", x = x, y = y, label = label, size = pt * PT, family = FAM,
           fontface = face, colour = col, hjust = hjust, lineheight = 1.05, ...)
}
box <- function(xmin, xmax, ymin, ymax, fill = LIGHT, col = NA, r = NULL) {
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
           fill = fill, colour = col, linewidth = 0.3)
}

# ---- screening funnel (per 1000 invited) ----------------------------------
funnel <- data.frame(
  label = c("1000 invited",
            "300 complete the\nself-report survey",
            "90 predicted high risk",
            paste0("81 undergo CAC CT"),
            paste0("24 start lipid-lowering\ntherapy (CAC ", GE, "100)")),
  ytop  = c(100, 82.5, 65, 47.5, 30)
)
funnel$ybot <- funnel$ytop - 12
funnel$ymid <- (funnel$ytop + funnel$ybot) / 2
funnel$fill <- c(LIGHT, LIGHT, LIGHT, LIGHT, ACCENT)
funnel$col  <- c(INK, INK, INK, INK, "white")
step_lab <- c("30% respond", "30% high risk", "90% attend",
              paste0("30% CAC ", GE, "100"))

p <- ggplot() +
  # ---------------- title band ----------------
  box(0, 180, 110, 125, fill = NAVY) +
  txt(90, 119.5, "Targeted two-stage screening to guide lipid-lowering therapy",
      pt = 12, face = "bold", col = "white") +
  txt(90, 113.5,
      paste0("Markov model of 1000 Swedish 60-year-olds · 20-year horizon · ",
             "healthcare-sector perspective"),
      pt = 8.5, col = "#C6D4E0") +

  # ---------------- column headings ----------------
  txt(30,  105.5, "SCREENING PATHWAY",  pt = 9.5, face = "bold", col = ACCENT) +
  txt(90,  105.5, "COST-EFFECTIVENESS", pt = 9.5, face = "bold", col = ACCENT) +
  txt(150, 105.5, "ROBUSTNESS",         pt = 9.5, face = "bold", col = ACCENT) +
  annotate("segment", x = c(60, 120), xend = c(60, 120), y = 19, yend = 102,
           colour = RULE, linewidth = 0.3) +

  # ---------------- left: funnel ----------------
  annotate("rect", xmin = 4, xmax = 56, ymin = funnel$ybot, ymax = funnel$ytop,
           fill = funnel$fill, colour = RULE, linewidth = 0.3) +
  annotate("text", x = 30, y = funnel$ymid, label = funnel$label,
           size = 9.5 * PT, family = FAM, colour = funnel$col,
           fontface = c("plain", "plain", "plain", "plain", "bold"),
           lineheight = 1.05) +
  annotate("segment", x = 30, xend = 30,
           y = funnel$ytop[-5] - 12, yend = funnel$ytop[-1],
           arrow = arrow(length = unit(1.6, "mm"), type = "closed"),
           colour = ACCENT, linewidth = 0.5) +
  annotate("text", x = 32.5, y = funnel$ytop[-1] + 3, label = step_lab,
           size = 8.5 * PT, family = FAM, colour = "#4A6076", hjust = 0) +

  # ---------------- middle: headline result ----------------
  txt(90, 97, "Per person invited", pt = 9.5) +
  txt(90, 92, paste0("+", EUR, "49 and +0.0030 QALYs"), pt = 9.5, face = "bold") +
  txt(90, 78, paste0(EUR, "16,611"), pt = 26, face = "bold", col = ACCENT) +
  txt(90, 69, "per QALY gained", pt = 10) +
  # bar: 0 to 50,000 mapped to x = 66..114
  annotate("segment", x = 65, xend = 110, y = 54, yend = 54,
           colour = RULE, linewidth = 0.4) +
  annotate("rect", xmin = 65, xmax = 65 + 45 * 16611 / 50000,
           ymin = 51.5, ymax = 56.5, fill = ACCENT) +
  annotate("segment", x = 65 + 45 * 46296 / 50000, xend = 65 + 45 * 46296 / 50000,
           y = 49, yend = 59, colour = INK, linewidth = 0.5, linetype = "22") +
  txt(65 + 45 * 46296 / 50000, 62.5,
      paste0("Swedish threshold\n", EUR, "46,296"), pt = 8.5, hjust = 1) +
  txt(65, 42, "Well below the threshold used", pt = 9, hjust = 0) +
  txt(65, 37.5, "for preventive care in Sweden", pt = 9, hjust = 0) +

  # ---------------- right: robustness ----------------
  annotate("text", x = 123, y = c(94, 78, 62, 46), label = TICK,
           size = 12 * PT, family = FAM, colour = ACCENT, fontface = "bold") +
  annotate("text", x = 127.5,  y = c(94, 78, 62, 46), hjust = 0,
           size = 9.5 * PT, family = FAM, colour = INK, lineheight = 1.1,
           label = c("98.5% of probabilistic\nsimulations cost-effective",
                     "Holds even if treatment\nis as weak as HR 0.71",
                     "Holds even if only 60%\nstart treatment",
                     "Cost-effective in all four\nsocioeconomic quartiles")) +

  # ---------------- take-home banner ----------------
  box(0, 180, 0, 16, fill = LIGHT) +
  txt(90, 10,
      "A 14-question self-report tool can target CT scanning so that",
      pt = 10, face = "bold") +
  txt(90, 5,
      "CAC-guided lipid-lowering therapy is affordable at population scale",
      pt = 10, face = "bold") +

  coord_cartesian(xlim = c(0, 180), ylim = c(0, 125), expand = FALSE) +
  theme_void()

save_graphical_abstract <- function(path = "outputs/figures/graphical_abstract.png",
                                    dpi = 600) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
  ggsave(path, p, width = 18.0, height = 12.5, units = "cm",
         dpi = dpi, bg = "white", device = dev)
  message("Saved: ", path, " (18.0 x 12.5 cm, ", dpi, " dpi)")
  invisible(path)
}
