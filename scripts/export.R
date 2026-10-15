export_xlsx <- function(named_sheets, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write_xlsx(named_sheets, path = path)
}

save_plot_png <- function(p, path, width = 8, height = 6, dpi = 300) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename = path, plot = p, width = width, height = height, dpi = dpi)
}

# Submission-quality figure export.
# Writes a 600 dpi PNG (the journal's minimum for raster line art) and, when a
# usable vector device is available, a vector file (the journal's preference for
# charts and diagrams).
#
# Device notes: the base pdf() device cannot render the "greater than or equal
# to" glyph used in the figure labels -- it silently substitutes dots -- so it is
# never used here. cairo_pdf() renders it correctly but fails to load in some
# headless sessions; svglite is used as a fallback when installed.
save_figure <- function(p, basename, width = 8, height = 6, dpi = 600,
                        dir = "outputs/figures") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  png_path <- file.path(dir, paste0(basename, ".png"))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(png_path, p, width = width, height = height, dpi = dpi,
           bg = "white", device = ragg::agg_png)
  } else {
    ggsave(png_path, p, width = width, height = height, dpi = dpi, bg = "white")
  }
  message("Saved: ", png_path, " (", dpi, " dpi)")

  vec_path <- NA_character_

  # 1st choice: cairo_pdf (vector, embeds fonts, handles the >= glyph)
  if (isTRUE(capabilities("cairo"))) {
    candidate <- file.path(dir, paste0(basename, ".pdf"))
    ok <- tryCatch({
      suppressWarnings(
        ggsave(candidate, p, width = width, height = height,
               device = grDevices::cairo_pdf, bg = "white")
      )
      file.exists(candidate)
    }, error = function(e) FALSE)
    if (ok) vec_path <- candidate
  }

  # 2nd choice: svglite
  if (is.na(vec_path) && requireNamespace("svglite", quietly = TRUE)) {
    candidate <- file.path(dir, paste0(basename, ".svg"))
    ok <- tryCatch({
      ggsave(candidate, p, width = width, height = height,
             device = svglite::svglite, bg = "white")
      file.exists(candidate)
    }, error = function(e) FALSE)
    if (ok) vec_path <- candidate
  }

  if (is.na(vec_path)) {
    message("  No vector file written (cairo unavailable and svglite not installed). ",
            "The 600 dpi PNG meets the journal minimum; install svglite for vector output.")
  } else {
    message("Saved: ", vec_path)
  }

  invisible(c(png = png_path, vector = vec_path))
}
