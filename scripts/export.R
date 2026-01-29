export_xlsx <- function(named_sheets, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write_xlsx(named_sheets, path = path)
}

save_plot_png <- function(p, path, width = 8, height = 6, dpi = 300) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename = path, plot = p, width = width, height = height, dpi = dpi)
}
