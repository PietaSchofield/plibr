#' Save a ggplot or base R plot to both SVG and PNG in a clean directory
#'
#' Saves a given plot object (ggplot or base R) as both SVG and high-resolution PNG,
#' to a specified directory. Keeps your project tidy for Git.
#'
#' @param plot A plot object (e.g. from ggplot2 or base graphics via `recordPlot()`).
#' @param name Base name (without extension) for the output files.
#' @param width Width in inches. Default is 6.
#' @param height Height in inches. Default is 4.
#' @param dpi Resolution for PNG. Default is 300.
#' @param dir Output directory. Default is "figures". Created if needed.
#'
#' @return Invisibly returns character vector of saved file paths.
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' save_fig(p, "scatter_plot")
#'
#' @export
save_fig <- function(plot, name, width = 6, height = 4, dpi = 300, dir = "figures") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  svg_file <- file.path(dir, paste0(name, ".svg"))
  png_file <- file.path(dir, paste0(name, ".png"))

  # SVG
  svglite::svglite(svg_file, width = width, height = height)
  on.exit(dev.off(), add = TRUE)
  print(plot)
  dev.off()

  # PNG
  png(png_file, width = width, height = height, units = "in", res = dpi)
  on.exit(dev.off(), add = TRUE)
  print(plot)
  dev.off()

  message("Saved: ", svg_file, " and ", png_file)
  invisible(c(svg_file, png_file))
}

