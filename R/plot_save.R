#' Plot and optionally save a figure depending on knitting context
#'
#' This helper function plots a ggplot object and, if called during
#' R Markdown rendering, saves it automatically to a specified folder.
#'
#' @param plot A ggplot object to plot.
#' @param filename Optional base filename (without extension). Defaults to the chunk label if available.
#' @param outdir Directory to save plots into (created if missing). Default is "figures".
#' @param width Plot width in inches. Default is 12.
#' @param height Plot height in inches. Default is 8.
#' @param dpi Resolution for saved images. Default is 300.
#'
#' @return Invisibly returns the input plot.
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' plot_save(p)
#'
#' @export
plot_save <- function(plot,
                         filename = NULL,
                         outdir = "figures",
                         width = 12,
                         height = 8,
                         dpi = 300) {

  # detect if running inside knitr
  in_knit <- isTRUE(getOption("knitr.in.progress"))

  # determine filename
  if (is.null(filename)) {
    filename <- knitr::opts_current$get("label")
    if (is.null(filename)) filename <- "unnamed_plot"
  }

  # ensure output directory exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  # file path
  filepath <- file.path(outdir, paste0(filename, ".png"))

  if (in_knit) {
    ggplot2::ggsave(filepath, plot = plot, width = width, height = height, dpi = dpi)
  }

  print(plot)

  invisible(plot)
}
