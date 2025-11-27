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
#'
#' @export
save_fig <- function(plot,
                     name,
                     width  = 6,
                     height = 4,
                     dpi    = 300,
                     dir    = "figures",
                     formats = c("svg", "png")) {

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  files <- character()

  for (ext in formats) {
    file <- file.path(dir, paste0(name, ".", ext))

    if (ext == "svg") {
      # use svglite device via ggsave
      ggsave(
        filename = file,
        plot     = plot,
        width    = width,
        height   = height,
        device   = svglite::svglite
      )
    } else {
      ggsave(
        filename = file,
        plot     = plot,
        width    = width,
        height   = height,
        dpi      = dpi
      )
    }

    files <- c(files, file)
  }

  message("Saved: ", paste(files, collapse = ", "))
  invisible(files)
}

