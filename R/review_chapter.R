#' review a book chapter
#'
#'
#' @export
review_chapter <- function(chap, fmt = "bookdown::gitbook") {
  cfg <- list(
    book_filename = "mybook",
    rmd_files = c("index.Rmd", chap),
    clean = FALSE
  )
  yaml::write_yaml(cfg, "_bookdown-preview.yml")
  on.exit(unlink("_bookdown-preview.yml"))
  bookdown::render_book("index.Rmd", output_format = fmt,
                        config_file = "_bookdown-preview.yml")
  browseURL(file.path("_book", "index.html"))
}
