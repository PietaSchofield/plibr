#' review a book chapter
#'
#'
#' @export
review_chapter <- function(chap, fmt = "bookdown::gitbook", root = NULL) {
  # find the book root (directory containing index.Rmd) unless supplied
  if (is.null(root)) {
    root <- rprojroot::find_root(rprojroot::has_file("index.Rmd"))
  }
  stopifnot(file.exists(file.path(root, "index.Rmd")),
            file.exists(file.path(root, chap)))

  cfg <- list(
    book_filename = "mybook",
    rmd_files = c("index.Rmd", chap),
    clean = FALSE,
    delete_merged_file = TRUE
  )

  cfg_file <- tempfile("bookdown-preview-", fileext = ".yml")
  yaml::write_yaml(cfg, cfg_file)
  on.exit(unlink(cfg_file), add = TRUE)

  withr::with_dir(root, {
    bookdown::render_book("index.Rmd",
                          output_format = fmt,
                          config_file = cfg_file,
                          output_dir = "_book")
    browseURL(file.path(root, "_book", "index.html"))
  })
}
