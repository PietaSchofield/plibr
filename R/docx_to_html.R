#' Convert a DOCX file to HTML
#'
#' @inheritParams docx_to_md
#' @param css_path Character or NULL. Path to a CSS file to apply.
#' @param embed Logical. Embed CSS/media into a single portable HTML file.
#' @param title Character or NULL. Document title for the HTML <title> tag.
#'   If NULL (default), derived from the output filename (no extension).
#' @export
docx_to_html <- function(docx_path,
                          out_path = NULL,
                          extract_media = TRUE,
                          overwrite = FALSE,
                          css_path = getOption("docx_to_html.css"),
                          embed = TRUE,
                          title = NULL) {

  if (!is.character(docx_path) || length(docx_path) != 1) {
    stop("`docx_path` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(docx_path)) {
    stop("File not found: ", docx_path, call. = FALSE)
  }
  if (!grepl("\\.docx$", docx_path, ignore.case = TRUE)) {
    warning("`docx_path` does not have a .docx extension: ", docx_path,
             call. = FALSE)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required but not installed.", call. = FALSE)
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc is not available. Install it, or run this from within ",
         "RStudio, which bundles its own copy.", call. = FALSE)
  }
  if (!is.null(css_path) && !file.exists(css_path)) {
    stop("CSS file not found: ", css_path, call. = FALSE)
  }

  if (is.null(out_path)) {
    out_path <- sub("\\.docx$", ".html", docx_path, ignore.case = TRUE)
  }
  if (file.exists(out_path) && !overwrite) {
    stop("Output file already exists: ", out_path,
         "\nSet `overwrite = TRUE` to replace it.", call. = FALSE)
  }

  if (is.null(title)) {
    title <- tools::file_path_sans_ext(basename(out_path))
  }

  args <- c("--standalone", paste0("--metadata=title:", title))

  if (extract_media) {
    media_dir <- sub("\\.html$", "_media", out_path)
    args <- c(args, paste0("--extract-media=", media_dir))
  }

  if (!is.null(css_path)) {
    args <- c(args, paste0("--css=", css_path))
  }

  if (embed) {
    args <- c(args, "--embed-resources")
  }

  rmarkdown::pandoc_convert(
    input   = docx_path,
    to      = "html",
    output  = out_path,
    options = args
  )

  message("Converted: ", docx_path, " -> ", out_path)
  invisible(out_path)
}
