#' Convert a DOCX file to Markdown
#'
#' Wraps \code{rmarkdown::pandoc_convert()} to convert a Word (.docx) document
#' into a Markdown (.md) file, optionally extracting embedded images/media
#' into a companion folder. Uses the pandoc binary bundled with RStudio/
#' \pkg{rmarkdown}, so no separate pandoc installation is required in that
#' context.
#'
#' @param docx_path Character. Path to the input \code{.docx} file.
#' @param out_path Character or \code{NULL}. Path to the output \code{.md}
#'   file. If \code{NULL} (default), it is derived from \code{docx_path} by
#'   replacing the \code{.docx} extension with \code{.md}.
#' @param extract_media Logical. If \code{TRUE} (default), embedded images
#'   and other media are extracted into a folder named
#'   \code{"<output_basename>_media"} alongside the output file.
#' @param overwrite Logical. If \code{FALSE} (default) and \code{out_path}
#'   already exists, the function errors rather than silently overwriting.
#'
#' @return The path to the created Markdown file, returned invisibly.
#'
#' @examples
#' \dontrun{
#' docx_to_md("meeting_notes.docx")
#' docx_to_md("meeting_notes.docx", "clean_version.md", extract_media = FALSE)
#' }
#'
#' @seealso \code{\link[rmarkdown]{pandoc_convert}}
#' @export
docx_to_md <- function(docx_path,
                        out_path = NULL,
                        extract_media = TRUE,
                        overwrite = FALSE) {

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

  if (is.null(out_path)) {
    out_path <- sub("\\.docx$", ".md", docx_path, ignore.case = TRUE)
  }

  if (file.exists(out_path) && !overwrite) {
    stop("Output file already exists: ", out_path,
         "\nSet `overwrite = TRUE` to replace it.", call. = FALSE)
  }

  args <- character(0)
  if (extract_media) {
    media_dir <- sub("\\.md$", "_media", out_path)
    args <- c(args, paste0("--extract-media=", media_dir))
  }

  rmarkdown::pandoc_convert(
    input   = docx_path,
    to      = "markdown",
    output  = out_path,
    options = args
  )

  message("Converted: ", docx_path, " -> ", out_path)

  invisible(out_path)
}
