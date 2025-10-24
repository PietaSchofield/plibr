#' Open a document file in the default system viewer
#'
#' Opens a file (for example, a Word `.docx` file) in the system's
#' default application for that file type.  
#' On Windows this uses `shell.exec()`, on macOS it uses the `open`
#' command, and on Linux it uses `xdg-open`.
#'
#' @param file Character string giving the path to the file to open.
#'   Can be relative or absolute.
#' @param wait Logical; if `TRUE`, R waits for the viewer to close before continuing.
#'   Defaults to `FALSE` so R does not block.
#'
#' @return Invisibly returns the system status code.
#'
#' @examples
#' \dontrun{
#' open_docx("paper.docx")
#' }
#'
#' @export
open_docx <- function(file, wait = FALSE) {
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }

  sysname <- Sys.info()[["sysname"]]

  if (.Platform$OS.type == "windows") {
    shell.exec(file)
  } else if (identical(sysname, "Darwin")) {
    system2("open", file, wait = wait)
  } else {
    # assume Linux/Unix
    system2("xdg-open", file, wait = wait)
  }

  invisible(0L)
}
