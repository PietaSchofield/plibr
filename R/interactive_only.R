#' Run only in interactive sessions
#'
#' @param expr Code to evaluate (not quoted)
#' @return NULL invisibly
#' @export
interactive_only <- function(expr) {
  if (interactive()) {
    force(expr)
  } else {
    invisible(NULL)
  }
}

