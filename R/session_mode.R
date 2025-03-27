#' Detect rendering context
#'
#' @return One of: "interactive", "rmarkdown", "knitr", "batch"
#' @export
session_mode <- function() {
  if (interactive()) return("interactive")
  if (isTRUE(getOption("knitr.in.progress"))) return("knitr")
  if (requireNamespace("rmarkdown", quietly = TRUE) &&
      !is.null(rmarkdown::metadata)) return("rmarkdown")
  return("batch")
}

