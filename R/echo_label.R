#' echo label knitr hook
#'
#' @export
echo_label <- function(before, options, envir,level="###") {
  if ( before ) {
    sprintf('%s %s',level, options$label )
  }
}
