#' georgie_says
#'
#' Simple single number reporting
#' @export
georgie_says <- function(x, fmt){
  cat(sprintf(fmt, x))
}
