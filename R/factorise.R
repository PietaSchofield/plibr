#' find divisors of an integer
#'
#' @export
factorise <- function(x) {
  x <- as.integer(x)
  y <- seq_len(abs(x))
  y[x %% y == 0L]
}
