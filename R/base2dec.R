#' Convert a bit string to a number 
#'
#' @param number 
#' @param base
#'
#' @export
base2dec = function(base_number, base = 2) {
  split_base = strsplit(as.character(base_number), split = "")
  return(sapply(split_base, function(x) sum(as.numeric(x) * base^(rev(seq_along(x) - 1)))))
}
