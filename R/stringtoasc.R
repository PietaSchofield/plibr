#' convert a string to a number that can be used as a seed for random 
#' 
#' @export
string_to_seed <- function(stringin){
  rin <- strsplit(stringin,"")[[1]]
  ret <- 0
  for(i in 1:length(rin)) ret <- ret + gtools::asc(rin[1])
  ret 
}

