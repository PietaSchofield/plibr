#' replace every nth character in a string with a character
#'
#' @param stringin the original string
#' @param enth the length of the repeat
#' @param ch the replacement character
#'
#' @export
replace_every <- function(stringin,enth,ch){
  ret <- strsplit(stringin,"")[[1]]
  for(i in 1:length(ret)) if(i%%enth==0) ret[i] <- ch
  paste0(ret,collapse="")
}
