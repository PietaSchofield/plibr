#' myip
#'
#' function to return the localhost's ip addresses
#' 
#' @param pattern pattern to search for in command output
#' @param command system command to produce ip configuration output
#' @param item name of first item to trim line to
#' @param linesep pattern between items on address line
  #' @param namesep pattern to split items into name and address
#'
#' @export
myip <- function(pattern="Bcast",command="ifconfig",item="addr",linesep="  ",namesep=":"){
  adline <- system(command, intern=T)[grep(pattern, system(command, intern = T))]
  adlist <- strsplit(gsub(paste0(".*",item),item,stringr::str_trim(adline,"both")),linesep)
  addf <- plyr::ldply(lapply(adlist,strsplit,namesep)[[1]])
  ret <- addf[,2]
  names(ret) <- addf[,1]
  ret
}
