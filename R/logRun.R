#' logRun 
#'
#' output details of the current node and R environment for summitted job 
#'
#' @export
logRun <- function(){
  sys <- plyr::ldply(Sys.info())
  colnames(sys) <- c("name","value")
  sys
}
