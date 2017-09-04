#' list remote directory
#'
#' @param pattern file pattern
#'
#' @export
rlsFiles <- function(pattern,crick=F){
  plib::rcmd(paste0("ls ",pattern),crick=crick)
}
