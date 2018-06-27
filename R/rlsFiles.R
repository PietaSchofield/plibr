#' list remote directory
#'
#' @param pattern file pattern
#'
#' @export
rlsFiles <- function(pattern,hostname="feenix",verb=F){
  if(verb){
    cmd <- "ls -lah --time-style=long-iso "
  }else{
    cmd <- "ls "
  }
  plib::rcmd(paste0(cmd,pattern),hostname=hostname)
}
