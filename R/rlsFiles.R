#' list remote directory
#'
#' @param pattern file pattern
#'
#' @export
rlsFiles <- function(pattern,hostname="pieta.me",username="pieta",verb=F,dir=F){
  if(verb){
    if(dir){
      cmd <- "ls -dlah --time-style=long-iso "
    }else{
      cmd <- "ls -lah --time-style=long-iso "
    }
  }else{
    if(dir){
      cmd <- "ls -d "
    }else{
      cmd <- "ls "
    }
  }
  plib::rcmd(paste0(cmd,pattern),hostname=hostname,username=username)
}
