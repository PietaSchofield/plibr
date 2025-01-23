#' display a web page nicely in chrome
#'
#' @export
displayURL <- function(urlpath="http://localhost", browser_path="/usr/bin/brave") {
  if(F){
    urlpath="http://localhost"
  }
  system2(browser_path,arg=urlpath,wait=F,stdout=NULL,stderr=NULL)
}

