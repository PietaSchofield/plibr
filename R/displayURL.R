#' display a web page nicely in chrome
#'
#' @export
displayURL <- function(urlpath=NULL, bp="browser_main") {
  if(F){
    urlpath=NULL
    bp="browser_main"
  }
  if (is.null(urlpath)) {
    urlpath <- shQuote('http://localhost/uol')
  }
  browser_path <- getOption(bp, "/usr/bin/librewolf") 
  arg=c(urlpath)
  system2(browser_path,arg=arg,wait=F,stdout=NULL,stderr=NULL)
}

