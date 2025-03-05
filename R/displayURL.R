#' display a web page nicely in chrome
#'
#' @export
displayURL <- function(urlpath=NULL, browser_path=NULL) {
  if (is.null(urlpath)) {
    urlpath <- "http://localhost/uol"
  }
  if (is.null(browser_path)) {
    browser_path <- getOption("browser_path", "/usr/bin/librewolf") # Default if not set
  }
  system2(browser_path,arg=urlpath,wait=F,stdout=NULL,stderr=NULL)
}

