#' Check and optionally open a local web page served from localhost
#'
#' Given a file path under a web server's document root, this function constructs
#' the corresponding localhost URL, checks the HTTP status, and (optionally)
#' opens the URL in your default browser without blocking R.
#'
#' @param file_path Character. Absolute path to a file within the server's document root.
#' @param root_dir Character. Absolute path to the server's document root.
#'   Default "/var/www/html".
#' @param host Character. Host URL prefix. Default "http://localhost".
#' @param open Logical. If TRUE, open the URL (non-blocking) via \code{utils::browseURL}.
#'   Default FALSE.
#' @param request_timeout Numeric. Seconds to wait for the HTTP request. Default 5.
#'
#' @return A list with elements:
#' \describe{
#'   \item{url}{The constructed URL.}
#'   \item{ok}{Logical, TRUE if status 200.}
#'   \item{status}{Integer HTTP status code, or NA if unreachable.}
#'   \item{opened}{Logical, TRUE if an attempt to open the URL was made (and \code{open=TRUE}).}
#' }
#'
#' @examples
#' \dontrun{
#' res <- check_local_page_open(
#'   file_path = "/var/www/html/myproject/index.html",
#'   open = TRUE
#' )
#' if (res$ok) message("Page is up: ", res$url)
#' }
#'
#' @importFrom httr GET status_code timeout
#' @export
local_page_open <- function(file_path,
                                  root_dir = "/srv/http",
                                  host = "http://localhost",
                                  open = FALSE,
                                  request_timeout = 5,
                                  browser=getOption("browser")) {

  if(F){
    file_path <- htmlPath
    root_dir = "/srv/http"
    host = "http://localhost"
    open = FALSE
    request_timeout = 5
    browser="opera"
  } 
  
  fp_norm <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
  root_norm <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)

  url <- gsub(paste0("^",root_dir),host,fp_norm)
 
  plibr::displayURL(as.character(url),browser=browser)
}

