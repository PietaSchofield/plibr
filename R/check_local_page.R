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
                                  root_dir = "/var/www/html",
                                  host = "http://localhost",
                                  open = FALSE,
                                  request_timeout = 5) {
  stopifnot(is.character(file_path), length(file_path) == 1L)
  stopifnot(is.character(root_dir), length(root_dir) == 1L)
  stopifnot(is.character(host), length(host) == 1L)

  # Normalise paths
  fp_norm <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
  root_norm <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)

  # Derive URL path relative to root
  rel_path <- sub(paste0("^", root_norm, "(?=/|$)"), "", fp_norm, perl = TRUE)
  if (!startsWith(rel_path, "/")) rel_path <- paste0("/", rel_path)

  # Construct URL (avoid double slashes)
  host <- sub("/+$", "", host)
  url <- paste0(host, rel_path)

  # Probe the URL
  status <- NA_integer_
  ok <- FALSE
  opened <- FALSE

  resp <- try(httr::GET(url, httr::timeout(request_timeout)), silent = TRUE)
  if (!inherits(resp, "try-error")) {
    status <- httr::status_code(resp)
    ok <- identical(status, 200L)
  }

  # Optionally open the URL without blocking
  if (isTRUE(open)) {
    # utils::browseURL is non-blocking and cross-platform
    utils::browseURL(url)
    opened <- TRUE
  }

  list(url = url, ok = ok, status = status, opened = opened)
}

