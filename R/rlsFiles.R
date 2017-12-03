#' list remote directory
#'
#' @param pattern file pattern
#'
#' @export
rlsFiles <- function(pattern,hostname="rds-ssh"){
  plib::rcmd(paste0("ls ",pattern),hostname=hostname)
}
