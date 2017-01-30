#' list remote directory
#'
#' @param pattern file pattern
#'
#' @export
rlsFiles <- function(pattern){
  plib::rcmd(paste0("ls ",pattern))
}
