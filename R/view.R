#' simple wrapper to open a file at the commandline on a mac
#'
#' very simple command possibly expand to take application as an argument
#'
#' @param fileName
#'
#' @export
view <- function(fileName){
  system(paste0("open ",fileName))
}
