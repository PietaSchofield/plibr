#' open a database using the credentials file
#'
#' @export
getcreds <- function(credir=file.path(Sys.getenv("HOME"),".config"),
                     credfile=file.path(credir,"psqlcred.yml"),
                     credname=NULL){
  pg <- config::get(file=credfile)
  pg[[credname]]
}
