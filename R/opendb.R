#' open a database using the credentials file
#'
#' @param db the name of the database
#' @param credir the location of the credentials file
#' @param credfile the name of the credentials file if not standard
#'
#' @export
openDB <- function(db,credir=NULL, credfile="psqlcred.yml"){
  credFile <- file.path(credir,credfile)
  pg <- config::get(file=credFile)
  cd <- pg[[db]]
  if(!is.null(cd$database)){
    tryCatch({
      RPostgres::dbConnect(RPostgres::Postgres(),
        dbname=cd$database,
        host=cd$server,
        port=cd$port,
        user=cd$uid,
        password=cd$pwd
    )},
    error=function(e){
      return(F)
    })
  }else{
     tryCatch({
      RPostgres::dbConnect(RPostgres::Postgres(),
        host=cd$server,
        port=cd$port,
        user=cd$uid,
        password=cd$pwd
    )},
    error=function(e){
      return(F)
    })
  }
}
