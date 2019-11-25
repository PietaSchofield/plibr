#' open a database using the credentials file
#'
#' @export
openDB <- function(db,credfile=file.path(getwd(),".psqlcred.yml")){
  pg <- config::get(file=credfile)
  cd <- pg[[db]]
  RPostgres::dbConnect(RPostgres::Postgres(),
    dbname=cd$database,
    host=cd$server,
    port=cd$port,
    user=cd$uid,
    password=cd$pwd
 )
}
