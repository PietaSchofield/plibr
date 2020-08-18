#' open a database using the credentials file
#'
#' @export
openDB <- function(db,credir=file.path(Sys.getenv("HOME"),".config"),
                   credfile=file.path(credir,"psqlcred.yml")){
  pg <- config::get(file=credfile)
  cd <- pg[[db]]
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
}
