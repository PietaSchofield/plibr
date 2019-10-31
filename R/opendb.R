#' open a database using the credentials file
#'
#' @export
openDB <- function(cred){
  dbConnect(RPostgres::Postgres(),
    dbname=cred$database,
    user=cred$uid,
    password=cred$pwd,
    host=cred$server
 )
}
