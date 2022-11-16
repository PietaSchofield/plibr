#' getWeek return the data for the week comencing
#'
#' get the time_sheet data for a particular week
#'
#' @param start date
#'
#' @export
delWeek <- function(startDate,projDir="/Users/pschofield/Projects/time_sheet/",
                    dbName="timeSheet.db"){
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=paste0(projDir,dbName))
  RSQLite::dbGetQuery(db,paste0("delete from sessions where End > '",startDate,"'"))
}
