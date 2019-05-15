#' getWeek return the data for the week comencing
#'
#' get the time_sheet data for a particular week
#'
#' @param start date
#'
#' @export
getWeek <- function(startDate,endDate,projDir="/Users/pschofield/Projects/time_sheet/",
                    dbName="timeSheet.db"){
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=paste0(projDir,dbName))
  ses <- RSQLite::dbGetQuery(db,paste0("select * from sessions where End > '",startDate,
                                       "' and End < '",endDate,"'"))
  plotDat <- reshape2::melt(reshape2::dcast(ses,Project~PI,value.var="Duration",sum))
  plotDat <- plotDat[which(plotDat$value>0),]
  plotDat$hours <- plotDat$value/60
  plotDat$percent <- round(100*plotDat$value/sum(plotDat$value))
  colnames(plotDat)[2:3] <- c("PI","minutes")
  plotDat
}
