#' Load data from OfficeTime export into my time recording DB
#
#' Takes a file name and squirts the info into the time recording database. It will check if
#' data already exists for the dates and stop rather than write duplicate entries.
#'
#' @param fileName name of officetime output file
#' @param projDir default directory of the project
#' @param dbName default name of the timesheet sqlite database
#'
#' @export
logTime <- function(fileName=NULL, projDir="/Users/pschofield/Projects/time_sheet/",
                    dbName="timeSheet.db"){
  # open the database
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=paste0(projDir,dbName))

  # convert the officetime UTF-16 to a UTF-8
  outFile <- paste0(projDir,format(Sys.time(),"%Y_%m_%d"),".txt")
  uploads <- as.data.frame(cbind(date=format(Sys.time(),"%Y_%m_%d"),
                                 infile=fileName,outFile=outFile))
  system(paste0("iconv -f UTF-16 -t UTF-8 < ",fileName," > ",outFile),intern=T)

  # read the data file
  dat <- read.delim(outFile,head=T,stringsAsFactors=F)[,c(-1,-2)]

  # manipulate the dates
  End <- lubridate::dmy_hm(dat$End.Time)
  Start <- lubridate::dmy_hm(dat$Start.Time)
  dat$time <- End-Start
  dat$End.Time <- lubridate::stamp("2000-2-23 12:30",quiet=T)(End)
  dat$Start.Time <- lubridate::stamp("2000-2-23 12:30",quiet=T)(Start)
  dat$Duration <- dat$Duration*60

  # cleanup the notes
  dat$Notes[which(is.na(dat$Notes))] <- ""

  # split out the PI and devision
  dat$Cost <- sapply(dat$Category,function(x) tail(strsplit(x,",")[[1]],1))
  dat$Category <- sapply(dat$Category,function(x) head(strsplit(x,",")[[1]],1))
  colnames(dat) <- c("Project","Start","End","Duration","PI","Division","Notes","Time")

  dat$Pno <- sapply(dat$Project,function(x) gsub("[] ]","",tail(strsplit(x,"[[]")[[1]],1)))
  dat$Project <- sapply(dat$Project,function(x) head(strsplit(x,"[[]")[[1]],1))
  if(!("sessions"%in%RSQLite::dbListTables(db))){
    RSQLite::dbWriteTable(db,"sessions",dat)
  }else{
    if("tmpTable" %in% RSQLite::dbListTables(db))
      RSQLite::dbSendQuery(db,"drop table tmpTable")
    RSQLite::dbWriteTable(db,"tmpTable",dat)
    RSQLite::dbSendQuery(db,paste0("insert into sessions select * from tmpTable"))
  }
  if(!("uploads"%in%RSQLite::dbListTables(db))){
    RSQLite::dbWriteTable(db,"uploads",uploads)
  }else{
    if("tmpTable" %in% RSQLite::dbListTables(db))
      RSQLite::dbSendQuery(db,"drop table tmpTable")
    RSQLite::dbWriteTable(db,"tmpTable",uploads)
    RSQLite::dbSendQuery(db,paste0("insert into uploads select * from tmpTable"))
  }
  RSQLite::dbDisconnect(db)
}
