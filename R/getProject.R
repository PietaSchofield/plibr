#' get information from project xlsx
#'
#' @param filename name of the project file
#' @param sumSheet name of summary sheet
#' @param fileSheet name of raw data files sheet
#' @param ssCol start column summary sheet
#' @param seCol end column summary sheet
#' @param ssRow start row summary sheet
#' @param fsCol start column file sheet
#' @param feCol end column file sheet
#' @param fsRow start row file sheet
#'
#' @export
getProject <- function(filename,sumSheet="Summary",fileSheet="Files",
                       ssCol=1,seCol=3,ssRow=1,fsCol=1,feCol=6,fsRow=1){
  wb <- xlsx::loadWorkbook(file=filename)
  shts <- getSheets(wb)
  sumData <- readColumns(shts[[sumSheet]],startColumn=ssCol,endColumn=seCol,startRow=ssRow,
                         stringsAsFactors=F)
  rownames(sumData) <- sumData[,1]
  sumData <- sumData[,-1]
  fileData <- readColumns(shts[[fileSheet]],startColumn=fsCol,endColumn=feCol,startRow=fsRow,
                          stringsAsFactors=F)
  list(summaryData=sumData,fileData=fileData)
}
