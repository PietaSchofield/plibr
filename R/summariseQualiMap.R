#' gather alignment statistic on a directory of star alignments
#'
#' @param projName project names
#' @param fileList list of log files
#' @param locDir location for (temporary) local storage of log files
#' 
#' @export
summariseQualimap <- function(fileList,projName){
  tmpFiles <- plib::getFiles(filenames=fileList,projName=projName,force=T)
  ret <- lapply(tmpFiles,function(fn){
    m <- read.delim(fn,head=TRUE,stringsAsFactors=F)
    colnames(m) <- c("position","coverage")
    median(m$coverage)
  })
  names(ret) <- basename(dirname(dirname(tmpFiles)))
  ret <- plyr::ldply(ret)
  colnames(ret) <- c("sample","medianCoverage") 
  ret
}
