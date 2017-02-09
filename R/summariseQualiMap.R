#' gather alignment statistic on a directory of star alignments
#'
#' @param projName project names
#' @param fileList list of log files
#' @param locDir location for (temporary) local storage of log files
#' 
#' @export
summariseQualimap <- function(fileList,projName, locDir="/Users/pschofield/.tmp"){
  ret <- lapply(fileList,function(fn){
    tmpFile <- plib::getFiles(filenames=basename(fn),
                                 remDir=gsub("/scratch/","",dirname(fn)),
                                 locDir=gsub("/Users/","",locDir),force=T)
    m <- read.delim(tmpFile,head=TRUE,stringsAsFactors=F)
    colnames(m) <- c("position","coverage")
    median(m$coverage)
  })
  names(ret) <- basename(dirname(dirname(fileList)))
  ret <- plyr::ldply(ret)
  colnames(ret) <- c("sample","medianCoverage") 
  ret
}
