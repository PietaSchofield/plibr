#' gather alignment statistic on a directory of star alignments
#'
#' @param projName project names
#' @param fileList list of log files
#' @param locDir location for (temporary) local storage of log files
#' 
#' @export
summariseSubread <- function(projName,fileList, locDir="/Users/pschofield/.tmp"){
  ret <- lapply(fileList,function(fn){
    tmpFile <- plib::getFiles(filenames=basename(fn),
                                 remDir=gsub("/scratch/","",dirname(fn)),
                                 locDir=gsub("/Users/","",locDir))
    res <- read.delim(file.path(locDir,basename(fn)),sep="\t",head=F,row.names=1,stringsAsFactors=F)
    rownames(res) <- gdata::trim(gsub("([:]|[(]|[)])","",rownames(res)))
    rownames(res) <- gsub("([ ]|[-])","_",rownames(res))
    t(res)
  })
  names(ret) <- gsub("[.]stats$","",basename(fileList))
  ret <- plyr::ldply(ret)
  rownames(ret) <- ret[,1]
  ret$percent_mapped <- ret$reads_mapped/srSummary$sequences
  ret$percent_multimapped <- ret$non_primary_alignment/srSummary$sequences
  ret[,-1]
}
