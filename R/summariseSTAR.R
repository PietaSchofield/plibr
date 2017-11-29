#' gather alignment statistic on a directory of star alignments
#'
#' @param projName project names
#' @param fileList list of log files
#' @param locDir location for (temporary) local storage of log files
#' 
#' @export
summariseSTAR <- function(projName,fileList,localDir=T){
  ret <- lapply(fileList,function(fn){
    if(!localDir){
      tmpFile <- plib::getFiles(filenames=fn,projName=projName)
    }else{
      tmpFile <- fn
    }
    res <- read.delim(tmpFile,sep="\t",head=F,row.names=1,stringsAsFactors=F)
    rownames(res) <- gsub("[%]","Percent",gsub("[|]","",gdata::trim(rownames(res))))
    rownames(res) <- gdata::trim(gsub("[|]","",rownames(res)))
    rownames(res) <- gsub(" ","_",rownames(res))
    t(res)
  })
  names(ret) <- gsub("Log[.]final[.]out$","",basename(fileList))
  ret <- plyr::ldply(ret)
  rownames(ret) <- make.names(ret[,1],unique=T)
  ret[,-1]
}
