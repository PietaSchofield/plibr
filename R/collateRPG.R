#' collate a directory of STAR .ReadsPerGene.out.tab files
#'
#' @param dirname directory name
#'
#' @export
collateRPG <- function(directoryName,projName,strandedness="US",force=F,loc=T){
  if(!loc){
    files <- rlsFiles(file.path(directoryName,"*ReadsPerGene.out.tab"))
  }else{
    files <- list.files(directoryName,pattern=".*ReadsPerGene.out.tab",full=T)
  }
  countList <- lapply(files,function(fn){
    if(!loc){
      tmpFile <- plib::getFiles(filenames=fn,projName=projName)
    }else{
      tmpFile <- fn
    }
    tab <- read.delim(tmpFile,sep="\t",head=F,row.names=1)
    colnames(tab) <- c("US","FS","SS")
    ret <- tab[,strandedness]
    names(ret) <- rownames(tab)
    ret
  })
  names(countList) <- gsub("ReadsPerGene.*$","",basename(files))
  retMat <- plyr::ldply(countList)
  rownames(retMat) <- retMat[,1]  
  t(retMat[,-1])
}

