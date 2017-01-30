#' collate a directory of STAR .ReadsPerGene.out.tab files
#'
#' @param dirname directory name
#'
#' @export
collateRPG <- function(directoryName,locDir=NULL,strandedness="US",force=F){
  files <- rlsFiles(file.path(directoryName,"*_ReadsPerGene.out.tab"))
  countList <- lapply(files,function(fn){
    tmpFile <- plib::getFiles(filenames=basename(fn),
                                 remDir=gsub("/scratch/","",dirname(fn)),
                                 locDir=gsub("/Users/","",locDir),force=force)
    tab <- read.delim(tmpFile,sep="\t",head=F,row.names=1)
    colnames(tab) <- c("US","FS","SS")
    ret <- tab[,strandedness]
    names(ret) <- rownames(tab)
    ret
  })
  names(countList) <- gsub("_ReadsPerGene.*$","",basename(files))
  retMat <- plyr::ldply(countList)
  rownames(retMat) <- retMat[,1]  
  t(retMat[,-1])
}

