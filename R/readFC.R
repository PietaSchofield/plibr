#' readFC read a featureCount File
#'
#' @param filename name of feature counts output file
#' @param exons boolean if features are exons and there will be duplicate gene ids 
#'
#' @export
readFC <- function(filename, exons=F,name=gsub("[.]bam$","",basename(filename))){
  dat <- read.delim(filename,head=T,skip=1,sep="\t")
  if(exons){
    rownames(dat) <- gsub(" ","",apply(dat[,c(1:4)],1,paste,collapse="_"))
  }else{
    rownames(dat) <- dat[,1]
  }
  annotation <- dat[,1:6]
  counts <- as.data.frame(dat[,7:ncol(dat)])
  colnames(counts) <- gsub("[.]bam$","",basename(colnames(dat)[7:ncol(dat)]))
  rownames(counts) <- rownames(dat)
  rownames(annotation) <- rownames(dat)
  if(file.exists(paste0(filename,".summary"))){
    sumDat <- read.delim(paste0(filename,".summary"), sep="\t",h=T)
    sumRet <- as.data.frame(sumDat[,-1])
    rownames(sumRet) <- sumDat$Status
    colnames(sumRet) <- gsub("[.]bam$","",basename(colnames(sumDat)[-1]))
  }else{
    sumDat <- NULL
  }
  list(counts=counts,annotation=annotation[,-1],summary=sumDat)
}

