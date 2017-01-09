#' readFC read a featureCount File
#'
#' @param filename name of feature counts output file
#' @param exons boolean if features are exons and there will be duplicate gene ids 
#'
#' @export
readFC <- function(filename, exons=F){
  dat <- read.delim(filename,head=T,skip=1,sep="\t")
  if(exons){
    rownames(dat) <- gsub(" ","",apply(dat[,c(1:4)],1,paste,collapse="_"))
  }else{
    rownames(dat) <- dat[,1]
  }
  annotation <- dat[,1:6]
  counts <- dat[,7:ncol(dat)]
  colnames(counts) <- gsub("[.]bam$","",colnames(counts))
  rownames(counts) <- rownames(dat)
  rownames(annotation) <- rownames(dat)
  if(file.exists(paste0(filename,".summary"))){
    sumDat <- read.delim(paste0(filename,".summary"), sep="\t",h=T)
    rownames(sumDat) <- sumDat$Status
    sumDat <- sumDat[,-1]
    colnames(counts) <- gsub("[.]bam$","",colnames(counts))
    sumDat <- as.data.frame(sumDat)
  }else{
    sumDat <- NULL
  }
  list(counts=counts,annotation=annotation[,-1],summary=sumDat)
}

