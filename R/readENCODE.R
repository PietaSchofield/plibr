#' readENCODE read broadPeak and narrowPeak formats and return a GRanges object
#'
#' @param filename filename to import
#'
#' @import GenomicRanges
#' @export
readENCODE <- function(filename){
  type <- tail(strsplit(basename(filename),"[.]")[[1]],1)
  pks <- NULL
  pks <- try(read.delim(filename,head=F,sep="\t",stringsAsFactor=F),silent = T)
  if(is(pks)=="try-error") return(NULL)
  if(type=="narrowPeak"){
    colnames(pks) <- c("seqnames","start","end","name","score","strand","signal","pValue",
                       "qValue","peak")
  }else if(type=="broadPeak"){
    colnames(pks) <- c("seqnames","start","end","name","score","strand","signal","pValue",
                       "qValue")
  }else{
    colnames(pks) <- c("seqnames","start","end","score")
    pks$strand <- "*"
  }
  pks$strand <- ifelse(!(pks$strand %in% c("+","-","*")),"*",pks$strand)
  as(pks,"GRanges")
}

