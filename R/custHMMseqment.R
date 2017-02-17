#' Custom CNV segmentation
#'
#' uses HMMcopy HMMsegment function but applys a custom segmentation mu string
#'
#' @param cn corrected copy number RangedData object
#' @param segmentationMu comma separated string of values
#' @param outFile if specified writes the segmentation out as a wig file
#' @param long if true performs adjustment to favour long segments
#'
#' @export
custHMMsegment <- function(cn,pl,outFile=NULL){
  ratioPosition <- log2(as.numeric(unlist(strsplit(pl[["segMu"]],",", fixed=TRUE)))/2)
  set.seed(123) # To ensure results are reproducible
  # Retrieve default converged parameters obtained via EM
  param <- HMMcopy::HMMsegment(cn, getparam=TRUE)
  # This is good for analysis when matched control is not available
  param$m <- ratioPosition
  # Decrease the number of segments, i.e. prefer longer segments
  if(pl[["hiFlex"]]){
    param$e <- pl[["evalue"]]
    param$strength <- pl[["strength"]]
    param$eta <- pl[["eta"]] 
  }else{
    param$mu <- param$m
  }
  # Perform segmentation via Viterbi algorithm
  hmmsegs <- HMMcopy::HMMsegment(cn, param)
  segs <- hmmsegs$segs 
  if(!is.null(outFile)){
    gr <- as(segs,"GRanges")
    gr$score <- as.numeric(gr$state)
    gr$score[is.na(gr$score)] <- 0
    suppressWarnings(seqlengths(gr) <- unname(pl[["seqLen"]][seqinfo(gr)@seqnames]))
    rtracklayer::export(gr,outFile,"BigWig")
  }
  hmmsegs 
}
