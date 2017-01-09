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
custHMMsegment <- function(cn,segmentationMu,outFile=NULL,long=FALSE,seqlen){
  ratioPosition <- log2(as.numeric(unlist(strsplit(segmentationMu,",", fixed=TRUE)))/2)
  set.seed(123) # To ensure results are reproducible
  # Retrieve default converged parameters obtained via EM
  param <- HMMcopy::HMMsegment(cn, getparam=TRUE)
  # This is good for analysis when matched control is not available
  param$mu <- ratioPosition
  param$m <- param$mu
  # Decrease the number of segments, i.e. prefer longer segments
  if(long){
    param$e <- 0.9999999999999
    param$strength <- 1e8
  }
  # Perform segmentation via Viterbi algorithm
  hmmsegs <- HMMcopy::HMMsegment(cn, param)
  segs <- hmmsegs$segs 
  gr <- as(segs,"GRanges")
  gr$score <- as.numeric(gr$state)
  gr$score[is.na(gr$score)] <- 0
  seqlengths(gr) <- seqlen[names(seqlengths(gr))]
  if(!is.null(outFile)){
    rtracklayer::export(gr,outFile,"BigWig")
  }
  hmmsegs 
}
