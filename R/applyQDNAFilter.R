#' Apply QDNAseq filter
#'
#' function to apply a filter to a HMMcopy RangedData object and return a filtered RangedData object
#'
#' @param rd rangeddata input object
#' @param filter a filter in the form of a granges object
#' @param outfile a file to write the output to a bigwig file for viewing in IGV
#'
#' @export 
applyQDNAFilter <- function(rd,filterGR=filter150kb,outfile=NULL){
  gr <- as(rd,"GRanges")
  seqlengths(gr) <- seqlengths(filterGR)[names(seqlengths(gr))]
  over <- queryHits(findOverlaps(gr,filterGR))
  grf <- gr[over, ]
  gr$copy[over] <- NA
  grf$score <- grf$copy
  grf$score[is.na(grf$score)] <- 0
  if(!is.null(outFile)){
    rtracklayer::export(grf,outFile,"BigWig")
  }
  as(gr,"RangedData")
}

