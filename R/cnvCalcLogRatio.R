#' Function for reading in wig file from a pair of samples
#'
#' @export 
cnvLogRatio <- function(tumourFile, normalFile, gcFile, mapFile, myChromosome=c(1:22,"X","Y")) {
  # Convert reads to IRanges object
  tumour_uncorrected_reads <- HMMcopy::wigsToRangedData(tumourFile, gcFile, mapFile)
  normal_uncorrected_reads <- HMMcopy::wigsToRangedData(normalFile, gcFile, mapFile)
  # Corrected reads by GC-content and mappbility
  tumour_corrected_copy <- HMMcopy::correctReadcount(tumour_uncorrected_reads, mappability=0.9, 
                        samplesize=100000)
  normal_corrected_copy <- HMMcopy::correctReadcount(normal_uncorrected_reads, mappability=0.9, 
                        samplesize=100000)
  # Normalise tumour read counts by normal read counts
  res <- tumour_corrected_copy
  res$copy <- tumour_corrected_copy$copy - normal_corrected_copy$copy
  # Sort by chromosome order
  x <- as.data.frame(res)
  x$space <- factor(as.character(x$space), levels= myChromosome)
  out <- as(x, "RangedData")
  return(out)
}

