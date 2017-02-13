#' Function for reading in wig file from a single sample
#'
#' @export
cnvCopyNumber <- function(readcountFile, gcFile, mapFile, myChromosome=c(1:22,"X","Y")) {
  # Convert reads to IRanges object
  uncorrected_reads <- HMMcopy::wigsToRangedData(readcountFile, gcFile, mapFile)
  # Corrected reads by GC-content and mappbility
  res <- HMMcopy::correctReadcount(uncorrected_reads, mappability=0.9, samplesize=100000)
  # Sort by chromosome order
  x <- as.data.frame(res)
  x$space <- factor(as.character(x$space), levels= myChromosome)
  out <- as(x, "RangedData")
  return(out)
}

