#' Calculate Genome Instability
#'
#' @param ds copy number data
#'
#' @export
cnvGIindex <- function(ds,segData,pl){
  # Calculate genome instability index (GI)
  cat("Calculating genome instability index\n", sep="")
  GIidx <- lapply(names(ds), function(nds){
    x <- ds[[nds]]
    # Remove segments without ratio (i.e. NAs)
    x <- x[!is.na(x$copy), ]
    workingRegion <- sum(as.numeric(width(x)))
    # Determine number of bases with copy number deviate from neutral
    y <- segData[[nds]]$segs
    # Remove neutral events
    y <- y[which(y$state != 3), ]
    if(nrow(y) > 0){  
      gr <- GRanges(seqnames = as.character(y$chr),
             range = IRanges(start=y$start, end=y$end),
             strand = "*"
             )
      basesWithCNV <- sum(as.numeric(width(gr)))
    }else{
      basesWithCNV = 0
    }
    # Genome stability index (GI) = percentage of total working region that
    # deviate from neutral status
    basesWithCNV/workingRegion * 100
  })
  names(GIidx) <- names(ds)
  write.csv(as.data.frame(GIidx), file=file.path(pl[["outDir"]], "genome_instability_index.csv"),
            row.names=TRUE)
  GIidx
}
