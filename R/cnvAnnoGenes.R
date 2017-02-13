#' annotate CNV segmentation and copynumber call
#'
#' @param segData segmentation dataset
#' @param annoRdata gene Annotation file
#'
#' @export
cnvAnnoGenes <- function(segData,annoRdata,pl){
  # Load cytoband info
  load(annoRdata)
  outDir <- pl[["outDir"]]
  # Determine copy number changes in each gene
  cat("Create gene by copy number matrix\n", sep="")
  # Loop through samples and find overlaps between genes and segments
  #
  gene.cnv <- do.call('cbind', lapply(segData, function(segs){
    x <- segs$segs
    gr <- GRanges(seqnames = as.character(x$chr),
           range = IRanges(start=x$start, end=x$end),
           strand = "*",
           state = as.character(x$state),
           ratio = x$median
           )
    olp <- findOverlaps(geneAnnotation, gr, minoverlap=5L)
    res <- data.frame(stable.id = geneAnnotation$gene_id[queryHits(olp)],
                      state = as.numeric(as.character(gr$state[subjectHits(olp)]))
           )
    # Convert HMM state to copy number
    res$copy.number <- res$state - 1
    # Take average of copy number in cases where a gene overlaps with more than one segments
    fac <- factor(as.character(res$stable.id), levels=as.character(geneAnnotation$gene_id))
    meanCNV <- tapply(res$copy.number, fac, mean)
    return(meanCNV)
  }))

  # Generate the final table
  final.cnv.table <- data.frame(
    Gene_name = as.character(geneAnnotation$gene_name),
    Ensembl_ID = as.character(geneAnnotation$gene_id),
    Biotype =  as.character(geneAnnotation$gene_biotype),
    Chromosome = as.character(seqnames(geneAnnotation)),
    Start = start(geneAnnotation),
    End = end(geneAnnotation),
    gene.cnv
  )

  # Exclude mitochondrial genome as this is not included in the plot
  final.cnv.table <- final.cnv.table[final.cnv.table$Chromosome != "MT", ]
  # Reorder by chromosome
  final.cnv.table$Chromosome <- factor(final.cnv.table$Chromosome, levels=c(1:22, "X", "Y"))
  final.cnv.table <- final.cnv.table[order(final.cnv.table$Chromosome), ]
  # Write results out
  write.csv(final.cnv.table, file=file.path(outDir, "cnv_by_gene.csv"), row.names=FALSE)
  final.cnv.table
}

