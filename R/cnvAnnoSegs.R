#' annotate CNV segmentation and copynumber call
#'
#' @param segData segmentation dataset
#' @param cytoFile cytoband file
#'
#' @export
cnvAnnoSegs <- function(segData,annoRdata,cytoFile,pl){
  # Load cytoband info
  load(cytoFile)
  # Load gene annotation
  load(annoRdata)
  # Keep just the protein-coding genes
  G <- geneAnnotation
  G <- G[elementMetadata(G)$gene_biotype == "protein_coding", ]
  outfile = file.path(pl[["outDir"]], "segmentation_results.xlsx")
  wb = createWorkbook(type="xlsx")
  segGenes <- lapply(names(segData),function(sdn){
    m <- segData[[sdn]]$segs 
    m <- m[as.character(m$state) != 3, ]
    if(nrow(m) > 0){
      m$class <- rep("", nrow(m))
      m[m$state == 1, "class"] <- "0 copies, homozygous deletion"
      m[m$state == 2, "class"] <- "1 copy, heterozygous deletion"
      m[m$state == 4, "class"] <- "3 copies, gain"
      m[m$state == 5, "class"] <- "4 copies, amplification"
      m[m$state == 6, "class"] <- "5 copies, high level amplification"   
      m$segment.width <- m$end - m$start + 1
      query <- GRanges(seqnames=as.character(m$chr), range=IRanges(start=m$start, end=m$end), 
                       strand="*", seg.median=m$median)
      olp <- findOverlaps(query, G)
      pcg <- elementMetadata(G)$gene_name[subjectHits(olp)]
      fac <- factor(queryHits(olp), levels=c(1:nrow(m)))
      nhits <- tapply(pcg, fac, length)
      genes <- tapply(pcg, fac, function(x) paste(x, collapse=",") )
      m$num.genes <- nhits
      m$genes <- genes
      # Determine cytoband info
      o2 <- findOverlaps(query, gr.cytoband)
      cbhits <- paste0(as.character(seqnames(gr.cytoband)[subjectHits(o2)]), gsub("\\.[0-9]+", "", 
              as.character(gr.cytoband$cytoband[subjectHits(o2)])))
      m$cytoband <- tapply(cbhits, factor(queryHits(o2)), function(x) paste(unique(x), collapse=",") )
      m <- subset(m, select=c("chr", "start", "end", "cytoband", "segment.width", 
                              "median", "state", "class", "num.genes", "genes"))
      sht <- createSheet(wb,sdn)
      addDataFrame(m,sht)
      m
    }else{
      cat("No hits in", sdn, "\n", sep=" ")
    }
  })
  names(segGenes) <- names(segData)
  saveWorkbook(wb, outfile)
  segGenes
}
