#'  Load Fcount output from : DEXSeq_after_Fcount.R into DEXSeq
#'  Copyright 2015 Vivek Bhardwaj (bhardwaj@ie-freiburg.mpg.de). Licence: GPLv3.
#'  Read Fcount output and convert to dxd
#' 
#' @export
DSFromFC <- function(countFile){ 
  # Take a fcount file and convert it to dcounts for dexseq
  message("Reading and adding Exon IDs for DEXSeq")
  fc <- read.table(countFile,,head=T,skip = 1)
  fc <- dplyr::arrange(fc, Geneid,Chr,Start) 
  dcounts <- dplyr::select(fc,-c(2:6))
  id <- as.character(dcounts[,1])
  n <- id
  split(n,id) <- lapply(split(n ,id), seq_along )
  rownames(dcounts) <- sprintf("%s%s%03.f",id,":E",as.numeric(n))
  dcounts <- dcounts[,2:ncol(dcounts)]
  dcounts <- dcounts[substr(rownames(dcounts), 1, 1) != "_", ] #remove _ from beginnning of gene name 
  ## get genes and exon names out
  splitted <- strsplit(rownames(dcounts), ":")
  exons <- sapply(splitted, "[[", 2)
  genesrle <- sapply(splitted, "[[", 1)
  list(dcounts=dcounts,exons=exons,genesrle=genesrle)
}
