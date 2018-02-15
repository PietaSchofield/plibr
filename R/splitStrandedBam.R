#' a routine to split and filter by insert size strand specific bam files
#'
#' @param bamFiles and list of fully qualified bamfile names
#' @param len length of insert to split by
#'
#' @export 
splitStrandedBAM <- function(bamFiles){
  lapply(bamFiles,function(fn){
    ofn <- fn
    fw1 <- gsub("[.]bam$","_fw1.bam",ofn)
    fw2 <- gsub("[.]bam$","_fw2.bam",ofn)
    rv1 <- gsub("[.]bam$","_rv1.bam",ofn)
    rv2 <- gsub("[.]bam$","_rv2.bam",ofn)
    fw <- gsub("[.]bam$","_forward.bam",ofn)
    rv <- gsub("[.]bam$","_reverse.bam",ofn)
    script <- c(
      paste0('mkdir -p ',dirname(ofn)),
      paste0('samtools view -b -f 128 -F 16 ',fn," > ",fw1),
      paste0('samtools index ',fw1),
      paste0('samtools view -b -f 64 -F 32 ',fn," > ",fw2),
      paste0('samtools index ',fw2),
      paste0('samtools merge -f ',fw,' ',fw1,' ',fw2),
      paste0('samtools index ',fw),
      paste0('samtools view -b -f 144 ',fn," > ",rv1),
      paste0('samtools index ',rv1),
      paste0('samtools view -b -f 96 ',fn," > ",rv2),
      paste0('samtools index ',rv2),
      paste0('samtools merge -f ',rv,' ',rv1,' ',rv2),
      paste0('samtools index ',rv),
      paste0("rm ",rv1),
      paste0("rm ",rv2),
      paste0("rm ",fw1),
      paste0("rm ",fw2),
      paste0("rm ",rv1,".bai"),
      paste0("rm ",rv2,".bai"),
      paste0("rm ",fw1,".bai"),
      paste0("rm ",fw2,".bai")
    )
    fnstub <- gsub("bam$","sh",fn)
    writeLines(script,con=fnstub)
  })
}
