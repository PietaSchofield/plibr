#' generate stats files for subread / subjunc alignments for summarisation
#'
#' @param fileName bam file
#' @param projName
#' 
#' @export
genSRstats <- function(fileName,projName,ncores=2,noSub=F,mem="8Gb",scpIt=T,
                       samtoolsMod="apps/samtools/1.3.1/gcc-4.4.7"){
  statFile <- gsub("bam$","stats",fileName)
  sampleID <- gsub("[.]bam$","",basename(fileName))
  script <- c(
    paste0("module load ",samtoolsMod),
    paste0("samtools stats ",fileName," | grep '^SN' | cut -f 2-3 > ",statFile)
  )
  plib::runScript(jname=paste0("genstats_",sampleID),jproj=projName,
                       jdesc=paste0("gen stats on subread alignment ",projName," on sample ", sampleID),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem=mem)
}

