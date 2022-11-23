#' generate some bigwig tracks from paired reads
#'
#' @param projName project name
#' @param fileList bam files to use
#' @param outPath directory to write output to
#'
#' @export 
genCovTracks <- function(projName,fileList, outPath, paired=T){
  outDir <- file.path("/scratch/pschofield/Projects",projName,outPath) 
  lapply(fileList,function(ffn){
    fname <- basename(ffn)
    bamDir <- dirname(ffn)
    fn <- gsub("[.]bam$","",fname)
    if(paired){
      rline <- paste0("frags <- readGAlignmentPairs('",file.path(bamDir,fname),"')")
    }else{
      rline <- paste0("frags <- readGAlignments('",file.path(bamDir,fname),"')")
    }
    rscript <- c(
      "require(GenomicAlignments)",
      "require(rtracklayer)",
      rline,
      "covTrack <- coverage(frags)",
      paste0("dir.create('",outDir,"',recursive=T,showWarnings=F)"),
      paste0("export(covTrack,con='",file.path(outDir,gsub("bam$","bw",fname)),"')"),
      "q()"
      )
   plib::runRScript(rscript,jname=paste0("bam2bw_",fn),jproj=projName,
                        jdesc=paste0("Run Rscript to make bw ",fname," "))
  })
}
