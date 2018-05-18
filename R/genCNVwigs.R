#' generate wiggle tracks for HMMcopy 
#'
#' @param sampleID the bam files
#' @param projName the project
#' @param location of bam files
#'
#' @export
genCNVwigs <- function(sampleID,projName,bamDir,bamExt="_dedup_realn_recal.bam",
                        outDir=file.path("/scratch/pschofield/Projects",projName,"Analysis/wigs"),
                        noSub=T,ncores=1,scpIt=T,mem="16Gb",windowSize,
                        chrNames=c(seq(1,22),"X","Y"),
                        hmmCopyMod= "apps/hmmcopy/0.99.0/gcc-4.4.7"){
  outFile <- file.path(outDir,paste0(sampleID,"_w",windowSize,".wig"))
  alignBAM <- file.path(bamDir, paste0(sampleID, bamExt))
  script <- c(
    paste0("mkdir -p ",outDir),
    paste0("module load ",hmmCopyMod),
    paste0("readCounter -w ",windowSize," -c ",paste0(chrNames,collapse=",")," ",
           alignBAM," -b "),
    paste0("readCounter -w ",windowSize," -c ",paste0(chrNames,collapse=",")," ",
           alignBAM," > ",outFile)
  )
  plib::runScript(jname=paste0("genwig_",sampleID),jproj=projName,
                    jdesc=paste0("generate HMMCopy wig for ",projName," on sample ", sampleID),
                    jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}
