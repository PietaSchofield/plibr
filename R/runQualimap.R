#' QC check using Qualimap
#'
#' @param fileList the bam files
#'
#' @export
runQualimap <- function(sampleID,projName,bamDir,bamExt="_dedup_realn_recal.bam",
                        outDir="Analysis/qualimap",remRoot="/scratch/pschofield/Projects",
                        noSub=F,ncores=1,scpIt=T,mem="32Gb",
                        qualimapMod= "apps/qualimap/2.2/linux-x86_64"){
  outDir <- file.path("/scratch/pschofield/Projects",projName,"Analysis/qualimap",sampleID)
  alignedBAM <- file.path(bamDir, paste0(sampleID, bamExt))
  script <- c(
    paste0("mkdir -p ",outDir),
    paste0("unset DISPLAY"),
    paste0("module load ",qualimapMod),
    paste0("qualimap bamqc -bam ", alignedBAM, " -outdir ", outDir)
  )
  plib::runScript(jname=paste0("qualimap_",sampleID),jproj=projName,
                    jdesc=paste0("qualimap align for project ",projName," on sample ", sampleID),
                    jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}
