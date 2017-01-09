#' Submit a cluster job to fastQC a directory
#'
#' @param fastqDir input directory
#' @param fastqPattern pattern for fastq file names unix wildcards not re
#' @param outDir subdirectory of project fastqc directory
#' @param projName project name will build output path
#' @param fastQCDir  output root directory 
#' @param jobName name for job script
#' @param fastqcMod fastQC module to load
#' @param noSub debug flag
#' @param makeCVS make cvs file with listing details
#'
#' @export
subFastQC <- function(fastqDir,fastqPattern="*.gz",outDir="pre",
                      projName="Sandbox",noSub=F,
                      projDir=file.path("/scratch/pschofield/Projects",projName),
                      fastQCDir=file.path("/scratch/pschofield/Projects",projName,"fastqc"),
                      jobName=paste0("fastQC_", projName),
                      fastqcMod="apps/fastqc/0.11.3/linux-x86_64",
                      nproc=16,mem="48gb",makeCVS=T){
  jobDesc <- paste0("Run fastqc on ",fastqDir," for project ",projName)
  if(makeCVS){
    cvsLine <- paste0("ls -la ",file.path(fastqDir,fastqPattern)," > ",
                      file.path(projDir,"fqFilelist.txt"))
  }else{
    cvsLine <- " "
  }
  script <- c(
    cvsLine,
    paste0("module load ",fastqcMod),
    paste0("mkdir -p ",file.path(fastQCDir,outDir)),
    paste0("fastqc ",file.path(fastqDir,fastqPattern),
           " -o ",file.path(fastQCDir,outDir)," -f fastq --threads ",nproc)
  )
  runScript(jname=jobName,jproj=projName,jscrp=script,jdesc=jobDesc,noSub=noSub,nproc=nproc,mem=mem)
}
