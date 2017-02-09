#' Submit a list of files associated with a project for  QC checking with fastQC file
#'
#' @param projName project name
#' @param outRoot output root directory
#' @param fileList list of fastq file
#'
#' @export
runFastQC <- function(projName,outRoot=file.path("/scratch/pschofield/Projects",projName),
                      fileList=NULL,outName="Analysis/fastqc",fileFormat="fastq",
                      noSub=F,scpIt=T,fileExt=".fastq.gz"){
  # Parameters
  outputDir <- file.path(outRoot, outName,fileFormat)
  scriptsDir <- file.path(outRoot,projName,"Scripts")
  fastqcModule <- "apps/fastqc/0.11.3/linux-x86_64"

  # ---- fastQC
  res <- lapply(fileList, function(pn){
    key <- gsub(fileExt, "", basename(pn))
    outDir <- file.path(outputDir,key)
    inputFile <- pn
    script <- c(
      paste0("module load ",fastqcModule),
      paste("mkdir -p ",outputDir),
      paste("fastqc", inputFile, "-o", outputDir, "-f ",fileFormat, sep=" ")
    )
    # Submit the script
    plib::runScript(jname=paste0("fastqc_",key),jproj=projName,
                       jdesc=paste0("FastQC run for project ",projName," on file ", pn),
                       jscrp=script,noSub=noSub,nproc=2,scpIt=scpIt)
  })
  res
}
