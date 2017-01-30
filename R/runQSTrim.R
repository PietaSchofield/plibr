#' Submit a list of files associated with a project for  QC checking with fastQC file
#'
#' @param projName project name
#' @param outRoot output root directory
#' @param fileList list of fastq file
#'
#' @export
runQSTrim <- function(projName,outRoot=file.path("/scratch/pschofield/Projects",projName),
                      fileList=NULL,outName="Data/trimmedfqs",noSub=F){
  # Parameters
  outputDir <- file.path(outRoot, outName)
  scriptsDir <- file.path(outRoot,projName,"Scripts")
  jMod <- "compilers/java/1.8.0_40/noarch"
  bbMod <- "apps/bbmap/36.20"
  fqtMod <- "apps/fqtrim/0.9.5"
  pyMod <- "apps/python"
  bbRef <- "/apps/modules/pkg/apps/bbmap/36.20/resources/truseq.fa.gz"

  # ---- fastQC
  res <- lapply(fileList, function(pn){
    key <- gsub(".fastq.gz", "", basename(pn))
    outDir <- file.path(outputDir,key)
    inFastq <- pn
    trimmedFq1 <- file.path(outputDir,paste0(key,"_bb.fq"))
    finalTrimmedFq = file.path(outputDir,paste0(key, ".fq.gz"))

    script <- c(
    # Specify modules
      paste0("module load ",jMod),
      paste0("module load ",bbMod),
      paste0("module load ",fqtMod),
      paste0("module load ",pyMod), 
      paste0("mkdir -p ",outputDir),
    # 1. Remove the first 12 bases with BBDUK as suggested in https://www.lexogen.com/quantseq-data-analysis/
    # k = Kmer length used for finding contaminants. Contaminants shorter than k will not be found.
    # ktrim = Trim reads to remove bases matching reference kmers.
    # forcetrimleft = Trim bases to the left of this position (0-based)
    # qtrim = Trim read ends to remove bases with quality below trimq. 
    # trimq = Regions with average quality BELOW this will be trimmed.
      paste0("bbduk.sh in=", inFastq, " out=", trimmedFq1," overwrite=t",
             " ref=",bbRef, " k=13 ktrim=r forcetrimleft=11 qtrim=w trimq=10 minlength=20",
             " stats=",file.path(outRoot,"log",paste0(key,"_trim.stats"))),

    # 2. Trim trailing Gs and polyA tail
      paste0('cutadapt -a \"G{75}\" ', trimmedFq1, 
            " | fqtrim -l 20 -p 16 -o ", finalTrimmedFq, " - ")
    ) 
    # Submit the script
    plib::runScript(jname=paste0("trim_",key),jproj=projName,
                       jdesc=paste0("Trim with bbduk and fqtrim for project ",projName," on file ", pn),
                       jscrp=script,noSub=noSub,mem="48Gb",nproc=16) 
  })
}
