#' Simple subread aligner 
#'
#' This is a simpler subread aligner that runs of single fastq files rather than individual lane fastqs
#' from the HiSeq. There maybe QC advantages of aligning the individual fastqc but sometimes it is just
#' simpler to merge before the event.
#'
#' @param projName project name
#' @param outRoot output root directory
#' @param fileList list of fastq file
#'
#' @export
 alignBWA <- function(fileName,projName,runId,
                         outRoot=file.path("/scratch/pschofield/Projects",projName),
                         outName="Data/alignments/bwa",noSub=F,scpIt=T,
                         bwaMod = "apps/bwa/0.7.13/gcc-4.4.7",
                         samtoolsMod =  "apps/samtools/0.1.19/gcc/4.4.7",
                         mem="32Gb", refGenome = NULL, ncores=8, fqExt=".fastq",pe=T){
  # Set up the file names and paths
  bamDir <- file.path(outRoot,outName)
  sampleName <- gsub("_R1.*","",basename(fileName))
  alignedBAM <- file.path(bamDir,paste0(sampleName,".bam"))
  alignedSAM <- file.path(bamDir,paste0(sampleName,".sam"))
  inputFastq1 <- fileName
  inputFastq2 <- gsub("_R1_","_R2__",inputFastq1)
  RG.ID <- runId
  RG.LB <- sampleName
  RG.SM <- gsub("(_L[0-4].*|_R1.*)","",sampleName)
  lane <- gsub(paste0("(",RG.SM,"|_R1.*)"),"",sampleName)
  RG.PU <- paste0(runId,".",lane,".",RG.SM) 
  ReadGroup <- paste('@RG\\tID:', RG.ID, '\\tSM:', RG.SM, '\\tPL:ILLUMINA\\tLB:',
       RG.LB, '\\tPU:', RG.PU, sep="")
  # generate the script commands
  inputFastq2 <- gsub("_R1","_R2",inputFastq1)
  script <- c(
    paste0("# run ",basename(sampleName)),
    paste0("module load ", bwaMod),
    paste0("module load ",samtoolsMod),
    paste0("mkdir -p ",bamDir),
    paste0("bwa mem -t ",ncores," -M -R '", as.character(ReadGroup), "' ",
      refGenome, " ", inputFastq1, " ", inputFastq2, " > ", alignedSAM),
    paste0("samtools view -bS -o ", alignedBAM, " ", alignedSAM),
    paste0("rm ",alignedSAM)
  )
  plib::runScript(jname=paste0("bwa_",sampleName),jproj=projName,
                       jdesc=paste0("BWA align for project ",projName," on sample ", sampleName),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}
