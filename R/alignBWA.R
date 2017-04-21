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
 alignBWA <- function(fileName,projName,runId,sampleBC=NULL,
                         outRoot=file.path("/scratch/pschofield/Projects",projName),
                         outName="Data/alignments/bwa",noSub=F,scpIt=T,
                         bwaMod = "apps/bwa/0.7.13/gcc-4.4.7",debugRG=F,
                         samtoolsMod =  "apps/samtools/0.1.19/gcc/4.4.7",
                         mem="32Gb", refGenome = NULL, ncores=8, fqExt=".fastq",pe=T){
  # Set up the file names and paths
  bamDir <- file.path(outRoot,outName)
  seqName <- gsub("_R1.*","",basename(fileName))
  libName <- gsub("_L[0-9]*_.*","",basename(fileName))
  if(is.null(sampleBC)){
    sampleBC <- libName
  }
  laneName <- gsub(paste0(libName,"_"),"",seqName)
  alignedBAM <- file.path(bamDir,paste0(seqName,".bam"))
  alignedSAM <- file.path(bamDir,paste0(seqName,".sam"))
  inputFastq1 <- fileName
  inputFastq2 <- gsub("_R1_","_R2__",inputFastq1)
  RG.LB <- libName
  RG.SM <- libName 
  RG.ID <- paste0(runId,".",laneName)
  RG.PU <- paste0(runId,".",laneName,".",sampleBC) 
  ReadGroup <- paste('@RG\\tID:', RG.ID, '\\tSM:', RG.SM, '\\tPL:ILLUMINA\\tLB:',
       RG.LB, '\\tPU:', RG.PU, sep="")
  # generate the script commands
  inputFastq2 <- gsub("_R1","_R2",inputFastq1)
  script <- c(
    paste0("# run ",basename(seqName)),
    paste0("module load ", bwaMod),
    paste0("module load ",samtoolsMod),
    paste0("mkdir -p ",bamDir),
    paste0("bwa mem -t ",ncores," -M -R '", as.character(ReadGroup), "' ",
      refGenome, " ", inputFastq1, " ", inputFastq2, " > ", alignedSAM),
    paste0("samtools view -bS -o ", alignedBAM, " ", alignedSAM),
    paste0("rm ",alignedSAM)
  )
  if(!debugRG){
    plib::runScript(jname=paste0("bwa_",seqName),jproj=projName,
                       jdesc=paste0("BWA align for project ",projName," on sample ", seqName),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
  }else{
    c(seqName, libName, laneName, ReadGroup)
  }
}
