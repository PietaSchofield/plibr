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
 alignSubread <- function(fileName,projName,runId,
                         outRoot=file.path("/scratch/pschofield/Projects",projName),
                         outName="Data/alignments/subread",noSub=F,scpIt=T,
                         subreadMod = "apps/subread/1.5.0-p3/gcc-4.4.7",
                         samtoolsMod =  "apps/samtools/1.2/gcc/4.4.7",
                         mem="32Gb",stranded="fr",nmap="2",itype=1,
                         refGenome = NULL, ncores=16, fqExt=".fastq",pe=T){
  # Set up the file names and paths
  bamDir <- file.path(outRoot,outName)
  sampleName <- gsub("_R1.*","",basename(fileName))
  outBAM <- file.path(bamDir,paste0(sampleName,"_a.bam"))
  sortBAM <- file.path(bamDir,paste0(sampleName,".bam"))
  inputFastq1 <- fileName
  RG.ID <- runId
  RG.LB <- sampleName
  RG.SM <- gsub("(_L[0-4].*|_R1.*)","",sampleName)
  lane <- gsub(paste0("(",RG.SM,"|_R1.*)"),"",sampleName)
  RG.PU <- paste0(runId,".",lane,".",RG.SM) 
  ReadGroup <- paste('SM:',RG.SM,'PL:ILLUMINA","LB:',RG.LB,'PU:',RG.PU,sep=" ")
  # generate the script commands
  if(pe){
    inputFastq2 <- gsub("_R1","_R2",inputFastq1)
    script <- c(
      paste0("# run ",basename(sampleName)),
      paste0("module load ", subreadMod),
      paste0("module load ",samtoolsMod),
      paste0("mkdir -p ",bamDir),
      paste0("subread-align -T ",ncores," -t ",itype," -B ",nmap,
             " -i ",refGenome,
             " -o ",outBAM,
             " --rg-id ",RG.ID,
             " --rg ",ReadGroup,
             " -d 50 -D 2000 ",
             " -S ",stranded,
             " -r ",inputFastq1,
             " -R ",inputFastq2),
      paste0("sambamba sort --nthreads ",ncores," -o ",sortBAM," ",outBAM),
      paste0("sambamba index ",sortBAM)
    )
  }else{
    script <- c(
      paste0("# run ",basename(sampleName)),
      paste0("module load ", subreadMod),
      paste0("module load ",samtoolsMod),
      paste0("mkdir -p ",bamDir),
      paste0("subread-align -T ",ncores," -t ",itype," -B ",nmap,
             " -i ",refGenome,
             " -o ",outBAM,
             " --rg-id ",RG.ID,
             " --rg ",ReadGroup,
             " -S ",stranded,
             " -r ",inputFastq1),
      paste0("sambamba sort --nthreads ",ncores," -o ",sortBAM," ",outBAM),
      paste0("sambamba index ",sortBAM)
    )
  }
  plib::runScript(jname=paste0("subread_",sampleName),jproj=projName,
                       jdesc=paste0("Subread align for project ",projName," on sample ", sampleName),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}


