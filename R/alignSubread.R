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
 alignSubread <- function(projName,sampleID,inputPath,outStub=NULL,
                         outRoot=file.path("/scratch/pschofield/Projects",projName),
                         outName="Data/alignment/star",noSub=F,scpIt=T,
                         subreadMod = "apps/subread/1.5.0-p3/gcc-4.4.7",
                         samtoolsMod =  "apps/samtools/1.2/gcc/4.4.7",
                         mem="32Gb",stranded="fr",nmap="2",itype=1,
                         refGenome = NULL, ncores=16, fqExt=".fastq",pe=T){
  # Set up the file names and paths
  if(is.null(outStub)) {outStub <- paste0(sampleID)}
  tmpDir <- file.path(outRoot,"tmp")
  bamDir <- file.path(outRoot,outName)
  outBAM <- file.path(bamDir,paste0(outStub,"_full.bam"))
  sortBAM <- file.path(bamDir,paste0(outStub,".bam"))
  uniqueBAM <- file.path(bamDir,paste0(outStub,"_unique.bam"))
  inputFastq1 <- file.path(inputPath,paste0(sampleID, "_R1",fqExt,".gz", sep=""))
  parts <- unlist(strsplit(basename(sampleID),"_"))
  RG.ID <- sampleID
  RG.SM <- parts[[1]]
  RG.LB <- sampleID
  RG.PU <- parts[[2]]
  ReadGroup <- paste('SM:',RG.SM,'PL:ILLUMINA","LB:',RG.LB,'PU:',RG.PU,sep=" ")
  # generate the script commands
  if(pe){
    inputFastq2 <- gsub("_R1","_R2",inputFastq1)
    script <- c(
      paste0("# run ",basename(sampleID)),
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
      paste0("# run ",basename(sampleID)),
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
  plib::runScript(jname=paste0("subread_",sampleID),jproj=projName,
                       jdesc=paste0("STAR align for project ",projName," on sample ", sampleID),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}


