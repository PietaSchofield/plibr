#' Simple STAR aligner does everything counts junctions and genes
#'
#' This is a simpler STAR aligner that runs of single fastq files rather than individual lane fastqs from
#' the HiSeq. There maybe QC advantages of aligning the individual fastqc but sometimes it is just
#' simpler to merge before the event.
#'
#' @param projName project name
#' @param outRoot output root directory
#' @param fileList list of fastq file
#'
#' @export
 alignStar <- function(projName,sampleID,inputPath,
                         outRoot=file.path("/scratch/pschofield/Projects",projName),
                         outName="Data/alignment/star",noSub=F,scpIt=T,
                         starMod = "apps/star/2.5.1b/gcc-5.1.0",
                         samtoolsMod =  "apps/samtools/0.1.19/gcc/4.4.7",
                         refSequence= NULL,
                         refGenome = NULL, ncores=16, fqExt=".fastq",pe=T){
  # Set up the file names and paths
  tmpDir <- file.path(outRoot,"tmp")
  bamDir <- file.path(outRoot,outName)
  outBAM <- file.path(bamDir,paste0(sampleID,"sortedByCoord.out.bam"))
  uniqueBAM <- file.path(bamDir,paste0(sampleID,"unique.bam"))
  inputFastq1 <- file.path(inputPath,paste0(sampleID, "_R1",fqExt,".gz", sep=""))
  if(pe){
    inputFastq2 <- gsub("_R1_","_R2_",inputFastq1)
  }else{
    inputFastq2 <- " "
  }
  parts <- unlist(strsplit(basename(sampleID),"_"))
  RG.ID <- sampleID
  RG.SM <- parts[[1]]
  RG.LB <- sampleID
  RG.PU <- parts[[2]]
  ReadGroup <- paste('ID:',RG.ID,'SM:',RG.SM,'PL:ILLUMINA","LB:',RG.LB,'PU:',RG.PU,sep=" ")
  # generate the script commands
  script <- c(
    paste0("# run ",basename(sampleID)),
    paste0("module load ", starMod),
    paste0("module load ",samtoolsMod),
    paste0("mkdir -p ",bamDir),
    paste0("STAR --runThreadN ",ncores," --genomeDir ",refSequence,
           " -–sjdbGTFfile ",refGenome,
           " --quantMode TranscriptomeSAM GeneCounts ",
           " --quantTranscriptomeBan Singleend ",
           " --outFileNamePrefix ",file.path(bamDir,sampleID),
           " --readFilesCommand zcat --outSAMstrandField intronMotif --outSAMattributes All ",
           " --outSAMunmapped Within KeepPairs --outWigType wiggle --outWigStrand Stranded ",
           " --outSAMattrRGline ",ReadGroup," --outSAMtype BAM SortedByCoordinate ",
           " --readFilesIn ",inputFastq1," ",inputFastq2),
    paste("samtools view -b -q 10 -f 2 -F 780 -o", uniqueBAM, outBAM, sep=" "),
    paste("samtools index ",uniqueBAM),
    paste("samtools index ",outBAM)
  )
  plib::runScript(jname=paste0("star_",sampleID),jproj=projName,
                       jdesc=paste0("STAR align for project ",projName," on sample ", sampleID),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}


