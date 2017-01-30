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
                         refSequence= NULL, refGenome = NULL, ncores=16, fqExt=".fastq",pe=T){
  # Set up the file names and paths
  outStub <- paste0(sampleID,"_")
  tmpDir <- file.path(outRoot,"tmp")
  bamDir <- file.path(outRoot,outName)
  starBAM <- file.path(bamDir,paste0(outStub,"Aligned.sortedByCoord.out.bam"))
  niceBAM <- file.path(bamDir,paste0(outStub,"full.bam"))
  uniqueBAM <- file.path(bamDir,paste0(outStub,"unique.bam"))
  tsBAM <- file.path(bamDir,paste0(outStub,"Aligned.toTranscriptome.out.bam"))
  niceTsBAM <- file.path(bamDir,paste0(outStub,"transcriptome.bam"))
  inputFastq1 <- file.path(inputPath,paste0(sampleID, "_R1",fqExt,".gz", sep=""))
  parts <- unlist(strsplit(sampleID,"_"))
  RG.ID <- sampleID
  RG.SM <- parts[[1]]
  RG.LB <- sampleID
  RG.PU <- parts[[2]]
  ReadGroup <- paste('ID:',RG.ID,'SM:',RG.SM,'PL:ILLUMINA","LB:',RG.LB,'PU:',RG.PU,sep=" ")
  if(pe){
    inputFastq2 <- gsub("_R1","_R2",inputFastq1)
    # generate the script commands
    script <- c(
      paste0("# run ",sampleID),
      paste0("module load ", starMod),
      paste0("module load ",samtoolsMod),
      paste0("mkdir -p ",bamDir),
      paste0("STAR --runThreadN ",ncores," --genomeDir ",refSequence,
             " --sjdbGTFfile ",refGenome,
             " --quantMode TranscriptomeSAM GeneCounts ",
             " --quantTranscriptomeBan Singleend ",
             " --outFileNamePrefix ",file.path(bamDir,outStub),
             " --outFilterScoreMinOverLread 0 --outFilterMatchNminOverLread 0 --outFilterMatchNmin 40 ",
             " --readFilesCommand zcat --outSAMstrandField intronMotif --outSAMattributes All ",
             " --outSAMunmapped Within KeepPairs --outWigType wiggle --outWigStrand Stranded ",
             " --outSAMattrRGline ",ReadGroup," --outSAMtype BAM SortedByCoordinate ",
             " --readFilesIn ",inputFastq1," ",inputFastq2),
      paste("mv ",starBAM," ",niceBAM),
      paste("samtools index ",niceBAM),
      paste("mv ",tsBAM," ",niceTsBAM)
    )
  }else{
    script <- c(
      paste0("# run ",sampleID),
      paste0("module load ", starMod),
      paste0("module load ",samtoolsMod),
      paste0("mkdir -p ",bamDir),
      paste0("STAR --runThreadN ",ncores," --genomeDir ",refSequence,
             " --sjdbGTFfile ",refGenome,
             " --quantMode GeneCounts ",
             " --outFileNamePrefix ",file.path(bamDir,outStub),
             " --readFilesCommand zcat --outSAMstrandField intronMotif --outSAMattributes All ",
             " --outWigType wiggle --outWigStrand Stranded ",
             " --outSAMattrRGline ",ReadGroup," --outSAMtype BAM SortedByCoordinate ",
             " --readFilesIn ",inputFastq1),
      paste("mv ",starBAM," ",niceBAM),
      paste("samtools index ",niceBAM),
      paste("mv ",tsBAM," ",niceTsBAM)
    )
  }
  plib::runScript(jname=paste0("star_",sampleID),jproj=projName,
                       jdesc=paste0("STAR align for project ",projName," on sample ", sampleID),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}


