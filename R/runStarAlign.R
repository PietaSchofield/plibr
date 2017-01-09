#' Submit a list of files associated with a project for bwa alignment and deduping 
#'
#' @param projName project name
#' @param outRoot output root directory
#' @param fileList list of fastq file
#'
#' @export
runStarAlign <- function(projName,sampleID,fileTable,runName,
                         outRoot=file.path("/scratch/pschofield/Projects",projName),
                         outName="Data/alignment/star",noSub=F,scpIt=T,
                         starMod = "apps/star/2.5.1b/gcc-5.1.0",
                         samtoolsMod =  "apps/samtools/0.1.19/gcc/4.4.7",
                         refGenome = NULL, ncores=16, fqExt=".fq",pe=T,jm=F){

  # Set up the file names and paths
  sName <- unique(fileTable$sampleName)
  tmpDir <- file.path(outRoot,"tmp")
  bamDir <- file.path(outRoot,outName)
  CRUKlib::rcmd(paste0("mkdir -p ",bamDir))
  mergedBAM = file.path(bamDir, paste(sName, ".bam", sep=""))
  uniqueBAM = file.path(bamDir, paste(sName, "_unique.bam", sep=""))
  inputFilelist = paste(file.path(bamDir, 
    paste(unique(fileTable$library), "Aligned.sortedByCoord.out.bam", sep="")), collapse=" ")
  # generate the script commands
  if(!jm){
    script <- c(
      paste0("# run ",basename(sampleID)),
      paste0("module load ", starMod),
      paste0("module load ",samtoolsMod),
      unlist(lapply(unique(fileTable$library), function(lsn){
        inputPath <- unique(fileTable$path[grep(lsn,fileTable$library)])
        inputFastq1 <- file.path(inputPath,paste0(lsn, "_R1_",fqExt,".gz", sep=""))
        if(pe){
          inputFastq2 <- gsub("_R1_","_R2_",inputFastq1)
        }else{
          inputFastq2 <- " "
        }
        parts <- unlist(strsplit(basename(lsn),"_"))
        RG.ID <- unique(fileTable$runName[grep(lsn,fileTable$library)])
        RG.SM <- parts[[1]]
        RG.LB <- lsn
        RG.PU <- parts[[2]]
        ReadGroup <- paste('ID:',RG.ID,'SM:',RG.SM,'PL:ILLUMINA","LB:',RG.LB,'PU:',RG.PU,sep=" ")
        paste0("STAR --runThreadN ",ncores," --genomeDir ",refGenome,
               " --outFileNamePrefix ",file.path(bamDir,basename(lsn)),
               " --readFilesCommand zcat --outSAMstrandField intronMotif --outSAMattributes All ",
               " --outSAMunmapped Within KeepPairs --outWigType wiggle --outWigStrand Stranded ",
               " --outSAMattrRGline ",ReadGroup," --outSAMtype BAM SortedByCoordinate ",
               " --readFilesIn ",inputFastq1," ",inputFastq2)
      })),
      paste("samtools merge", mergedBAM, inputFilelist, sep=" "),
      paste("samtools view -b -q 10 -f 2 -F 780 -o", uniqueBAM, mergedBAM, sep=" "),
      paste("samtools index ",uniqueBAM),
      paste("samtools index ",mergedBAM)
    )
  }else{
    script <- c(
      paste0("# run ",basename(sampleID)),
      paste0("module load ", starMod),
      paste0("module load ",samtoolsMod),
      paste("samtools merge -f ", mergedBAM, inputFilelist, sep=" "),
      paste("samtools view -b -q 10 -f 2 -F 780 -o", uniqueBAM, mergedBAM, sep=" "),
      paste("samtools index ",uniqueBAM),
      paste("samtools index ",mergedBAM)
    )
  }
  CRUKlib::runScript(jname=paste0("star_",sName),jproj=projName,
                       jdesc=paste0("STAR align for project ",projName," on sample ", sName),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}


