#' run the picard and GATK pre processing steps on bam alignments
#'
#' @param filename merged bam file
#'
#' @export
dedupCNV <- function(inBAM,projName,refGenome,gatkStep=T,
                     tmpDir=file.path("/scratch/pschofield/Projects",projName,"tmp"),
                     picardDir="/apps/modules/pkg/apps/picardtools/1.96/noarch",
                     gatkDir="/apps/modules/pkg/apps/gatk/3.1.1/noarch",
                     javaMod="compilers/java/1.7.0_80/noarch" ,
                     gatkMod="apps/gatk/3.1.1/noarch",
                     picardMod="apps/picardtools/1.96/noarch" ,
                     samtoolsMod="apps/samtools/0.1.19/gcc/4.4.7" ,
                     gatkRes= "/data/compbio/hsleong/Genome/gatk/hg19/bundle",
                     ncores=1,scpIt=T,noSub=F
                     ){

  MillsIndels = file.path(gatkRes, "Mills_and_1000G_gold_standard.indels.b37.vcf")
  Genome1000Indels = file.path(gatkRes, "1000G_phase1.indels.b37.vcf")
  dbSNPs = file.path(gatkRes, "dbsnp_138.b37.vcf")
  # Filter alignment and retain proper pairs
  sampleName <- gsub("[.]bam","",basename(inBAM))
  bamDir <- dirname(inBAM)
  uniqueBAM <- file.path(bamDir, paste0(sampleName, "_unique.bam"))
  sortedBAM <- file.path(bamDir, paste0(sampleName, "_sorted.bam"))
  dedupBAM <- file.path(bamDir, paste0(sampleName, "_dedup.bam"))
  dedupMetrics <- file.path(bamDir, paste0(sampleName, "_metrics.txt"))
  rmdupBAM <- file.path(bamDir, paste0(sampleName, "_rmdup.bam"))
  rmdupMetrics <- file.path(bamDir, paste0(sampleName, "_rmdup.txt"))
  realnTargetIntervals <- file.path(bamDir, paste0(sampleName, "_realigner.intervals"))
  realnBAM <- file.path(bamDir, paste0(sampleName, "_realn.bam"))
  recalTable <- file.path(bamDir, paste0(sampleName, "_recal.table"))
  recalBAM <- file.path(bamDir, paste0(sampleName, "_dedup_realn_recal.bam"))

  script <- c(  
    paste0("module load ",javaMod),
    paste0("module load ",samtoolsMod),
    paste0("module load ",gatkMod),
    paste0("module load ",picardMod),
    paste("samtools view -b -q 10 -f 2 -F 780 -o", uniqueBAM, inBAM, sep=" "),
    paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
      " -jar ", file.path(picardDir, "SortSam.jar"),  
      " INPUT=", uniqueBAM, " OUTPUT=", sortedBAM, 
      " SORT_ORDER=coordinate", sep=""),
    paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
      " -jar ", file.path(picardDir, "MarkDuplicates.jar"),
      " INPUT=", sortedBAM, " OUTPUT=", dedupBAM, 
      " METRICS_FILE=", dedupMetrics, sep=""),
    paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
      " -jar ", file.path(picardDir, "BuildBamIndex.jar"), 
      " INPUT=", dedupBAM, sep=""),
    paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
      " -jar ", file.path(picardDir, "MarkDuplicates.jar"),
      " INPUT=", sortedBAM, " OUTPUT=", rmdupBAM, 
      " METRICS_FILE=", rmdupMetrics, 
      " REMOVE_DUPLICATES=true ASSUME_SORTED=true", sep=""),
    paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
      " -jar ", file.path(picardDir, "BuildBamIndex.jar"), 
      " INPUT=", rmdupBAM, sep="")
    )
  if(gatkStep){
    script <- c(script,
      paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
        " -jar ", file.path(gatkDir, "GenomeAnalysisTK.jar"), 
        " -T RealignerTargetCreator -R ", refGenome, 
        " -I ", dedupBAM, " -known ",  MillsIndels, " -known ", Genome1000Indels,
        " -o ", realnTargetIntervals, sep=""),
      paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
        " -jar ", file.path(gatkDir, "GenomeAnalysisTK.jar"), 
        " -T IndelRealigner -R ", refGenome, 
        " -I ", dedupBAM, " -known ", MillsIndels, " -known ", Genome1000Indels, 
        " -targetIntervals ", realnTargetIntervals, " -o ", realnBAM, sep=""),
      paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
        " -jar ", file.path(gatkDir, "GenomeAnalysisTK.jar"), 
        " -T BaseRecalibrator -R ", refGenome, 
        " -I ", realnBAM, " -knownSites ", MillsIndels, 
        " -knownSites ", Genome1000Indels, " -knownSites ", dbSNPs,
        " -o ", recalTable, sep=""),
      paste("java -Xmx32g -Djava.io.tmpdir=", tmpDir,  
        " -jar ", file.path(gatkDir, "GenomeAnalysisTK.jar"), 
        " -T PrintReads -R ", refGenome, " -I ", realnBAM, 
        " -BQSR ", recalTable, " -o ", recalBAM, sep="")
    )
  }
  plib::runScript(jname=paste0("dedup_",sampleName),jproj=projName,
                       jdesc=paste0("dedup  ",projName," on sample ", sampleName),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")

}



