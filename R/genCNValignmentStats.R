#' generate alignments stats for BWA CNV alignments
#'
#' @param bamDir directory of alignment files
#'
#' @export
genCNValignmentStats <- function(sid,bamDir, fqsDir, projName,
                                 remRoot=file.path("/scratch/pschofield/Projects",projName),
                                 outDir="Analysis/mapping_stats",
                                 noSub=F,ncores=1,scpIt=T,mem="32Gb",
                                 samtoolsMod="apps/samtools/1.2/gcc-4.4.7",
                                 ufExt=".bam",mdExt="_dedup.bam",rdExt="_rmdup.bam"){
  outDir <- file.path(remRoot,outDir)
  resFile <- file.path(outDir,paste0(sid,"_align.stats"))
  ufBam <- file.path(bamDir,paste0(sid,ufExt))
  mdBam <- file.path(bamDir,paste0(sid,mdExt))
  rdBam <- file.path(bamDir,paste0(sid,rdExt))
  outFile <- file.path(outDir, paste0(sid, "_insertSize.txt"))
  fqFiles <- rlsFiles(file.path(fqsDir,paste0(sid,"*_R1_*.gz")))
  script <- c(
    paste0("module load ",samtoolsMod),
    paste0("mkdir -p ",outDir),
    paste0("rm ",resFile),
    paste0("echo 'total' >> ",resFile),
    unlist(lapply(fqFiles,function(fqf){
      paste0( "zcat ", fqf, " | echo $((`wc -l`/4)) >> ", resFile)
    })),
    paste0("echo 'totalAlign' >> ",resFile),
    paste0("samtools flagstat ", ufBam, "| grep 'mapped [(]' | cut -d ' ' -f 1 >> ",resFile),
    paste0("echo 'uniqueAlign' >> ",resFile),
    paste0("samtools flagstat ", mdBam, "| grep 'total [(]' | cut -d ' ' -f 1 >> ",resFile),
    paste0("echo 'duplicates' >> ",resFile),
    paste0("samtools flagstat ", mdBam, "| grep 'duplicates' | cut -d ' ' -f 1 >> ",resFile),
    paste0("echo 'postdedup_unique_reads' >> ",resFile),
    paste0("samtools flagstat ", rdBam, "| grep 'total [(]' | cut -d ' ' -f 1 >> ",resFile),
    paste0("samtools view ", mdBam, " | awk 'and($2, 0x0002) && and ($2, 0x0040)' | cut -f 9 > ",
           outFile)
  )
  plib::runScript(jname=paste0("genAlignStats_",sid),jproj=projName,
                       jdesc=paste0("generate align stats for project ",projName," on sample ", sid),
                       jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}

