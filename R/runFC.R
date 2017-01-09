#' Run featureCounts on a set of bam files
#'
#' @param projName project name
#' @param bamDir location of bamfiles (actually directory where counting takes place)
#' @param fileList list of files if not full paths will have to be in bamDir but can be fully qualified
#' paths
#' @param outFile name of output file
#' @param ncores number of cores to use
#' @param annoFile the gtf annotation file to count against must of course match the alignment reference
#' @param switches extra switches for featureCounts for example paired end or exon counting 
#'
#' @export 
runFC <- function(projName,bamDir,fileList,outFile,ncores=16,
              subreadMod="apps/subread/1.5.0-p3/gcc-4.4.7",
              refDir="/scratch/pschofield/ref/star/GRCh37_pa",
              annoFile=file.path(refDir,"Homo_sapiens.GRCh37.85.gtf"),
              genomeFile=file.path(refDir,"Homo_sapiens.GRCh37.dna_sm.primary_assembly.fa"),
              switches= " -M -O -C -P -p -t exon -g transcript_id -f -d 50"){

  projDir <- file.path("/scratch/pschofield/Projects",projName)
  outFile <- file.path(projDir,outFile)
  bamDir <- file.path(projDir,bamDir)
  script <- c(
    paste0("mkdir -p ",dirname(outFile)),
    paste0("cd ",bamDir),
    paste0("module load ",subreadMod),
    paste0("featureCounts",
           " -a ", annoFile,
           " -o ", outFile,
           " -T ",ncores,
           " -G ",genomeFile,
           switches,
           " ",paste(basename(fileList),collapse=" ")
          )
    )
    jn <- paste0("featureCount_",projName)
    jd <- paste0("featureCount  ",projName," ",outFile)
    CRUKlib::runScript(jname=jn,jproj=projName,jscrp=script, jdesc=jd,nproc=ncores)
}
