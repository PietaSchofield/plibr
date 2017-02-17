#' run JAFFA fusion gene detection
#'
#' @param sampleId id for naming the job script
#' @param dataDir location of the fastq files
#' @param filePattern file pattern to include all the relevant fastq files
#' @param projName name of the project
#' @param outDir full path to file output
#'
#' @export
runJAFFA <- function(sampleId, dataDir,filePattern, projName,outDir,
                     refGenome="hg19",refAnno="genCode19",readLayout="single",
                     javaMod="compilers/java/1.8.0_40/noarch",
                     velvetMod="apps/velvet/1.2.10/gcc-4.4.7",
                     oasesMod="apps/oases/0.2.08/gcc-4.4.7",
                     blatMod="apps/blat/35/gcc-4.4.7",
                     samtoolsMod="apps/samtools/0.1.19/gcc-4.4.7",
                     bowtie2Mod="apps/bowtie2/2.2.1/gcc/4.4.7",
                     rMod="apps/R/3.3.0/gcc-4.8.5",
                     jaffaPath="/scratch/pschofield/Projects/Sandbox/JAFFA-version-1.08",
                     ncores=2,mem="32Gb",noSub=F){
  script <- c(
    paste0("module load ",javaMod),
    paste0("module load ",samtoolsMod),
    paste0("module load ",bowtie2Mod),
    paste0("module load ",blatMod),
    paste0("module load ",velvetMod),
    paste0("module load ",oasesMod),
    paste0("module load ",rMod),
    paste0("mkdir -p ",outDir),
    paste0("cd ",outDir),
    paste0(file.path(jaffaPath,"tools/bin/bpipe"),
           " run -p genome=",refGenome," -p annotation=",refAnno , " -p readLayout=",readLayout,
           " ",file.path(jaffaPath,"JAFFA_assembly.groovy"),
           " ",file.path(dataDir,filePattern))
  )
  plib::runScript(jname=paste0("jaffa_",projName,"_",sampleId),jproj=projName,
                jdesc=paste0("run of JAFFA fusion transcript detection pipeline  "),
                jscrp=script,nproc=ncores,mem=mem,noSub=noSub)
}
