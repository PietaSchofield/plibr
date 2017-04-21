#' merge some bamfiles
#'
#' @param sampleName stub of the sample name
#' @param bamDir the directory where the files are
#' @param outDir where to put the merged files
#' 
#' @export
mergeBAMS <- function(sampleName,inFiles=NULL,bamDir=NULL,projName,
                      remRoot="/scratch/pschofield/Projects",bamExt="*.bam",
                      outDir="Data/alignments/merge",noSub=F,ncores=16){
  outPath <- file.path(remRoot,projName,outDir)
  if(is.null(inFiles)){
    inFiles <- plib::rcmd(paste0("ls ",file.path(bamDir,paste0(sampleName,bamExt))))
  }
  script <- c(
    paste0("mkdir -p ",outPath),
    paste0("sambamba merge -t ",ncores," ",outPath,"/",sampleName,".bam ",
           paste0(inFiles,collapse=" "))
  )
  plib::runScript(jname=paste0("merge_",sampleName),jproj=projName,
                  jdesc=paste0("merge lanes for sample ",sampleName," for  ", projName),
                  jscrp=script,noSub=noSub,nproc=ncores)
}
