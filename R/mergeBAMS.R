#' merge some bamfiles
#'
#' @param sampleName stub of the sample name
#' @param bamDir the directory where the files are
#' @param outDir where to put the merged files
#' 
#' @export
mergeBAMS <- function(sampleName,bamDir,outDir,projName,noSub=F,ncores=16){
  inFiles <- plib::rcmd(paste0("ls ",file.path(bamDir,paste0(sampleName,"*.bam"))))
  script <- c(
    paste0("mkdir -p ",outDir),
    paste0("sambamba merge -t ",ncores," ",outDir,"/",sampleName,".bam ",
           paste0(inFiles,collapse=" "))
  )
  plib::runScript(jname=paste0("merge_",sampleName),jproj=projName,
                  jdesc=paste0("merge lanes for sample ",sampleName," for  ", projName),
                  jscrp=script,noSub=noSub,nproc=ncores)
}
