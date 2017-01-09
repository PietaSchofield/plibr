#' generate and submit a batch job that runs an Rjob
#'
#' @param sname name of the script file
#' @param args any additional arguments
#' @param rMod which version of R to run
#' @param jname job name
#' @param jproj project name
#' @param jdesc job description
#' @param logdir log directory
#' @param locroot local root
#' @param remroot remote root
#'
#' @export
runRScript <- function(rscript,args=NULL,jname, jproj, jdesc, 
                      remRoot="/scratch/pschofield/Projects",
                      locRoot="/Users/pschofield/Projects",
                      ncores=1, nproc=8, mem="16Gb",wtime="30:00:00",
                      overwrite=T,eviron=F, rMod = "apps/R/3.3.0/gcc-4.8.5"){
  locRfile <- file.path(locRoot,jproj,"Scripts",paste0(jname,".R"))
  remRfile <- file.path(remRoot,jproj,"Scripts",paste0(jname,".R"))
  rFile <- file(file.path(locRoot,jproj,"Scripts",paste0(jname,".R")))
  writeLines(rscript,con=rFile)
  close(rFile)
  system(paste0("scp ",locRfile," troodon:",remRfile))
  script <- c(
    paste0("cd /scratch/pschofield"),
    paste0("module load ",rMod),
    paste0('Rscript ',remRfile," ")
  )
  if(!is.null(args)){
    script <- append(script,paste(args,sep=" "))
  }
  runScript(jname=jname,jproj=jproj,jdesc=jdesc,jscrp=script,nproc=ncores,mem=mem)
}
