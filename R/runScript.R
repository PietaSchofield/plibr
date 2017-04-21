#' generate and submit a batch job
#'
#' @param jname job name
#' @param jproj project name
#' @param jdesc job description
#' @param jscrp job script
#' @param scrpdir scripts directory
#' @param logdir log directory
#' @param locroot local root
#' @param remroot remote root
#' @param nproc number of processor requested
#' @param nnodes number of nodes requested
#' @param mem memory size requested
#' @param wtime wall time requested
#' @param overwrite overwrite existing script file if it exists
#' @param environ take environment variables of submitting process (not relevant for remote jobs)
#' @param noSub return script rather than running it for debug purposes
#'
#' @export
runScript <- function(jname, jproj, jdesc, jscrp,
                      remroot="/scratch/pschofield",
                      locroot=file.path(Sys.getenv("HOME")),
                      logdir = file.path(remroot,"Projects",jproj,"log"),
                      scrpdir = file.path(locroot,"Projects",jproj,"Scripts"),
                      nnodes=1, nproc=8, mem="8Gb",wtime="24:00:00",
                      overwrite=T,eviron=F,noSub=F,scpIt=T){
  qsubScript <- file.path(scrpdir, paste(jname, ".sh", sep=""))
  jobName <- paste(jname,jdesc, sep="_")

# Generate PBS script header
  header <- genJobScript(jobName=jname,
                        jobDescription=jdesc, Nnodes=nnodes, Nproc=nproc, Memory=mem, 
                        Walltime=wtime, log.dir=logdir,environ=eviron )
  if(file.exists(qsubScript)){
    if(!overwrite){
      stop(paste0("Script ",qsubScript," already exists delete before running again "))
    }else{
      unlink(qsubScript)
    }
  }
  con <- file(qsubScript)
  on.exit(close(con))
  writeLines(c(header,jscrp), con=con)
# Submit the script
  subJob(qsubScript,noSub=noSub,scpIt=scpIt)
}

