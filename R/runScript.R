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
#'
#' @export
runScript <- function(jname, jdesc,
                      jproj=.projName,
                      jscrp=.script., 
                      locroot=file.path(Sys.getenv("HOME")),
                      logdir = .wsLogs, scrpdir = .localScripts, remscrpdir = .wsScrpts,
                      nnodes=1, nproc=8, mem="8Gb",wtime="24:00:00",db=0,
                      xoverwrite=T,eviron=F,host="feenix", jobDep=NULL){

# Generate PBS script header
  header <- switch(host,
    dpsf = genSGEHead(jobName=jname, jobDescription=jdesc, Nnodes=nnodes, Nproc=nproc, Memory=mem,
                     logDir=logdir ),
    feenix = genPBSHead(jobName=jname, jobDescription=jdesc, Nnodes=nnodes, Nproc=nproc, Memory=mem,
                     Walltime=wtime, log.dir=logdir,environ=eviron,jobDep=jobDep )
  )
  qsubScript <- file.path(scrpdir, paste(jname, ".sh", sep=""))
  if(file.exists(qsubScript)){
    unlink(qsubScript)
  }
  con <- file(qsubScript)
  on.exit(close(con))
  writeLines(c(header,jscrp), con=con)
# Submit the script
  if(db<0){
    return(writeLines(c(header,jscrp)))
  }else{
    system(paste0("scp ",qsubScript," ",host,":",remscrpdir))
    paste0("qsub ",file.path(remscrpdir,basename(qsubScript)))
  }
}

