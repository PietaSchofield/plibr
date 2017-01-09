#' Some R functions for processing and analysing of genomics data
#'
#' Generate headers for running batch PBS jobs
#'
#' @param PBSscript name of the script file
#' @param jobName name of job
#' @param jobDescription description of job
#' @param Nnodes number of nodes requested
#' @param Nproc number of cores requested
#' @param Memory size of memory requested
#' @param Walltime limit to run time
#' @param delayTil time delay for start
#' @param environ pass current environment variables
#' @param email address to notify of job progress
#' @param overwrite boolean overwrite existing scripts
#' @param log.dir directory output logs
#'
#' @export
genPBSScript <- function(PBSscript, jobName="myrun", jobDescription="My analysis", 
  Nnodes=1, Nproc=16, Memory="32gb", Walltime="30:00:00",delayTil=NULL, environ=TRUE,
  email="pieta.schofield@cruk.manchester.ac.uk",overwrite=FALSE,
  log.dir="/lustre/scratch/pschofield/tmp")
{
  header <- c(
    paste0('# ', file=PBSscript),
    paste0('# ', jobDescription),
    paste0('#PBS -N ', jobName),
    paste0('#PBS -l nodes=', Nnodes, ':ppn=', Nproc, ',mem=', Memory),
    paste0('#PBS -m a'),
    paste0('#PBS -M ', email),
    paste0('#PBS -o ', log.dir),
    paste0('#PBS -e ', log.dir),
    paste0('#PBS -j oe')
  )
  if(environ){
    header <- c(header,'#PBS -V')  
  }
  if(!is.null(delayTil)){
    header <- c(header, paste0('#PBS -a ',delayTil))
  }
  return(header)
}

