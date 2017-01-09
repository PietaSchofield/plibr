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
generateHeaders <- function(PBSscript, jobName="myrun", jobDescription="My analysis", 
  Nnodes=1, Nproc=16, Memory="32gb", Walltime="30:00:00",delayTil=NULL, environ=TRUE,
  email="pieta.schofield@cruk.manchester.ac.uk",overwrite=FALSE,
  log.dir="/lustre/scratch/pschofield/tmp")
{
  if(file.exists(PBSscript)){
    if(!overwrite){
      stop(paste0("Script ",PBSscript," already exists delete before running again "))
    }else{
      unlink(PBSscript)
    }
  }
  write('#', file=PBSscript)
  write(paste('#', jobDescription, sep=" "), file=PBSscript, append=TRUE)
  write('#', file=PBSscript, append=TRUE)
  write(paste('#PBS -N', jobName, sep=" "), file=PBSscript, append=TRUE)
  write('#', file=PBSscript, append=TRUE)
  write(paste('#PBS -l nodes=', Nnodes, ':ppn=', Nproc, ',mem=', Memory, sep=""), file=PBSscript, append=TRUE)
  write('#', file=PBSscript, append=TRUE)
  write('#PBS -m a', file=PBSscript, append=TRUE)
  write(paste('#PBS -M', email, sep=" "), file=PBSscript, append=TRUE)
  write('#', file=PBSscript, append=TRUE)
  write(paste('#PBS -o', log.dir, sep=" "), file=PBSscript, append=TRUE)
  write(paste('#PBS -e', log.dir, sep=" "), file=PBSscript, append=TRUE)
  write('#PBS -j oe', file=PBSscript, append=TRUE)
  if(environ){
    write('#', file=PBSscript, append=TRUE)
    write('#PBS -V', file=PBSscript, append=TRUE)  
  }
  if(!is.null(delayTil)){
    write('#', file=PBSscript, append=TRUE)
    write(paste0('#PBS -a ',delayTil), file=PBSscript, append=TRUE)  
  }
}

