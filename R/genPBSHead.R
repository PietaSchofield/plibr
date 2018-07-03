#' Batch job script creation 
#'
#' Generate headers for running batch PBS jobs
#'
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
genPBSHead <- function(jobName="myrun", jobDescription="My analysis", 
  Nnodes=1, Nproc=16, Memory="32gb", Walltime="1:00:00",delayTil=NULL, environ=TRUE,
  email="pieta.schofield@manchester.ac.uk",overwrite=FALSE,emailMode="ae",
  log.dir="/data/compbio/pschofield/logs", dirSet=NULL,jobDep=NULL)
{
  header <- c(
    "#!/bin/sh ",
    paste('#', jobDescription, sep=" "),
    paste('#PBS -N', jobName, sep=" "),
    paste('#PBS -l nodes=', Nnodes, ':ppn=', Nproc, ',mem=', Memory, sep=""),
    paste('#PBS -l walltime=',Walltime,sep=""),
    paste('#PBS -m ',emailMode,sep=" "),
    paste('#PBS -M', email, sep=" "),
    paste('#PBS -o', log.dir, sep=" "),
    paste('#PBS -e', log.dir, sep=" "),
    '#PBS -j oe')
  if(environ){
    header <- append(header,'#PBS -V')
  }
  if(!is.null(delayTil)){
    header <- append(header,paste0('#PBS -a ',delayTil))
  }
  if(!is.null(jobDep)){
    header <- append(header,paste0('#PBS -W depend=afterok:',paste(jobDep,collapse=":")))
  }
  if(!is.null(dirSet)){
    header <- append(header," ")
    header <- append(header,paste0("cd ",dirSet))
  }
  header
}
