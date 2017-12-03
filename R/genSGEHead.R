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
genSGEHead <- function(jobName="myrun", jobDescription="My analysis", Nnodes=1, Nproc=16,
                       Memory=NULL, email="pieta.schofield@manchester.ac.uk",
                       emailMode="ae",logDir="/scratch/log", pe="smp.pe")
{
  header <- c(
    "#!/bin/bash --login",
    "#$ -S /bin/bash",
    paste('#', jobDescription, sep=" "),
    paste('#$ -N', jobName, sep=" "),
    paste('#$ -m', emailMode,sep=" "),
    paste('#$ -M', email, sep=" "),
    paste('#$ -o', logDir, sep=" "),
    paste('#$ -e', logDir, sep=" "))
  if(!is.null(Memory)){
    header <- c(header, paste('#$ -l', Memory, sep=" "))
  }
  if(Nproc>1){
    header <- c(header, paste('#$ -pe', pe, Nproc,sep=" "))
  }
  header
}
