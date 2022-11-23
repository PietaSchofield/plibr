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
#' @param email address to notify of job progress
#' @param logDir directory output logs
#'
#' @export
genSlurmHead <- function(jobName="myrun", jobDescription="My analysis", 
  Nnodes=1, Nproc=16, Memory="32gb", Walltime="24:00:00",
  email="pieta.schofield@cruk.manchester.ac.uk",emailMode="END",
  logDir="/home/camp/schofip/logs",Partition="compute")
{
  header <- c(
    "#!/bin/sh ",
    paste0('# ', jobDescription),
    paste0('#SBATCH --job-name=', jobName),
    paste0('#SBATCH --nodes=', 1),
    paste0('#SBATCH --ntasks=', Nnodes),
    paste0('#SBATCH --cpus-per-task=',Nproc),
    paste0('#SBATCH --time=',Walltime),
    paste0('#SBATCH --mem=',Memory),
    paste0('#SBATCH --mail-user=', email),
    paste0('#SBATCH --partition=', Partition),
    paste0('#SBATCH --mail-type=', emailMode),
    paste0('#SBATCH --output=', file.path(logDir,paste0(jobName,"_%j.log"))),
    paste0('#')
  )
  header
}
