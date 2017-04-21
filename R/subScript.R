#' subScript will submit a script to the cluster
#'
#' calls subJob to submit a script to the cluster as a temporary file it relies on the temporary
#' directory where the temporary file will be written to being mounted to the local machine.
#' I could get round this by writing it locally and then copying it with scp but at the moment
#' this is not worth the effort.
#'
#' @param scriptstub the stub for the temporary file name
#' @param script the content of the script as a vector of strings
#' @param tmpdir location for the temporary file
#' @param scriptext extention for the temporary file
#' @param logdir location for the batch job logs
#' @param cores number of cores
#' @param nosub boolean to switch off actual submission but to create the tempfiles
#' @param queue queue name
#' @param email job status email settings
#' @param emailAddress where to send job status emails
#' @param args any arguments to add to the job submission string
#'
#' @export
subScript <- function(scriptstub="pieta_qsub",script=c("#!/bin/bash","hostname"),scriptext=".sh",
                      tmpdir="/homes/pschofield/tmp/",logdir="",cores=8,nosub=FALSE,email="as",
                      queue=NULL,emailAddress="p.schofield@dundee.ac.uk",args=NULL,ramSize="4G",
                      nNodes=1)
{
  batchJob <- tempfile(pattern=paste0(scriptstub,"_"),tmpdir=tmpdir,fileext=scriptext)
  filecon <- file(batchJob)
  writeLines(script, filecon)
  close(filecon)
  if(!nosub) subJob(scriptfile=batchJob,logdir=logdir,mcCores=cores,email=email,args=args,
                    queue=queue,ramSize = ramSize)

}


