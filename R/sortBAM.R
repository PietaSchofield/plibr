#' sort a bamfile
#'
#' @param inBAM name of input file
#' @param outBAM name of sorted file if NULL inBAM is overwritten
#'
#' @export
sortBAM <- function(inBAM,outSuf=NULL,projName,noSub=F){
  bamDIR <- dirname(inBAM)
  bamStub <- gsub("[.]bam","",basename(inBAM))
  unsortedBAM <- file.path(bamDIR,paste0(bamStub,"_unsorted.bam"))
  if(is.null(outSuf)){
    outBAM <- inBAM
    lastLine <- paste0("rm ",unsortedBAM) 
  } else {
    outBAM <- file.path(bamDIR,paste0(bamStub,"_",outSuf,".bam"))
    lastLine <- paste0("mv ",unsortedBAM," ",inBAM)
  }
  script <- c(
    paste0("mv ",inBAM," ",unsortedBAM),
    paste0("sambamba sort --nthreads ",ncores," -o ",outBAM," ",unsortedBAM),
    paste0("sambamba index ",outBAM),
    lastLine
  )
  runScript(jname=paste0("sort_",bamStub),jdesc="sort and index bamfile",jproj=projName,
           nproc=ncores,mem="32Gb",jscrp=script,noSub=noSub)
}

