#' submit a MACS2 job on the the cluster
#'
#' @param trBam treatment bam file
#' @param ctBam control bam file
#' @param outDir output directory
#' @param name output name
#' @param broad call broad peaks
#'
#' @export
runMACS2 <- function(trBam,ctBam,outDir,name,broad=T,pe=F,projName,
                     pyMod="apps/python"){
  if(broad){
    bline <- "--nomodel --broad"
  } else {
    bline <- " "
  }
  if(pe){
    peline <- " -f BAMPE "
  }else{
    peline <- " -f BAM "
  }
  script <- c(
    paste0("module load ",pyMod),
    paste0("mkdir -p ",outDir),
    paste0("macs2 callpeak ",
           " -t ", trBam,
           " -c ", ctBam,
           peline,
           " -g hs ",
           " --outdir ", outDir,
           " --name ", name ,
           bline )
    )
    jn <- paste0("macs2_",name,"_",projName)
    jd <- paste0("macs2 ",name," ",projName," ")
    plib::runScript(jname=jn,jproj=projName,jscrp=script, jdesc=jd,nproc=2)
}
