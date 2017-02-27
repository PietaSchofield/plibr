#' submit a MACS2 job on the the cluster
#'
#' @param trBam treatment bam file
#' @param ctBam control bam file
#' @param outDir output directory
#' @param name output name
#' @param broad call broad peaks
#'
#' @export
runMACS2 <- function(trBam,ctBam,outDir,name,broad=T,pe=F,projName,genome="hs",bcutoff=0.01,
                     pyMod="apps/python",pyLib="libs/python2sitepkgs",noMod=F,
                     remRoot=file.path("/scratch/pschofield/Projects",projName)){
  if(noMod){
    nm <- "--nomodel"
  }else{
    nm <- ""
  }
  if(broad){
    bline <- paste0(" ",nm," --broad --broad-cutoff ",bcutoff)
  } else {
    bline <- paste0(" ",nm," -q ",bcutoff)
  }
  if(pe){
    peline <- " -f BAMPE "
  }else{
    peline <- " -f BAM "
  }
  script <- c(
    paste0("module load ",pyMod),
    paste0("module load ",pyLib),
    paste0("mkdir -p ",outDir),
    paste0("macs2 callpeak ",
           " -t ", trBam,
           " -c ", ctBam,
           peline,
           " -g ",genome,
           " --outdir ", file.path(remRoot,outDir),
           " --name ", name ," ",
           bline )
    )
    jn <- paste0("macs2_",name,"_",projName)
    jd <- paste0("macs2 ",name," ",projName," ")
    plib::runScript(jname=jn,jproj=projName,jscrp=script, jdesc=jd,nproc=2)
}
