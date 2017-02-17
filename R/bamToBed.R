#' create bed file from a bam file
#'
#' @param inFile input bam file
#' @param outDir output directory
#' @param projName name of the project
#' 
#' @export
bamToBed <- function(inFile, outDir,projName,paired="",
                       outRoot=file.path("/scratch/pschofield/Projects", projName),
                       bedtoolsMod="apps/bedtools/2.25.0/gcc-4.4.7"){
  outBed <- file.path(outDir,gsub("[.]bam$",".bed",basename(inFile)))
  script <- c(
    paste0("module load ",bedtoolsMod),
    paste0("mkdir -p ",outDir),
    paste0("bedtools bamtobed -cigar ",paired," -i ", inFile, " > ", outBed)
  )
  plib::runScript(jproj=projName,jname=paste0("bamToBed_",basename(inFile)),
                     jdesc="bam to bed conversion", nproc=1,mem="16Gb",jscrp=script)
}
