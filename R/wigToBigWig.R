#' create strand specific bigwigs from a standed bamfile
#'
#' @param inFile input bam file
#' @param outDir output directory
#' @param projName name of the project
#' 
#' @export
sswigToBigWig <- function(inFile,samFile, outDir,projName,
                       outRoot=file.path("/scratch/pschofield/Projects", projName),
                       ucscMod="apps/ucscsuite/20150630/linux-x86_64",
                       samtoolsMod="apps/samtools/1.3.1/gcc-4.4.7",noSub=F){
  chrSize <- file.path(outRoot,"tmp",gsub("[.]wig","_chr.size",basename(inFile)))
  outFile <- file.path(outRoot,outDir,gsub("[.]wig",".bw",tolower(basename(inFile))))
  outFile <- gsub("[.]signal[.]uniquem","m",outFile)
  outFile <- gsub("[.]signal[.]unique","u",outFile)
  sampleId <- gsub("[.]out.*$","",basename(inFile))
  script <- c(
    paste0("module load ",ucscMod),
    paste0("module load ",samtoolsMod),
    paste0("mkdir -p ",file.path(outRoot,outDir)),
    paste0("samtools view -H ", samFile," | grep ^@SQ | sed -e 's/^.*SN://g' | sed -e 's/LN://g'",
           " > ",chrSize), 
    paste0("wigToBigWig ",inFile," ",chrSize," ",outFile)
  )
  plib::runScript(jproj=projName,jname=paste0("w2bw_",sampleId),
                     jdesc="convert wig to bigwigs",
                     nproc=1,mem="48Gb",jscrp=script,noSub=noSub)
}
