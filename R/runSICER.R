#' run SICER on a set of file pairs
#'
#' @param projName name of the project
#' @param filePairs list of file pairs
#'
#' @export
runSICER <- function(projName,filePairs,bedDir,
                     pyMod="apps/python",
                     pyLib="libs/python2sitepkgs",
                     sicerPath="/scratch/pschofield/local/SICER",
                     outRoot="/scratch/pschofield/Projects",outPath="",
                     winsize=500,gapsize=4000,genome="GRCh37",fdr="1E-3",
                     insize=300,gencov=0.8){

  outDir <- file.path(outRoot,projName,outPath)
  lapply(names(filePairs),function(fpn){
    script <- c(
      paste0("module load ",pyMod),
      paste0("module load ",pyLib),
      paste0("mkdir -p ",file.path(outDir,fpn)),
      paste0("cd ",file.path(outDir,fpn)),
      paste0("sh ",file.path(sicerPath,"SICER.sh")," ",
             bedDir, " ", filePairs[[fpn]]["treat"], " ", filePairs[[fpn]]["control"]," ",
             file.path(outDir,fpn)," ", genome," 1 ",winsize," ",insize," ",
             gencov," ", gapsize," ",fdr)
    )
    CRUKlib::runScript(jname=paste0("SICER_",fpn),jproj=projName,jscrp=script,nproc=2,mem="16Gb",
              jdesc=paste0("Call peaks with SICER comparing ",
                           " treat = ",filePairs[[fpn]]["treat"], 
                           " control = ",filePairs[[fpn]]["control"]))
  })
}
