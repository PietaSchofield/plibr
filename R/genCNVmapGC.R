#' generate wiggle tracks for HMMcopy 
#'
#' @param sampleID the bam files
#' @param projName the project
#' @param location of bam files
#'
#' @export
genCNVmapGC <- function(projName,remRoot="/scratch/pschofield/Projects",
                        outPath="Analysis/wigs", noSub=F,ncores=1,scpIt=T,mem="16Gb",windowSize,
                        chrNames=c(seq(1,22),"X","Y"),genomeFasta, mapFile,
                        hmmCopyMod= "apps/hmmcopy/0.99.0/gcc-4.4.7"){
  outDir=file.path(remRoot,projName,outPath)
  script <- c(
    paste0("mkdir -p ",outDir),
    paste0("module load ",hmmCopyMod),
    paste0("gcCounter -w ",windowSize," -c ",paste0(chrNames,collapse=",")," ", genomeFasta, 
      " > ",file.path(outDir,paste0("/GC_w",windowSize,".wig"))),
    paste0("mapCounter -w ",windowSize, " -c ", paste0("chr",chrNames,collapse=",")," ", mapFile,
       " > ", file.path(outDir,paste0("DukeMappability35mer_w",windowSize,".wig")))
  )
  plib::runScript(jname=paste0("gengcmap_"),jproj=projName,
                    jdesc=paste0("generate HMMCopy wig for project ",projName ),
                    jscrp=script,noSub=noSub,nproc=ncores,scpIt=scpIt,mem="32Gb")
}
