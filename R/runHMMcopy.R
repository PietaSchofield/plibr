#' Script for performing copy number analysis using HMMcopy
#'
#' adapted from a script created: 11 Aug 2016, LHS
#'
#' @export
runHMMcopy <- function( projName, pairings=NULL, wigDir,remRoot="/scratch/pschofield/Projects",
                        mapFile,gcFile, paramList, chrToPlot=c(1:22,"X","Y")){
  if(!is.null(pairings)){
    paired <- T
  }else{
    paired <- F
  }
  dir.create(paramList[["outDir"]],recursive=T,showWarning=FALSE)
  wigDir <- file.path(remRoot,projName,wigDir)
  wigFiles <- rlsFiles(file.path(wigDir,"*.*"))
  sampleFiles <- getFiles(filenames=basename(wigFiles),remDir=gsub("/scratch/","",wigDir))
  gcFile <- sampleFiles[grep(gcFile,sampleFiles)]
  mapFile <- sampleFiles[grep(mapFile,sampleFiles)]
  sampleFiles <- sampleFiles[!grepl(paste0("(",mapFile,"|",gcFile,")"),sampleFiles)]

  if(!paired)
  {
    ds <- lapply(sampleFiles,cnvCopyNumber, gcFile=gcFile, mapFile=mapFile, myChromosome=chrToPlot)
    names(ds) <- gsub(paste0("_w",paramList[["windowSize"]],".*$"),"",basename(sampleFiles))
    # Create a matrix containing log2-Ratio for each of the above tumour-normal pairs
    dsmat <- do.call('cbind', lapply(1:length(ds), function(n) return(ds[[n]]$copy) ))
    colnames(dsmat) <- names(ds)
    dsmat <- data.frame(chr=IRanges::space(ds[[1]]), start=start(ds[[1]]), end=end(ds[[1]]), dsmat)
  }else{
    ds <- lapply(split(pairings,seq(nrow(pairings))),function(pair){
      pid <- rownames(pair)
      tumourFile = sampleFiles[grep(pair[1],sampleFiles)]
      normalFile = sampleFiles[grep(pair[2],sampleFiles)]
      cnvLogRatio(tumourFile=tumourFile, normalFile=normalFile, gcFile=gcFile, mapFile=mapFile, 
                       myChromosome=chrToPlot)
    })
    names(ds) <- rownames(pairings) 
    # Create a matrix containing log2-Ratio for each of the above tumour-normal pairs
    dsmat <- do.call('cbind', lapply(1:length(ds), function(n) return(ds[[n]]$copy) ))
    colnames(dsmat) <- names(ds)
    dsmat <- data.frame(chr=IRanges::space(ds[[1]]), start=start(ds[[1]]), end=end(ds[[1]]), dsmat)
  }
  list(cnList=ds,cndf=dsmat)
}

