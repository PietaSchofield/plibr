#' segment as copy number data set
#'
#' @param ds copynumber dataset
#'
#' @export
cnvSegment <- function(ds,pl,mkwigs=T){
  if(mkwigs){
    outDir <- file.path(locRoot,projName,pl[["outDir"]],"wigs")
    dir.create(outDir, recursive=T,showWarnings=F)
  }
  segData <- lapply(names(ds),function(n){
               if(mkwigs){
                 outFile <- file.path(outDir,paste0("Segs_",n,".bw"))
               }else{
                 outFile <- NULL
               }
               plib::custHMMsegment(ds[[n]],pl, outFil=outFile)
              })
  names(segData) <- names(ds)
  segData
}

