#' get basic stats from fastQC html
#'
#' @param fileName html output file
#'
#' @export
getQC <- function(fileName,basicOnly=F){
  res <- XML::readHTMLTable(fileName,stringsAsFactors=F)
  bs <- res[[1]]$Value
  names(bs) <- res[[1]]$Measure
  if(basicOnly){
    bs
  }else{
    list(
      basic=bs,
      other=lapply(2:length(res),function(n) res[[n]])
    )
  }  
}
