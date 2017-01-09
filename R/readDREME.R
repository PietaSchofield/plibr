#' read in dreme results file
#'
#' @param filename name of xml file
#'
#' @return returns a list with an array of statistic on each motif and an list of all the pwms
#'
#' @export
readDREME <- function( filename ) {
  file <- XML::xmlParse( file = filename, getDTD = FALSE )
  mtfs <- XML::getNodeSet(file,"//motif")
  attrs <- plyr::ldply(XML::xpathApply(file,"//motif",XML::xmlAttrs))
  pwm <- lapply(mtfs, function(mf) t(plyr::ldply(XML::xpathApply(mf,"pos",XML::xmlAttrs))))
  motifs <- list()
  motifs$array <- attrs
  motifs$pwm <- pwm
  return(motifs)
}
