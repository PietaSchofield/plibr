#' read in meme xml results file into an R list structure
#'
#' @param filename name of xml file
#'
#' @return returns a list with an array of statistic on each motif and an list of all the pwms
#'
#' @export
readMEME <- function( filename ) {
  file <- XML::xmlParse( file = filename, getDTD = FALSE )
  motifs <- list()
  motifs$motifs <- plyr::ldply(XML::xpathApply(file,"//motif",XML::xmlAttrs))
  motifs$scansitesum <- plyr::ldply(XML::xpathApply(file,"//scanned_sites",XML::xmlAttrs))

  motifs$pwm <- XML::xpathApply(file,"//probabilities",
          function(tb){
            tab <- t(plyr::ldply(XML::xpathApply(tb,".//alphabet_array",
                    function(yc){
                      XML::xpathSApply(yc, ".//value",XML::xmlValue)
                    })))
            rownames(tab) <- c("A","C","G","T")
            tab
          })

  motifs$scannedSites <- XML::xpathApply(file,"//scanned_sites",
           function(yc){
             ss <- as.data.frame(t(XML::xpathSApply(yc, ".//scanned_site",XML::xmlAttrs)),
                                  stringsAsFactors=F)
             df <- as.data.frame(table(ss$motif_id))
             colnames(df) <- c("motif_id","occurances")
             df[order(df$occurances),]
          })

  motifs$contSites <- XML::xpathApply(file,"//contributing_sites",
    function(cs){
        attr <- plyr::ldply(XML::xpathApply(cs,".//contributing_site",XML::xmlAttrs))
        lf <- XML::xpathSApply(cs,".//left_flank",XML::xmlValue)
        rf <- XML::xpathSApply(cs,".//right_flank",XML::xmlValue)
        site <- XML::xpathSApply(cs,".//contributing_site",
                function(css){
                  paste0(XML::xpathSApply(css, ".//letter_ref",XML::xmlAttrs),collapse="")
                })
        cbind(attr,left_flank=lf,site=site,right_flant=rf)
    })

  names(motifs$scannedSites) <- motifs$scansitesum$sequence_id
  names(motifs$contSites) <- motifs$motifs$id
  names(motifs$pwm) <- motifs$motifs$id

  motifs$motifs$consensus <- sapply(motifs$pwm,function(pm)
    paste(rownames(pm)[apply(pm,2,which.max)],collapse=''))
  motifs$motifs$regexp <- gsub("\n","",XML::xpathSApply(file,"//regular_expression",XML::xmlValue))
  return(motifs)
}



