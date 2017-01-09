#' Read a Prism file.
#'
#' This function is in no way a generic Prism reader it is specifically designed
#' to read qRT_PCR results files from A Seifert 
#'
#' @param filename the name of the prism file
#'
#' @export
readPrism <- function(filename){
  # unfortunately the prism files are window so need the linefeed stripping
  # can use sed for this
  system(paste0("sed -i 1 's/\r//g' ",filename))
  # then parse the xml
  xData <- XML::xmlParse(filename)
  # extract all the data
  tabn <- unlist(XML::xpathApply(xData,"//Table",
            function(tb){
              unlist(XML::xpathApply(tb,"./Title",XML::xmlValue))
            }))

  tabs <- XML::xpathApply(xData,"//Table",
            function(tb){
              columns <- plyr::ldply(XML::xpathApply(tb,".//YColumn",
                          function(yc){
                            unlist(XML::xpathApply(yc, ".//Subcolumn",
                              function(cc){ 
                                XML::xpathApply(cc,"./d",XML::xmlValue)
                              }))
                          }))
              # get the rownames
              rowT <- plyr::ldply(XML::xpathApply(tb,".//YColumn",
                        function(yc){
                          unlist(XML::xpathApply(yc, ".//Title",XML::xmlValue))
                        }))
              # get the column names
              colT <- plyr::ldply(XML::xpathApply(tb,".//RowTitlesColumn",
                        function(yc){
                          unlist(XML::xpathApply(yc, ".//Subcolumn",
                            function(cc){ 
                            XML::xpathApply(cc,"./d",XML::xmlValue)
                          }))
                        }))
              rowT
              # set the column names 
              colnames(columns) <- rep(colT[1,],ncol(columns)/ncol(colT))
              rownames(columns) <- rowT[,1]
              # turn it into longform nice for ggplot
              mData <- reshape2::melt(as.matrix(columns))
              mData$value <- as.numeric(as.character(mData$value))
              mData
            })
  names(tabs) <- tabn
  ret <- ldply(tabs)
  colnames(ret)[1] <- "Table"
  ret
}
