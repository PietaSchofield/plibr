#' codelab
#' 
#' Add code labels in Rmarkdown
#' @export
codelab <- function(before,options,envir){
  if(before){
    if(!grepl("unname", options$label)){
      sprintf('#### _%s_', options$label )
    }
  }
}
