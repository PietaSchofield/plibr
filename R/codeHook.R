#' simple knitr hook for code chunk referencing
#'
#'
#' @export
codeHook <- function(before, options, envir) {
  if(before){
    sprintf('\\begin{rcode}') 
  }else{
    sprintf('\\caption{%s}\\label{%s}\\end{rcode}',
            options$code.cap,options$label)

  }
}
