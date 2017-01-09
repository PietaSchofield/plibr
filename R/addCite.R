#' include citation for package in markdown
#'
#' @param bib bibliography for RefManageR otherwise put bibtex citation in
#' @param packages vector of package names
#' @param type T for in text A for other
#'
#' @export
addCite <- function(bib=NULL,packages=NULL,type="T"){
  if(!is.null(bib)){
    switch(type,
      "T"=TextCite(bib,lck(packages=packages)),
      "A"=AutoCite(bib,lck(packages=packages))
    )
  }else{
    switch(type,
      "T"= paste0("\\citet{",paste(lck(packages),collapse=","),"}"),
      "A"= paste0("\\citep{",paste(lck(packages),collapse=","),"}")
    )
  }
}
