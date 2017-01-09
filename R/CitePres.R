#' Presentation citation
#' 
#' A funtion to add citations in presentations without having to have a bibliography at the end
#'
#' @param biblio bibliography 
#' @param key bibtex citation key
#' @param alim maximum number of authors before et. al.
#' 
#' @export
CitePres <- function(bib=NULL,ckey="",alim=2){
  x <- bib[[ckey]]
  detail <- switch(x$bibtype,
    PhdThesis = paste0("Ph.D. Thesis, ",x$school),
    Article = paste0(x$shortjournal),
    paste0(x$bibtype," not coded yet")
  )
  year <- gsub("[-].*$","",x$date)
  if(length(x$author)>alim){
    txt <- paste0(.pname(x$author[[1]])," et. al. ",detail," ",year)
  }else{
    txt <- paste0(paste(lapply(x$author,.pname),collapse=" and ")," ",detail," ",year)
  }
  if("doi"%in%levels(x)[[1]]){
    url <- paste0("http://dx.doi.org/",x$doi)  
  }else if("url"%in%levels(x)[[1]]){
    url <- x$url  
  }else{
    url<-"http://www.compbio.dundee.ac.uk/user/pschofield"
  }
  paste0("([",txt,"](",url,"))")
}

.pname <- function(p,init=F){
  if(init){
    paste0(p$family," ",paste(lapply(p$given,substr,1,1),collapse="."),".")
  }else{
    p$family
  }
}