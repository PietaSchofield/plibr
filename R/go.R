#' edit most recent note
#'
#' @param pname project name
#' @param fname file name
#' @param tname tree name
#' @param rname root of tree
#' @param oflag open a file
#'
#' @export
go <- function(pname="notes",fname=NULL,tname="git_tree",rname="/Users/pschofield/",oflag=TRUE){
  dname <- paste0(rname,tname,"/",pname)
  setwd(dname)
  if(is.null(fname)){
    fnames <- list.files(".",pattern="^note.*[.]Rmd$")
    fname <- tail(fnames[order(fnames)],1)
  }
  if(oflag){
    file.edit(fname)
  }
}
