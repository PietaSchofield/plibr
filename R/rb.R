#' rebuild a package
#'
#' @param pkgname
#'
#' @param rstudio
#'
#' @export
rb <- function(wd=NULL,projName="plib_ps", pkgName="package", 
               baseDir=file.path(Sys.getenv("HOME"),"Projects")){
  curd <- getwd()
  if(is.null(wd)){
    if(projName!="plib_ps"){
      wd <- file.path(baseDir,projName,pkgName)
    }else{
      wd <- file.path(baseDir,projName)
    }
  }
  print(wd)
  setwd(wd)
  devtools::document()
  devtools::load_all()
  devtools::install(reload=T)
  setwd(curd)
}
