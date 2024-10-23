#' rebuild a package
#'
#' @param pkgname
#'
#' @param rstudio
#'
#' @export
rb <- function(wd=NULL,projName="plibr", pkgName="package",install=F,
               baseDir=file.path(Sys.getenv("HOME"),"GitLab")){
  curd <- getwd()
  if(is.null(wd)){
    if(projName!="plibr"){
      wd <- file.path(baseDir,projName,pkgName)
    }else{
      wd <- file.path(baseDir,projName)
    }
  }
  print(wd)
  setwd(wd)
  devtools::document()
  devtools::load_all()
  if(install) devtools::install(reload=T)
  setwd(curd)
}
