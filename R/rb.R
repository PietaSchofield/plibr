#' rebuild a package
#'
#' @param pkgname
#'
#' @param rstudio
#'
#' @export
rb <- function(pkgName="plib", baseDir="/Users/pschofield/Code"){
  curd <- getwd()
  setwd(file.path(baseDir,pkgName))
  devtools::document()
  devtools::load_all()
  devtools::install(reload=T)
  setwd(curd)
}
