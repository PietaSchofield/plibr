#' create a new project
#'
#' @param name name of project
#' @param root root directory for project directory
#' @param noidx don't create a default index.Rmd file
#' @param noDir don't create an output directory
#' @param noWWW don't create a soft link under public_html on server
#'
#' @export
newProject <- function(name=NULL,root="/Users/pschofield/Code/",noidx=F,noDir=F,noWWW=F){
  dir.create(paste0(root,name))
  if(!noDir){
    dir.create(paste0("/Users/pschofield/Projects/",name,"/notebook"),recursive=T)
  }
  if(!noWWW){
    dir.create(paste0("/homes/pschofield/public_html/Projects/",name),recursive=F)
    dir.create(paste0("/homes/pschofield/public_html/Projects/",name,"/notebook"),recursive=F)
  }
  system(paste0("cp /Users/pschofield/git_hub/template.Rmd ",root,name,"/index.Rmd"))
  file.edit(paste0(root,name,"/index.Rmd"))
}
