#' create a new notebook file
#'
#' @param name project name
#' @param root default project root directory
#'
#' @export
newNote <- function(name=NULL,root="/Users/pschofield/Code/"){
  projDir <- paste0("/Users/pschofield/Projects/",name)
  wwwDir <- paste0("/homes/pschofield/public_html/Projects/",name)
  fname<-paste0("note_",format(Sys.time(), "%Y_%m"))
  if(!file.exists(paste0(wwwDir,"/notebook"))){
    dir.create(paste0(wwwDir,"/notebook"),recursive=T)
  }
  if(!file.exists(paste0(projDir,"notebook"))){
    dir.create(paste0(projDir,"/notebook"),recursive=T)
  }
  system(paste0("cp /Users/pschofield/GitHub/PietaSchofield/motley/template.Rmd ",
                root,name,"/",fname,".Rmd"))
  file.edit(paste0(root,name,"/",fname,".Rmd"))
}
