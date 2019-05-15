#' create a new notebook file
#'
#' @param name project name
#' @param root default project root directory
#'
#' @export
newFile <- function(project=NULL,fname=NULL,root="/Users/pschofield/git_tree/",
                    template="/Users/pschofield/git_hub/template.Rmd"){
  system(paste0("cp ",template," ",root,project,"/",fname,".Rmd"))
  file.edit(paste0(root,project,"/",fname,".Rmd"))
}
