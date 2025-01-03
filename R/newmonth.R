#' make a new notebook for a project
#'
#' @export
newNote <- function(fileName,projName,rootDir=file.path(Sys.getenv("HOME"),"GitLab"),gitRepo="liverpool",
                    ext="Rmd",db=F, template='notepage',package='plibr',chkidx=T,ed=F){
  if(db){
    projName <- 'notes'
    fileName <- "202411"
    rootDir <- file.path(Sys.getenv("HOME"),"GitLab")
    gitRepo <- "liverpool"
    ext <- "Rmd"
    package <- 'plibr'
    template <- 'notepage'
    chkidx <- T
    ed <- F
  }
  outDir <- file.path(rootDir,gitRepo,projName)
  dir.create(outDir,showWarnings=F,recursive=T)
  idxfile <- file.path(outDir,"index.Rmd")
  newfile <- file.path(outDir,paste0(fileName,'.',ext))
  if(!file.exists(idxfile) && chkidx){
    rmarkdown::draft(file=idxfile,template="noteindex",package=package,edit=ed)
  }
  if(!file.exists(newfile)){
    rmarkdown::draft(file=newfile,template=template,package=package,edit=ed)
  }
  return(newfile)
}
