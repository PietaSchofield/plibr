#' render the current document
#'
#' renders the named document and puts the output html file in the correct location in my filespace
#' This currently assumes that I have mounted my troodon home directory to the local host via sshfs
#' which is available from OSXFUSE. Default paths are configured for my code locations.
#'
#' Been making changes to this to use bookdown document styles
#'
#' @param projDir project name
#' @param fileName file name
#' @param codeDir code tree path
#' @param sysRoot home directory
#' @param userid User id 
#' @param topDir output home directory on god
#' @param outRoot local temporary file creation location
#'
#' @export
rc <- function(fileName=.curFile,projName=.projName,gitRepo=.gitRepo,
               sysRoot=Sys.getenv("HOME"), userid=Sys.getenv("USER"), 
               codeDir=file.path(sysRoot,"GitLab",gitRepo),
               godRoot="public_html", outRoot=file.path(sysRoot,".tmp"),
               livRoot=file.path("/","var","www","html"),
               setHome=F, toPDF=F,toDOCX=F, toHTML=T,
               godUP=T, livUP=FALSE,over=T){
  if(!is.null(projName)){
    codePath <- file.path(codeDir,projName)
    godPath <- file.path(godRoot,gitRepo,projName)
    livPath <- file.path(livRoot,gitRepo,projName)
    outPath <- file.path(outRoot,projName)
  }else{
    codePath <- file.path(codeDir)
    outPath <- file.path(outRoot,gitRepo)
    godPath <- file.path(godRoot,gitRepo)
    livPath <- file.path(livRoot,gitRepo)
  }
  if(setHome){
    godPath <- godRoot 
    livPath <- livRoot
    godFile <- file.path(godPath,"index.html")
    livFile <- file.path(livPath,"index.html")
  }else{
    godFile <- file.path(godPath,paste0(fileName,".html"))
    livFile <- file.path(livPath,paste0(fileName,".html"))
  }
  dir.create(outPath,showW=F,recur=T)
  infile <- file.path(codePath,paste0(fileName,".Rmd"))
  if(toDOCX){
    docxFile <- rmarkdown::render(input=infile,output_dir=outPath,
                               output_format="bookdown::word_document2")
  }
  if(toHTML){
    htmlFile <- rmarkdown::render(input=infile,output_dir=outPath,
                               output_format="bookdown::html_document2")
  }
  if(toPDF){
    pdfFile <- rmarkdown::render(input=infile,output_dir=outPath,
                                 output_format="bookdown::pdf_document2")
  }
  if(godUP){
    system(paste0("scp ",htmlFile," ",paste0("pieta@pieta.me:",godFile)))
  }
  if(livUP){
    dir.create(livPath,showW=F,recur=T)
    file.copy(htmlFile,livFile,overwrite=over)
  }
}
