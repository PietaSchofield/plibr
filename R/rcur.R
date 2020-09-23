#' render the current document
#'
#' renders the named document and puts the output html file in the correct location in my filespace
#' This currently assumes that I have mounted my troodon home directory to the local host via sshfs
#' which is available from OSXFUSE. Default paths are configured for my code locations.
#' 
#'
#' Been making changes to this to use bookdown document styles
#'
#' @param projDir project name
#' @param fileName file name
#' @param codeDir code tree path
#' @param sysRoot home directory
#' @param userid User id 
#' @param outRoot local temporary file creation location
#'
#' @export
rc <- function(fileName=.curFile,projName=.projName,gitRepo=.gitRepo,
               sysRoot=Sys.getenv("HOME"), user=Sys.getenv("USER"), 
               codeDir=file.path(sysRoot,"GitLab",gitRepo),hostname="dh174037.liv.ac.uk",
               outRoot=file.path(sysRoot,".tmp"),
               htmlRoot=file.path("/","var","www","html"),
               shinyRoot=file.path("/","opt","shiny-server","samples","sample-apps"),
               docRoot=file.path(sysRoot,"Projects",.projName),
               setHome=F, toPDF=F,toDOCX=F, toHTML=T,
               htmlUP=T, shinyUP=F,pdfUP=F,docUP=F,ext="Rmd"){
  if(!is.null(projName)){
    codePath <- file.path(codeDir,projName)
    htmlPath <- file.path(htmlRoot,gitRepo,projName)
    shinyPath <- file.path(shinyRoot,gitRepo,projName)
    outPath <- file.path(outRoot,projName)
  }else{
    codePath <- file.path(codeDir)
    outPath <- file.path(outRoot,gitRepo)
    shinyPath <- file.path(shinyRoot,gitRepo)
    htmlPath <- file.path(htmlRoot,gitRepo)
  }
  if(setHome){
    htmlFileName <- file.path(htmlPath,"index.html")
  }else{
    htmlFileName <- file.path(htmlPath,paste0(fileName,".html"))
    shinyFileName <- file.path(shinyPath,paste0(fileName,".",ext))
    docFileName <- file.path(docPath,fileName)
  }
  dir.create(outPath,showW=F,recur=T)
  infile <- file.path(codePath,paste0(fileName,".",ext))
  if(toDOCX){
    docxFile <- rmarkdown::render(input=infile,output_dir=outPath,
                               output_format="bookdown::word_document2")
    if(docUP){
       system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",docFileName,".docx")))
    }
  }
  if(toHTML){
    htmlFile <- rmarkdown::render(input=infile,output_dir=outPath,
                               output_format="bookdown::html_document2")
    if(htmlUP){
      system(paste0("scp ",htmlFile," ",paste0(user,"@",hostname,":",htmlFileName)))
    }
  }
  if(toPDF){
    pdfFile <- rmarkdown::render(input=infile,output_dir=outPath,
                                 output_format="bookdown::pdf_document2")
    if(pdfUP){
       system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",docFileName,".pdf")))
    }
  }
  if(shinyUP){
    system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",shinyFileName)))
  }

}
