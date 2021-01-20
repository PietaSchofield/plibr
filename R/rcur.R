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
               shinyRoot=file.path("/","u1","shiny-server","samples"),
               onedrive=file.path(sysRoot,"OneDrive","me","html"),
               docRoot=file.path(sysRoot,"Projects",projName),
               setHome=F, toPDF=F,toDOCX=F, toHTML=T,setRepo=T,setProj=T,toShiny=F,
               htmlUP=T, rmdUP=F,pdfUP=F,docUP=F,ext="Rmd",dbg=F){
  codePath <- file.path(codeDir)
  outPath <- file.path(outRoot,gitRepo)
  docPath <- file.path(docRoot)
  if(toShiny){
    htmlPath <- file.path(shinyRoot)
  }else{
    htmlPath <- file.path(htmlRoot)
  }
  odPath <- file.path(onedrive)
  if(setRepo){
    htmlPath <- file.path(htmlPath,gitRepo)
    docPath <- file.path(docPath,gitRepo)
    odPath <- file.path(odPath,gitRepo)
  }
  if(setProj){
    codePath <- file.path(codePath,projName)
    docPath <- file.path(docPath,projName)
    if(toShiny){
      htmlPath <- file.path(htmlPath,"apps",projName)
    }else{
      htmlPath <- file.path(htmlPath,projName)
    }
    outPath <- file.path(outPath,projName)
    odPath <- file.path(odPath,projName)
  }
  if(setHome){
    htmlFileName <- file.path(htmlPath,"index.html")
    odFileName <- file.path(odPath,"index.html")
  }else{
    htmlFileName <- file.path(htmlPath,paste0(fileName,".html"))
    odFileName <- file.path(odPath,paste0(fileName,".html"))
    if(rmdUP){
      shinyFileName <- file.path(htmlPath,paste0(fileName,".Rmd"))
    }else{
      shinyFileName <- file.path(htmlPath,paste0(fileName,".html"))
    }
    docFileName <- file.path(docPath,fileName)
  }
  if(dbg){
    return(outPath)
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
  if(toHTML & !rmdUP){
    htmlFile <- rmarkdown::render(input=infile,output_dir=outPath,
                               output_format="bookdown::html_document2")
    if(htmlUP){
      system(paste0("scp ",htmlFile," ",paste0(user,"@",hostname,":",htmlFileName)))
      if(file.exists(odPath)){
        system(paste0("cp ",htmlFile," ",odFileName))
      }
    }
  }
  if(toPDF){
    pdfFile <- rmarkdown::render(input=infile,output_dir=outPath,
                                 output_format="bookdown::pdf_document2")
    if(pdfUP){
       system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",docFileName,".pdf")))
    }
  }
  if(rmdUP){
    system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",shinyFileName)))
  }
}
