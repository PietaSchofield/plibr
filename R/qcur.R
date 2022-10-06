#' render the current document
#'
#' This function has evolved way beyond the parameter list. It has also had to adapt from the original
#' dundee version to a version that worked with GoDaddy and then worked at Manchester CRUK and then cope
#' with the changes at CRUK and the whole reconfiguration due to the fire and then the  move to Liverpool
#' and the introduction of the virtual machine at Liverpook running a shiny server and the final dropping
#' of GoDaddy.
#'
#' @param projDir project name
#' @param fileName file name
#' @param codeDir code tree path
#' @param sysRoot home directory
#' @param userid User id
#' @param outRoot local temporary file creation location
#'
#' @export
qc <- function(fileName=.curFile,projName=.projName,gitRepo=.gitRepo,
               sysRoot=Sys.getenv("HOME"), user=Sys.getenv("USER"),
               codeDir=file.path(sysRoot,"GitLab",gitRepo),hostname="dh174037.liv.ac.uk",
               outRoot=file.path(sysRoot,".tmp"),
               htmlRoot=file.path("/","var","www","html"),
               shinyRoot=file.path("/","u1","shiny-server","samples","apps"),
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
      htmlPath <- file.path(htmlPath,projName)
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
  quartoFile <- quarto::quarto_render(input=infile,output_dir=outPath,
                                    output_format="all")
  if(toDOCX){
    if(docUP){
       system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",docFileName,".docx")))
    }
  }
  if(toHTML & !rmdUP){
    if(htmlUP){
      system(paste0("scp ",htmlFile," ",paste0(user,"@",hostname,":",htmlFileName)))
      if(file.exists(odPath)){
        system(paste0("cp ",htmlFile," ",odFileName))
      }
    }
  }
  if(toPDF){
    if(pdfUP){
       system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",docFileName,".pdf")))
    }
  }
  if(rmdUP){
    system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",shinyFileName)))
  }
}
