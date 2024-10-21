#' render the current document
#'
#' Latest changes are to stop pushing files up to an html server and to keep them local by default 
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
displaycurrent <- function(fileName=.fileName,
               projName=.projName,
               gitRepo="liverpool",
               sysRoot=Sys.getenv("HOME"),
               user=Sys.getenv("USER"), 
               outPath=file.path(Sys.getenv("HOME"),"Notes"),
               quartoPath=NULL, 
               nbPath=file.path("/srv","http"),
               default_browser="nyxt",
               codePath=file.path(sysRoot,"GitLab",gitRepo),
               docPath=file.path(sysRoot,"Projects"),
               silent=F,setHome=F,toPDF=F,toDOCX=F,toHTML=T,nomove=F,
               htmlUP=T, pdfUP=F,docUP=F,ext="Rmd",dbg=F,quarto=NULL,quartoUP=F){
 
  if(gitRepo=="liverpool"){
    nbPath <- file.path(nbPath,"uol")
    outPath <- file.path(outPath,"uol")
  }else{
    nbPath <- file.path(nbPath,gitRepo)
    outPath <- file.path(outPath,gitRepo)
  }

  if(!file.exists(nbPath)){
    htmlUP <- F
  }else{
    outPath <- nbPath
  }

  if(!is.null(projName)){
    codePath <- file.path(codePath,projName)
    nbPath <- file.path(nbPath,projName)
    outPath <- file.path(outPath,projName)
    docPath <- file.path(docPath,projName)
  }

  if(is.null(quartoPath)) quartoPath=outPath

  if(setHome){
    nbFileName <- file.path(nbPath,"index.html")
  }else{
    nbFileName <- file.path(nbPath,paste0(fileName,".html"))
    docFileName <- file.path(docPath,paste0(fileName,".docx"))
    pdfFileName <- file.path(docPath,paste0(fileName,".pdf"))
  }

  dir.create(dirname(nbFileName),showW=F,recur=T)
  dir.create(dirname(docFileName),showW=F,recur=T)
  infile <- file.path(codePath,paste0(fileName,".",ext))

  htmlFile <- file.path(nbFileName)

  if(!silent){
    ofile <- htmlFile

    if(RCurl::url.exists("http://localhost")){
      ofile <- gsub("/srv/http/","http://localhost/",ofile)
      displayURL(ofile)
    }else{
      if(!is.null(lfile)){
       displayURL(lfile)       
      }
    }
  }
}
