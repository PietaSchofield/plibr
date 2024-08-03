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
compilecurrent <- function(fileName=.fileName,
               projName=.projName,
               gitRepo="liverpool",
               sysRoot=Sys.getenv("HOME"),
               user=Sys.getenv("USER"), 
               outPath=file.path(Sys.getenv("HOME"),"Notes"),
               quartoPath=NULL, 
               nbPath=file.path("/srv","http"),
               codePath=file.path(sysRoot,"GitLab",gitRepo),
               docPath=file.path(sysRoot,"Projects"),
               silent=F,setHome=F,toPDF=F,toDOCX=F,toHTML=T,setProj=T,
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

  if(setProj){
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

  dir.create(outPath,showW=F,recur=T)
  infile <- file.path(codePath,paste0(fileName,".",ext))
  
  if(!is.null(quarto)){
    quartoFile <- paste0(xfun::sans_ext(infile),".",quarto)
    if(dbg) print(quartoFile)
    quarto::quarto_render(input=infile,execute_dir=outPath)
    if(file.exists(quartoFile)){
      if(quartoUP){
        quartoFileName <- file.path(nbPath,paste0(fileName,".",quarto))
        fs::file_copy(quartoFile,quartoFileName,overwrite=T)
      }
      fs::file_move(paste0(quartoFile),quartoPath)
    }
  }else{
    if(toDOCX){
      docxFile <- rmarkdown::render(input=infile,output_dir=outPath,
                                    output_format="bookdown::word_document2")
      if(docUP){
        fs::file_copy(docxFile,docFileName,overwrite=T)
      }
    }
    if(toHTML){
      htmlFile <- rmarkdown::render(input=infile,output_dir=outPath,
                                output_format="bookdown::html_document2")
      if(htmlUP){
        fs::file_copy(htmlFile,nbFileName,overwrite=T)
      }
    }
    if(toPDF){
      pdfFile <- rmarkdown::render(input=infile,output_dir=outPath,
                                  output_format="bookdown::pdf_document2")
      if(pdfUP){
        fs::file_copy(pdfFile,pdfFileName,overwrite=T)
      }
    }
  }

  if(!silent){
    ofile <- htmlFile
    if(file.exists(file.path(sysRoot,"OneDrive","ul","Notes"))){
      lfile <- gsub(file.path("/srv","http"),file.path(sysRoot,"OneDrive","ul","Notes"),ofile)
      if(file.copy(ofile,lfile,over=T)) cat(paste(basename(ofile),"copied to OneDrive\n"))
    }

    if(RCurl::url.exists("http://localhost")){
      ofile <- gsub("/srv/http/","http://localhost/",ofile)
      system2("firefox",args=ofile ,wait=F,stderr=F)
    }else{
      if(!is.null(lfile)){
        system2("firefox",args=lfile,wait=F)
      }
    }
  }
}
