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
rc <- function(fileName=.curFile,projName=.projName,gitRepo=.gitRepo,sysRoot=.sysRoot,
               user=Sys.getenv("USER"),
               hostname=.hostName,
               htmlRoot=file.path("/","var","www","html"),
               shinyRoot=file.path("/","u1","shiny-server","samples","apps"),
               setHome=F, toPDF=F,toDOCX=F, toHTML=T,setRepo=T,setProj=T,toShiny=F,
               htmlUP=T, rmdUP=F,pdfUP=F,docUP=F,ext="Rmd",dbg=F,quarto=NULL,quartoUP=F){
  codeDir=file.path(sysRoot,"GitLab",gitRepo)
  outRoot=file.path(sysRoot,".tmp")
  docRoot=file.path(sysRoot,".tmp")
  codePath <- file.path(codeDir)
  if(toShiny){
    htmlPath <- file.path(shinyRoot)
  }else{
    htmlPath <- file.path(htmlRoot)
  }
  if(setRepo){
    htmlPath <- file.path(htmlPath,gitRepo)
    outPath <- file.path(outRoot,gitRepo)
  }else{
    outPath <- file.path(outRoot)
  }
  if(setProj){
    codePath <- file.path(codePath,projName)
    htmlPath <- file.path(htmlPath,projName)
    outPath <- file.path(outPath,projName)
  }
  if(setHome){
    htmlFileName <- file.path(htmlPath,"index.html")
  }else{
    htmlFileName <- file.path(htmlPath,paste0(fileName,".html"))
    if(rmdUP){
      shinyFileName <- file.path(htmlPath,paste0(fileName,".Rmd"))
    }else{
      shinyFileName <- file.path(htmlPath,paste0(fileName,".html"))
    }
    docFileName <- file.path(htmlPath,paste0(fileName,".docx"))
    pdfFileName <- file.path(htmlPath,paste0(fileName,".pdf"))
  }
  dir.create(outPath,showW=F,recur=T)
  infile <- file.path(codePath,paste0(fileName,".",ext))
  if(!is.null(quarto)){
    quartoFile <- paste0(xfun::sans_ext(infile),".",quarto)
    if(dbg) print(quartoFile)
    quarto::quarto_render(input=infile,execute_dir=outPath)
    if(file.exists(quartoFile)){
      if(quartoUP){
        quartoFileName <- file.path(htmlPath,paste0(fileName,".",quarto))
        system(paste0("scp ",quartoFile," ",paste0(user,"@",hostname,":",quartoFileName)))
      }
      fs::file_move(paste0(quartoFile),outPath)
    }
  }else{
    if(toDOCX){
      docxFile <- rmarkdown::render(input=infile,output_dir=outPath,
                                    output_format="bookdown::word_document2")
      if(docUP){
         system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",docFileName)))
      }
    }
    if(toHTML & !rmdUP){
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
        system(paste0("scp ",pdffile," ",paste0(user,"@",hostname,":",pdfFileName)))
      }
    }
    if(rmdUP){
      system(paste0("scp ",infile," ",paste0(user,"@",hostname,":",shinyFileName)))
    }
  }
}
