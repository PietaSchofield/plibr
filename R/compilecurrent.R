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
#' @importFrom rlang %||%
#'
#' @export
compilecurrent <- function(fileName=.fileName,
               projName=.projName,
               gitRepo="uol",
               sysRoot=Sys.getenv("HOME"),
               user=Sys.getenv("USER"), 
               nbPath=file.path("/srv","http"),
               codePath=file.path(sysRoot,"repositories",gitRepo),
               docPath=file.path(sysRoot,"Projects"),
               silent=F,setHome=F,toPDF=F,toHTML=T,
               htmlUP=T, ext="Rmd",dbg=F,
               outPath=file.path(Sys.getenv("HOME"),"Projects"),
               srvRoot=NULL,
               html_of = "bookdown::html_document2",
               pdf_of ="bookdown::pdf_document2",
               browserOpt="browser_main"){

  if(gitRepo=="uol"){
    nbPath <- file.path(nbPath,"uol")
    outPath <- file.path(outPath)
  }else if(gitRepo=="pers"){
    nbPath <- file.path(nbPath,"pers")
    outPath <- file.path(outPath,"pers")
  }else{
    if(!is.null(srvRoot)){
      nbPath <- file.path(nbPath,srvRoot,gitRepo)
    }else{
      nbPath <- file.path(nbPath,gitRepo)
    }
    outPath <- file.path(outPath,gitRepo)
  }

  if(!is.null(projName)){
    codePath <- file.path(codePath,projName)
    nbPath <- file.path(nbPath,projName)
    outPath <- file.path(outPath,projName,"pubs")
  }

  # --- THE FIX: check source exists BEFORE touching the filesystem anywhere else ---
  infile <- file.path(codePath, paste0(fileName,".",ext))
  if(!file.exists(infile)){
    stop("compilecurrent: source file not found, nothing was created.\n  Looked for: ",
         infile, call. = FALSE)
  }
  # --------------------------------------------------------------------------------

  if(setHome){
    nbFileName <- file.path(nbPath,"index.html")
  }else{
    nbFileName <- file.path(nbPath,paste0(fileName,".html"))
  }

  dir.create(nbPath,showW=F,recur=T)
  dir.create(outPath,showW=F,recur=T)

  if(toHTML){
    if(htmlUP){
      htmlFile <- rmarkdown::render(input=infile,output_dir=nbPath, output_format=html_of)
    }else{
      htmlFile <- rmarkdown::render(input=infile,output_dir=outPath, output_format=html_of)
    }
    if(!silent && session_mode()=="interactive"){
      urlout <- gsub("/srv/http/","http://localhost/",htmlFile)
      if(RCurl::url.exists(urlout)){
        displayURL(urlout,bp=browserOpt)
      }else{
        displayURL(htmlFile,bp=browserOpt)
      }
    }
  }
  if(toPDF){
    pdfFile <- rmarkdown::render(input=infile,output_dir=outPath, output_format=pdf_of)
    if(!silent && session_mode()=="interactive"){
      system(paste("okular", shQuote(pdfFile),"&"))
    }
  }
  system("wmctrl -a vim")
}
