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
#'
#' @export
rc <- function(fileName=.curFile,projName=.projName,codeDir="GitLab",gitRepo="liverpool",
               sysRoot=Sys.getenv("HOME"), topDir="public_html", setGH=F, toPDF=F,toDOCX=F,
               toHTML=T,upload=T,outRoot=".tmp",outDocxPath=NULL,userid=Sys.getenv("USER"),
               mys=Sys.getenv("MYS"),livUP=FALSE,livFTP="pcftp.liv.ac.uk:21",
               livPath="/public.www",godPath=NULL){
  if(tolower(Sys.info()["sysname"])=="windows"){
    sysRoot <- "M:"
    outRoot <- "/Documents"
    outDocxPath <- NULL
    upload <- FALSE
  }
  if(setGH){
    godFile <- file.path(topDir,"index.html")
  } else {
    if(!is.null(projName)){
      if(is.null(godPath)){
        godPath <- file.path(topDir,gitRepo,projName)
      }
      outpath <- file.path(sysRoot,outRoot,projName)
      codePath <- file.path(sysRoot,codeDir,gitRepo,projName)
    }else{
      if(is.null(godPath)){
        godPath <- file.path(topDir,gitRepo)
      }
      outpath <- file.path(sysRoot,outRoot,gitRepo)
      codePath <- file.path(sysRoot,codeDir,gitRepo)
    }
    dir.create(outpath,showW=F,recur=T)
    godFile <- file.path(godPath,paste0(fileName,".html"))
  }
  infile <- file.path(codePath,paste0(fileName,".Rmd"))
  if(toDOCX){
    docxFile <- rmarkdown::render(input=infile,output_dir=outpath,
                               output_format="bookdown::word_document2")
    if(!is.null(outDocxPath)){
      RCurl::ftpUpload(docxFile,paste0("ftp://",userid,":",mys,"@",livFTP,
                                       file.path(outDocxPath,basename(docxFile))))
    }
  }
  if(toHTML){
    htmlFile <- rmarkdown::render(input=infile,output_dir=outpath,
                               output_format="bookdown::html_document2")
  }
  if(toPDF){
    pdfFile <- rmarkdown::render(infile,output_dir=outpath,
                                 output_format="bookdown::pdf_document2")
  }
  if(upload){
    system(paste0("scp ",htmlFile," ",paste0("pieta@pieta.me:",godFile)))
  }else{
    cat(paste0("vivaldi ",htmlFile,"\n"))
  }
  loadTo <- paste0("ftp://",userid,":",mys,"@",livFTP,
                   file.path(livPath,projName,basename(htmlFile)))
  if(livUP){
    RCurl::ftpUpload( what=htmlFile, to=loadTo)
  }else{
    cat(paste0("\nLiverpool Path ",loadTo,"\n"))
  }
}
