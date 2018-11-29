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
#' @param rootDir root of output tree
#'
#' @export
rc <- function(fileName=.curFile,projDir=.projName,godDir="work",codeDir=NULL,
                godHead="public_html", setGH=F,setPH=F, toPDF=F,toDOCX=F,toHTML=T,upload=T,
                setWH=F,setPI=F, locCopy=F){
  godPath <- file.path(godHead,godDir)
  outpath <- file.path(Sys.getenv("HOME"),".tmp")
  if(is.null(codeDir)){
    codePath <- file.path(Sys.getenv("HOME"),"Projects",projDir,"Code")
  }else{
    codePath <- file.path(Sys.getenv("HOME"),codeDir)
  }
  if(setGH){
    godFile <- file.path(godHead,"index.html")
  } else if(setWH){
    godFile <- file.path(godPath,"index.html")
  } else if(setPH){
    godFile <- file.path(godHead,"pers","index.html")
  }else if(setPI){
    godFile <- file.path(godPath,"Projects","index.html")
  } else {
    godFile <- file.path(godPath,projDir,paste0(fileName,".html"))
  }
  dir.create(outpath,showW=F,recur=T)
  infile <- file.path(codePath,paste0(fileName,".Rmd"))
  if(toDOCX){
    docxFile <- rmarkdown::render(input=infile,output_dir=outpath,
                               output_format="bookdown::word_document2")
  }
  if(toHTML){
    htmlFile <- rmarkdown::render(input=infile,output_dir=outpath,
                               output_format="bookdown::html_document2")
  }
  if(toPDF){
    pdfFile <- rmarkdown::render(infile,output_dir=outpath,
                                 output_format="bookdown::pdf_document2")
  }
  if(locCopy){
    system(paste0("cp ",htmlFile," ",file.path(Sys.getenv("HOME"),godFile)))
  }
  if(upload){
    system(paste0("scp ",htmlFile," ",paste0("pieta@pieta.me:",godFile)))
  }
  paste0("sudo cp ",htmlFile," ", file.path("/var","www",gsub("public_","",godFile)))
}
