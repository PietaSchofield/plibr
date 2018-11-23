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
#' @param inDir override codeDir construction with exact path
#' @param outDir copy to specific output subdirectory
#' @param sourceCopy copy source to the output location too
#' @param toData make a copy of the output in another public location
#' @param dataDir secondary location for copy 
#' @param open view the output file
#' @param locOnly write output to local hardrive rather than network share
#' @param setWH sets this file as the home file in the work directory
#' @param setPI sets this file as the home file in the work directory
#'
#' @export
rc <- function(fileName=.curFile,projDir=.projName,godDir="work",codeDir="Projects",
                 inDir=NULL, outDir=NULL,open="html",godHead="public_html",
                 sysId=Sys.info()["sysname"],setGH=F,setPH=F,
                 toPDF=F,toDOCX=F,toHTML=T,upload=T,setWH=F,setPI=F){
  godPath <- file.path(godHead,godDir)
  outpath <- file.path(Sys.getenv("HOME"),".tmp")
  codePath <- file.path(Sys.getenv("HOME"),codeDir,projDir,"Code")
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
  system(paste0("cp ",htmlFile," ",file.path(Sys.getenv("HOME"),godFile)))
  system(paste0("cp ",htmlFile," ",file.path(Sys.getenv("HOME"),godFile)))
  if(upload){
    system(paste0("scp ",htmlFile," ",paste0("pieta@pieta.me:",godFile)))
  }
  paste0("sudo cp ",paste0(file.path(Sys.getenv("HOME"),godFile))," ",
         file.path("/var","www",gsub("public_","",godFile)))
}
