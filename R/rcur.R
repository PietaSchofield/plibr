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
rc <- function(fileName=.curFile,projDir=.projName,pers=NULL,godDir=NULL,
                 inDir=NULL, outDir=NULL,open="html",godHead="public_html",
                 sysId=Sys.info()["sysname"],setGH=F,setPH=F,
                 htmlApp="firefox",pdfApp="evince",wordApp="loffice",
                 toPDF=F,toDOCX=F,toHTML=T,upload=T,setWH=F,setPI=F){
  if(sysId=="Darwin"){
    htmlApp="open"
    pdfApp="open"
    wordApp="open"
  }
  if(is.null(pers)){
    godDir <- file.path("work","Projects")
    keydir <- "Projects"
  }else{
    if(is.null(godDir)) godDir <- "pers"
    keydir <- pers
  }
  godPath <- file.path(godHead,godDir)
  rootDir=file.path(Sys.getenv("HOME"),keydir)
  codeDir=file.path(Sys.getenv("HOME"),keydir)
  if(is.null(outDir) & is.null(inDir)){
    infile <- file.path(codeDir,projDir,"Code",paste0(fileName,".Rmd"))
    outpath <- file.path(rootDir,projDir,"Notes")
  }else if(!is.null(outDir) & !is.null(inDir)){
    infile <- file.path(inDir,paste0(fileName,".Rmd"))
    outpath <- file.path(outDir)
  }else{
    stop("either use default project paths or specify both in and out directories")
  } 
  dir.create(outpath,showW=F,recur=T)
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
  if(upload){
    if(setGH){
      godFile <- file.path("public_html","index.html")
    } else if(setWH){
      godFile <- file.path("public_html","work","index.html")
    } else if(setPH){
      godFile <- file.path("public_html","pers","index.html")
    }else if(setPI){
      godFile <- file.path("public_html","work","Projects","index.html")
    } else {
      godFile <- file.path(godPath,projDir,basename(htmlFile))
    }
    system(paste0("scp ",htmlFile," ",paste0("pieta@pieta.me:",godFile)))
    paste0(file.path("http://pieta.me",gsub(paste0(godHead,"/"),"",godFile)))
  }else{
    basename(htmlFile)
  }
}
