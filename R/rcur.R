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
#'
#' @export
rcur <- function(fileName=.curFile,projDir=.curProj,
                 inDir=NULL, outDir=NULL,open="html",godPath=NULL,
                 rootDir=file.path(Sys.getenv("HOME"),"Projects"),
                 codeDir=file.path(Sys.getenv("HOME"),"Projects"),
                 sourcecopy=F,toPDF=F,toDOCX=F,toHTML=T,toGOD=T,GOD=T,setWH=F){
  if(is.null(outDir) & is.null(inDir)){
    infile <- file.path(codeDir,projDir,"Code",paste0(fileName,".Rmd"))
    outpath <- file.path(rootDir,projDir,"Notes")
  }else if(!is.null(outDir) & !is.null(inDir)){
    infile <- file.path(inDir,paste0(fileName,".Rmd"))
    outpath <- file.path(outDir)
  }else{
    stop("either use default project paths or specify both in and out directories")
  } 
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
  if(sourcecopy){
    file.copy(infile,outpath,overwrite=T)
  }
  if(toGOD){
    if(is.null(godPath)){
      godPath <- paste0("/work/Projects/",projDir,"/")
    }
    system(paste0("scp ",htmlFile," pieta@pieta.me:public_html",godPath))
    if(setWH){
       system(paste0("scp ",htmlFile," pieta@pieta.me:public_html/work/home.html"))
    }
  }
  if(!is.null(open)){
    switch(open,
      pdf=if(toPDF){
        system(paste0("evinc ",pdfFile))
      },
      docx=if(toDOCX){
        system(paste0("loffice ",docxFile))
      },
      if(GOD){
        system(paste0("firefox http://pieta.me",godPath,basename(htmlFile)))
      }else{
        system(paste0("firefox ",htmlFile))
      }
    )
  }
}
