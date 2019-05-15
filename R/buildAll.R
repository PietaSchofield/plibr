#' Build an Rmarkdown file or directory of files into html
#'
#' This function is to make it easier to build pages from Rmd files in my personal webspace
#' It is a bit of a work in progress I am trying to keep it logical and not have too many 
#' exceptions that need special cases. At the moment I have not sorted out index pages for 
#' the private and public directories
#'
#' @param project the name of the project
#' @param file the name of the file (this is the file stub without path or extention)
#' @param outdir the location where the output file will be written
#' @param indir the path to the input file
#' @param explicit boolean specifies that the paths will be use rather than constructed
#' @param outType character string type of output 'all' will make all
#'
#' @export
buildAll <- function(project,file="*",outstub="", outdir="/homes/pschofield/",
                     indir="/Users/pschofield/git_tree/",explicit=FALSE, 
                     outType="html_document",interact=FALSE){
  if(!file.exists(outdir) & !explicit){
    outdir <- "/Users/pschofield/"
  }else if(!file.exists(outdir)){
    stop(paste("Can't find ",outdir," Check network drive?"))
  }
  pat <- paste0("^",file,"[.]Rmd$")
  if(!explicit){
    inDir <- paste0(indir,project,"/")
    if(project=="web_pages"){
      outDir <- paste0(outdir,"public_html/")
    }else{
      outDir <- paste0(outdir,"Projects/",project,"/web/",outstub)
    }
    if(file=="*"){
      pat <- ".*Rmd"
    }
  }else{
    inDir <- indir
    outDir <- outdir
  }
  files <- list.files(inDir,pattern=pat,full=T)
  lapply(files,
    function(f){
      print(f)
      if(!interact){
        rmarkdown::render(f,output_format=outType, output_dir=outDir)
      }else{
        rmarkdown::run(f,output_format=outType, output_dir=outDir)
      }
    })
}


