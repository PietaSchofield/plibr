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
presentcurrent <- function(fileName=.fileName,
               projName=.projName,
               gitRepo="liverpool",
               sysRoot=Sys.getenv("HOME"),
               user=Sys.getenv("USER"), 
               outPath=file.path(Sys.getenv("HOME"),"Projects"),
               codePath=file.path(sysRoot,"GitLab",gitRepo),
               silent=F, ext="Rmd",dbg=F){
 
  pptxPath <- file.path(outPath,projName)
  codePath <- file.path(codePath,projName)
  pptxFileName <- file.path(pptxPath,paste0(fileName,".pptx"))

  dir.create(dirname(pptxFileName),showW=F,recur=T)
  infile <- file.path(codePath,paste0(fileName,".",ext))
  

  outfile <- rmarkdown::render(infile, 
                  output_format = "powerpoint_presentation",
                  output_options = list(reference_doc =
                                        system.file('pptx','chi_template.pptx',package='plibr')),
                  output_file = pptxFileName)

  if(!silent){
    system(paste("onlyoffice-desktopeditors", shQuote(pptxFileName)),intern=F,wait=F)
  }
}
