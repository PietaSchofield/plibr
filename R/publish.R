#' Publish all relevant files from the local directory to my public_html tree
#'
#' @param filePath full name of the markdown file from which the html was generated
#' @param note publish to the notebook directory
#' @param outStub name of subdirectory to publish to
#' @param delete delete the local files after copying
#' @param open open the file on publishing
#'
#' @export
publish <- function(rname="/Users/pschofield/Code/",pname=NULL,fname=NULL,note=T,
                    outStub=NULL,delete=F,open=T){
  devDir <- paste0(rname,pname,"/")
  if(note) outStub <- "notebook"
  outDir <- paste0("/Volumes/pschofield/public_html/Projects/",pname,"/",outStub,"/")

  system(paste0("sed -i.bak 's/libs\\//\\/user\\/pschofield\\/libs\\//g' ",
                devDir,fname,".html"))
  # copy the file
  file.copy(paste0(devDir,fname,".html"),paste0(outDir,fname,".html"), overwrite=T)
  dir.create(paste0(outDir,fname,"_files/figure-html"),rec=T,showWarnings = F)
  # copy all the pictures
  success <- lapply(list.files(paste0(devDir,fname,"_files"), pattern="*[.]*",recur=T),
                    function(fn){
                      file.copy(from = paste0(devDir,fname,"_files/",fn),
                                to = paste0(outDir,fname,"_files/",fn),overwrite=T)
                    })
  if(open){
    system(paste0("open http://www.compbio.dundee.ac.uk/user/pschofield/",
                  "Projects/",pname,"/",outStub,"/",fname,".html"))
  }
  if(delete){
    unlink(paste0(devDir,fname,"_files"),rec=T,force=T)
    unlink(paste0(devDir,fname,".html*"))
  }
}
