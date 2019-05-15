#' get files from the cluster using scp to avoid using fuse for the moment
#'
#' @param filename base files
#' @param remDir remote directory path
#' @param locDir local directory path
#' @param remRoot remote root
#' @param locRoot local root
#' @param host hostname
#' @param user username
#'
#' @export
getFiles <- function(fn=NULL,filenames=NULL, projName, locDir=.localData,recursive=F, 
                     host="feenix",user="pschofield",force=F, simple=T){
  cmd <- "scp "
  dir.create(file.path(locDir),showWarnings=F,recursive=T)
  if(recursive){
    cmd <- "scp -r "
  }
  if(!is.null(filenames)){
    ret <- plyr::ldply(sapply(filenames,function(fn){
      locFile <- file.path(locDir,basename(fn))
      if(!force){
        if(file.exists(locFile)){
          return(locFile)
        }
      }
      system(paste0(cmd,user,"@",host,":",fn," ",locFile))
      locFile
    }))
    colnames(ret) <- c("remotedir","localdir")
    ret$filename <- basename(ret$localdir)
    ret$remotedir <- dirname(ret$remotedir)
    ret$localdir <- dirname(ret$localdir)
    if(!simple){
      ret
    }else{
      file.path(ret$localdir,ret$filename)
    }
  }else{
    system(paste0(cmd,user,"@",host,":",fn," ",locDir),intern=T)
  }
}
