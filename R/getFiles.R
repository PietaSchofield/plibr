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
getFiles <- function(filenames, projName, locDir=NULL, 
                     remRoot="/scratch/pschofield/Projects",
                     locRoot=file.path(Sys.getenv("HOME"),"Projects"),
                     host="troodon",user="pschofield",force=F,
                     simple=T){
  ret <- plyr::ldply(sapply(filenames,function(fn){
    if(!is.null(locDir)){
      toDir=file.path(locRoot,projName,locDir)
    }else{
      toDir=gsub(remRoot,locRoot,dirname(fn))
    }
    dir.create(file.path(toDir),showWarnings=F,recursive=T)
    remFile <- file.path(fn)
    locFile <- file.path(toDir,basename(fn))
    if(!force){
      if(file.exists(locFile)){
        return(locFile)
      }
    }
    system(paste0("scp ",user,"@",host,":",remFile," ",locFile))
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
}
