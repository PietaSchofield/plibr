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
getFiles <- function(filenames, remDir, locDir=remDir, remRoot="/scratch", locRoot="/Users",
                     host="troodon",user="pschofield",force=F){
  dir.create(file.path(locRoot,locDir),showWarnings=F,recursive=T)
  ret <- lapply(filenames,function(fn){
    remFile <- file.path(remRoot,remDir,fn)
    locFile <- file.path(locRoot,locDir,fn)
    if(!force){
      if(file.exists(locFile)){
        return(locFile)
      }
    }
    system(paste0("scp ",user,"@",host,":",remFile," ",locFile))
    locFile
  }) 
  unlist(ret)
}
