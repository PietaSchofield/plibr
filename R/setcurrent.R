#' set current month
#' 
#' @export
setcurrent <- function(fileName,dir_path="/srv/http/uol/notes",linkName="current"){
  
  # Full path to the target file
  target_path <- file.path(dir_path, paste0(fileName,".html"))
  
  # Path for the symbolic link
  link_path <- file.path(dir_path, paste0(linkName,".html"))
  if(file.exists(link_path)) unlink(link_path)  
  # Create the symbolic link
  file.symlink(from = target_path, to = link_path)
}
