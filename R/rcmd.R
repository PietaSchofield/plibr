#' execute a command remotely via ssh
#'
#' @param cmd the command to execute
#' @param hostname the remote machine
#' @param username currently works with ssh keys rather than password
#' 
#' @export
rcmd <- function(cmd="",hostname=NULL,username=NULL,Xfwd=""){
  if(!is.null(username)){
    system(paste0("ssh ",Xfwd," ",username,"@",hostname," '",cmd,"'"),intern=T)
  }else{
    system(paste0("ssh ",hostname," '",cmd,"'"),intern=T)
  }
}
