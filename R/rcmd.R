#' execute a command remotely via ssh
#'
#' @param cmd the command to execute
#' @param hostname the remote machine
#' @param username currently works with ssh keys rather than password
#' 
#' @export
rcmd <- function(cmd, hostname="troodon.scicom.picr.man.ac.uk",username="pschofield"){
  system(paste0("ssh -Y ",username,"@",hostname," '",cmd,"'"),intern=T)
}

