#' execute a command remotely via ssh
#'
#' @param cmd the command to execute
#' @param hostname the remote machine
#' @param username currently works with ssh keys rather than password
#' 
#' @export
rsh <- function(cmd, hostname="feenix",username="pschofield"){
  system(paste0("ssh -Y ",username,"@",hostname," '-l -c bash ",cmd,"'"),intern=T)
}

