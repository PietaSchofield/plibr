#' execute a command remotely via ssh
#'
#' @param cmd the command to execute
#' @param hostname the remote machine
#' @param username currently works with ssh keys rather than password
#' 
#' @export
rcmd <- function(cmd="",crick=F,hostname=NULL,username=NULL,Xfwd=""){
  if(!crick){
    if(is.null(hostname)){
      hostname <- "troodon.scicom.picr.man.ac.uk"
      username <- "pschofield"
      Xfwd <- "-Y"
    }
  }else{
    hostname <- "login002.camp.thecrick.org"
    username <- "schofip"
    Xfwd <- ""
  }
  system(paste0("ssh ",Xfwd," ",username,"@",hostname," '",cmd,"'"),intern=T)
}
