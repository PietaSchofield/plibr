#' Submit a job to the cluster
#'
#' This functions submits a job in the form of a scriptfile to the cluster via ssh
#'
#' @param batchFile a string representing the name of the script file to be submitted
#' @param user a string with the user name of the person making the ssh connection
#' @param host a string with the name of the login host for the cluster
#' @param qsubstring the submission string for the torque system
#' @param args any arguments to add to the job submission string
#' @param noSub unless set to false will merely display the command rather than submit it.
#' @param scpIt copy to remote using scp before submit when not using FUSE
#'
#' @export
subJob <- function(scriptfile,locRoot="/Users",remRoot="/scratch",user="pschofield", 
                   host="troodon.scicom.picr.man.ac.uk",setDir="cd /scratch/pschofield;",
                   qsubString="qsub",args=NULL, noSub=T, scpIt=F, db=F )
{
  remScript <- gsub(locRoot,remRoot,scriptfile)
  if(scpIt){
    mkCmd <- paste0("ssh ",user,"@",host," 'mkdir -p ",dirname(remScript),"'")
    system(mkCmd)
    cpCmd <- paste0("scp ",scriptfile," ",user,"@",host,":",dirname(remScript))
    system(cpCmd)
    if(db){
      print(mkCmd)
      print(cpCmd)
    }
  }
  jobString <- paste0("'source /etc/bashrc;",setDir , qsubString," ",remScript, " ",args," ")
  sshString <- paste0("ssh ",user,"@",host," ",jobString,"'")
  if(noSub){
    paste0(qsubString," ",remScript, " ",args," ")
  } else if(db){
    sshString
  }else{
    list(jobid=system(sshString, intern=T), jobstring=sshString)
  }
}


