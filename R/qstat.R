#' qstat
#'
#' run qstat on the cluster remotely and return the results to an R session this is
#' currently just a simple function which defaults to my user name
#'
#' @param user the username for the cluster job listing
#'
#' @export
qstat <- function(user="pschofield"){
 rsh(paste0('qstat -u "',user,'"'))
}
