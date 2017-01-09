#' sumForPlot 
#'
#' generate a summary for creating pretty boxplots for data with missing values
#'
#' @param dfin data frame in
#' @param cols over
#' @param val field
#'
#' @export
sumForPlot <- function(dfin,cols,val) {
  sumit <- function(xx,co){
            x <- xx[[co]]
            if(length(x)>0){
              n <- length(x)
              qv <- quantile(x)
              llim <- qv[3] - (1.5*(qv[4]-qv[2]))
              lwk <- ifelse(length(x[x>llim])>0, min(x[x > llim]), 0)
              llim <- qv[3] + (1.5*(qv[4]-qv[2]))
              uwk <- ifelse(length(x[x<llim])>0, max(x[x < llim]), max(x))
              ret <- c(n,lwk,qv[2:4],uwk)
            } else {
              ret <- c(0,rep(NA,5))
            }         
            names(ret) <-  c("n","ymin", "lower", "middle", "upper", "ymax")
            ret
          }
  ddply(dfin,cols,.drop=F,.fun=sumit,val)
}


