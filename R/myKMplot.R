#' plot Kaplan Myer curves
#'
#' @param fit result of survfit
#' @param cox result of coxph
#' @param alp alpha for transparancy of errors
#' @param sfact survival value to plot
#'
#' @export
plotKM <- function(formstr,dat,alp=I(1/10),sfact="surv"){
  require(ggplot2)
  require(survival)
  require(gridExtra)
  form <- as.formula(formstr)
  fit <- survival::survfit(form,data=dat)
  cox <- survival::coxph(form,data=dat)
  pd <- plyr::ldply( lapply(c(2:6,9:11),function(x)fit[[x]]))
  pd <- t(pd)
  pd[!is.finite(pd)] <- 0
  pd <- as.data.frame(pd)
  colnames(pd) <- names(fit)[c(2:6,9:11)]
  pd$Class <- sub("^.*=","",unlist(lapply(seq(1,length(fit$strata)),function(x){
                rep(names(fit$strata)[x],fit$strata[x])
              })))
  zeros <- plyr::ldply(lapply(names(fit$strata),function(n){
             c(0,fit$strata[n]+2,0,0,1,1,1,0.99,gsub(".*=","",n))
           }))
  colnames(zeros) <- colnames(pd)
  pd <- rbind(pd,zeros)
  pd[,1:8] <- apply(pd[,1:8],2,as.numeric)
  mt <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.75)),
    colhead = list(fg_params=list(cex = 0.75)),
    rowhead = list(fg_params=list(cex = 0.75)))
  tabkm <- apply(summary(fit)$table[,-c(2,3)],2,signif,3)
  tabcox <- as.data.frame(t(signif(summary(cox)$sctest,3)))
  rownames(tabcox) <- "Log Rank Test"
  gp <- ggplot(pd) + geom_line(aes_string(x="time",y=sfact,colour="Class")) + ylim(c(0,1.1)) +
    geom_ribbon(aes(x=time,ymin=lower,ymax=upper,fill=Class),alpha=I(1/10)) +
    geom_point(aes_string(x="time",y=sfact,colour="Class"),data=pd[which(pd$n.censor!=0),]) 
  return(list(graph=gp,tabkm=tabkm,tabcox=tabcox))
}
