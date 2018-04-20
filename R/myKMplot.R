#' plot Kaplan Myer curves
#'
#' @param fit result of survfit
#' @param cox result of coxph
#' @param alp alpha for transparancy of errors
#' @param sfact survival value to plot
#'
#' @export
plotKM <- function(formstr,dat,alp=I(1/10),sfact="surv",inclusive=F,y1=-0.5,y2=-1,
                   titlestr=gsub(".*[~]","",formstr)){
  require(ggplot2)
  require(survival)
  require(gridExtra)
  form <- as.formula(formstr)
  fit <- survival::survfit(form,data=dat)
  cox <- survival::coxph(form,data=dat)
  dif <- survival::survdiff(form,data=dat)
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
  rownames(tabkm) <- gsub(".*[=]","",rownames(tabkm))
  gp <- ggplot2::ggplot(pd) + ggplot2::geom_line(ggplot2::aes_string(x="time",y=sfact,
                                                                     colour="Class")) +
    ggplot2::geom_ribbon(ggplot2::aes(x=time,ymin=lower,ymax=upper,fill=Class),alpha=I(1/10)) +
    ggplot2::geom_point(aes_string(x="time",y=sfact,colour="Class"),
                        data=pd[which(pd$n.censor!=0),]) +
    ggplot2::ggtitle(paste0(titlestr)) 
  if(inclusive){
    gp <- gp + ylim(-1,1) +
      ggplot2::annotation_custom(tableGrob(tabcox),xmin=0,xmax=max(pd$time),
                                 ymin=y1,ymax=0) +
      ggplot2::annotation_custom(tableGrob(tabkm),xmin=0,xmax=max(pd$time),
                                 ymin=y2,ymax=y1)
  }
  return(list(graph=gp,tabkm=tabkm,tabcox=tabcox,fits=list(fit,cox,dif)))
}
