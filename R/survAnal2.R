#' survAnal: runs cox proportional hazard models against patients with top and bottom n percent
#' expression values
#'
#' derived from the runCPH function will process three separate file one with sample ids one with
#' survival data and one with expression values. In test mode if a list of sample ids are supplied
#' but not the other two files it assumes the ids are from the TCGA SKCM data set and uses a
#' cached version of the normalised expression values from the TCGA SKCM data set and a currated
#' set of survival data.
#'
#' @param geneData names of the genes to be tested (must be in the rownames of exprData) 
#' @param survData survival data must contain a time and status
#' @param exprData normalised gene expression data 
#' @param exprRange top and bottom quantiles of expression values to select samples for testing
#' @param survStatus name of suvival censoring column 
#' @param survTime name of survival time column 
#' @param mcores number of cores for bioparallel
#'
#' @return the function creates several csv output files with the Cox PH results, the Limma DGE
#' results, a combined set of both results and a set of those genes for which the DGE result is
#' significant at 0.05 FDR
#'
#'
#' @export
survAnal2 <- function(survData=NULL, exprData=NULL,survTime="time", exprRange=0.33,
                     survStatus="status",mcores=4,summarise=TRUE){
  # Filter by samples in sample data just incase
  exprSamples <- colnames(exprData) 
  survSamples <- rownames(survData)

  # Sanity check the sample ids are the same
  if(!identical(unique(sort(exprSamples)), sort(survSamples))){
    stop(paste0("The samples in ",paste0(survSamples,collapse=",")," do not match those in the ",
                "expression and survival data files"))
  }
  if(!identical(sort(exprSamples), sort(survSamples))){
    warning(paste0("The samples in ",paste(survSamples,collapse=","),
                   " potentially have more than one expression sample"))
  }

  mcParam <- BiocParallel::MulticoreParam(workers=mcores)
  exprData <- as.matrix(exprData)
  geneRun <- rownames(exprData)
  # run cox proportional hazard on all expressed genes
  #resList <- BiocParallel::bplapply(geneRun,function(geneid){
  resList <- lapply(geneRun,function(geneid){
    # get Hi samples expression >= n centile 
    hlim <- as.numeric(quantile(exprData[geneid,],1-exprRange))
    hi <- survData[names(which(exprData[geneid,]>=hlim)),]
    hi$Class <- "high"
    # get Lo samples expression <= 1-n centile
    llim <- as.numeric(quantile(exprData[geneid,],exprRange))
    lo <- survData[names(which(exprData[geneid,]<=llim)),]
    lo$Class <- "low"
    # make survival data frame
    survData2 <- rbind(hi,lo)
    # set expression class
    survData2$Expr <- exprData[geneid,match(rownames(survData2),colnames(exprData))]
    survData2$Class <- as.factor(survData2$Class)
    survData2$time <- as.numeric(survData2[,survTime])
    survData2$status <- survData2[,survStatus]
    survData2$survival <- with(survData2,survival::Surv(time,status))
    # fit cox proportional hazards (non-parametric) survival model
    if(summarise){
      coxfit <- survival::coxph(survival~Expr, data=survData2)
      c(summary(coxfit)$conf.int,summary(coxfit)$sctest["pvalue"])
    }else{
      diffit <- survival::survdiff(survival~Class, data=survData2)
      surfit <- survival::survfit(survival~Class, data=survData2)
      #plt <- plotKM("survival::Surv(survTime,survStatus)~Class", dat=survData2)
      #plt <- NULL
      plt <- survminer::ggsurvplot(survival::survfit(survival~Class,data=survData2),
                                   data=survData2, pval=T,risk.table=T)
      list(cph=coxfit,dif=diffit,kmf=surfit,plt=plt)
    }
  })#, BPPARAM=mcParam)
  names(resList) <- geneRun
  # convert results to data frame
  if(summarise){
    resSurv<- plyr::ldply(resList)
    rownames(resSurv) <- resSurv[,1]
    resSurv <- resSurv[,-1]
    colnames(resSurv) <- c("HR","1/HR","lower95CI","upper95CI","pvalue")
    resSurv
  }else{
    resList
  }
}

.testSurvAnal <- function(){
  survData=pdClin
  exprData=expData
  geneData=upAll
  survTime="time"
  exprRange=0.33
  survStatus="status"
  mcores=4
  summarise=TRUE
  geneid=geneData[2]
}

