#' survAnal: runs cox proportional hazard models against patients with top and bottom n percent
#' expression values
#'
#' The runCPH function will process three separate file one with sample ids one with survival data
#' and one with expression values. In test mode if a list of sample ids are supplied but not the
#' other two files it assumes the ids are from the TCGA SKCM data set and uses a cached version of
#' the normalised expression values from the TCGA SKCM data set and a currated set of survival
#' data.
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
#' @examples
#' analysisName="test3"
#' exprSep=","
#' res <- runCPH(sampleDataFile=sampleDataFile,expressionDataFile=expressionDataFile,
#'               survivalDataFile=survivalDataFile,
#'               analysisName=analysisName,exprSep=exprSep)
#'
#' @export
survFit <- function(survData=NULL, exprData=NULL,geneData=NULL,survTime="time", 
                    survStatus="status",mcores=4){
  require(survival)
  # Filter by samples in sample data just incase
  exprSamples <- colnames(exprData)
  survSamples <- rownames(survData)

  # Sanity check the sample ids are the same
  if(!identical(unique(sort(exprSamples)), sort(survSamples))){
    stop(paste0("The samples in ",paste0(survSamples,collapse=",")," do not match those in the ",
                "expression and survival data files"))
  }
  if(!identical(sort(exprSamples), sort(survSamples))){
    warning(paste0("The samples in ",survSamples," potentially have more than one expression",
                   " sample"))
  }
  rownames(exprData) <- gsub("(-|[.]|[/])","_",rownames(exprData))
  geneData <- gsub("(-|[.]|[/])","_",geneData)
  mcParam <- BiocParallel::MulticoreParam(workers=mcores)
  # filter out genes not expressed in percentage of samples
  exprData <- as.matrix(exprData)
  survData2 <- merge(survData[,c(survTime,survStatus)],
                     t(exprData[which(rownames(exprData)%in%geneData),]),by="row.names")
  # run cox proportional hazard on all expressed genes
  resList <- BiocParallel::bplapply(geneData,function(geneid){
    # Censor the time data to generate survival data
    form <- paste0("Surv( ",survTime,",",survStatus,") ~ ",geneid)
    # fit cox proportional hazards (non-parametric) survival model
    coxfit <- survival::coxph(as.formula(form),data=survData2)
    list(surv=c(summary(coxfit)$conf.int,summary(coxfit)$sctest["pvalue"]))
  }, BPPARAM=mcParam)
  names(resList) <- geneData
  # convert results to data frame
  resSurv<- plyr::ldply(lapply(resList,"[[","surv"))
  rownames(resSurv) <- resSurv[,1]
  resSurv <- resSurv[,-1]
  colnames(resSurv) <- c("HR","1/HR","lower95CI","upper95CI","pvalue")
  return(resSurv)
}



