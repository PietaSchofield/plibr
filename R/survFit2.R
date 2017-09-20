#' survAnal: runs cox proportional hazard models against patients with top and bottom n percent expression values
#'
#' The runCPH function will process three separate file one with sample ids one with survival data and one with
#' expression values. In test mode if a list of sample ids are supplied but not the other two files it assumes the
#' ids are from the TCGA SKCM data set and uses a cached version of the normalised expression values from the TCGA
#' SKCM data set and a currated set of survival data.
#'
#' It will also perform a limma differential gene expression test for the significance of the fold change between the
#' top and bottom n percent of expression values
#'
#' @param geneData names of the genes to be tested (must be in the rownames of exprData)
#' @param survData survival data must contain a time and status
#' @param exprData normalised gene expression data
#' @param percExpr filter for minimum percentage of samples required to be expression a gene
#'    to select genes for testing
#' @param exprRange top and bottom quantiles of expression values to select samples for testing
#' @param sep default separator for delimited survival and sample id files supplied
#' @param exprSep default separator for expression file supplied
#' @param outputDir location to write output files
#' @param analysisName stub to add to output file names
#' @param silent boolean return the full set of statistics as a data frame
#'
#' @return the function creates several csv output files with the Cox PH results, the Limma DGE results, a combined set
#' of both results and a set of those genes for which the DGE result is significant at 0.05 FDR
#'
#' @examples
#' analysisName="test3"
#' exprSep=","
#' res <- runCPH(sampleDataFile=sampleDataFile,expressionDataFile=expressionDataFile,
#'               survivalDataFile=survivalDataFile,
#'               analysisName=analysisName,exprSep=exprSep)
#'
#' @export
survFit2 <- function(survData=NULL, exprData=NULL,geneData=NULL,survTime="time", 
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
  mcParam <- BiocParallel::MulticoreParam(workers=mcores)
  exprData2 <- t(exprData[which(rownames(exprData)%in%geneData),])
  survData2 <- merge(survData[,c(survTime,survStatus)],
                     exprData2,by="row.names") 
  # filter out genes not expressed in percentage of samples
  # run cox proportional hazard on all expressed genes
  resList <- BiocParallel::bplapply(geneData,function(geneid){
    # get max of either days to death or days to last follow up for time
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
  colnames(resSurv) <- c("log(coef)","log(1/coef)","lower95CI","upper95CI","pvalue")
  return(resSurv)
}

