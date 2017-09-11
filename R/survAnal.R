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
survAnal <- function(survData=NULL, exprData=NULL,geneData=NULL,survTime="time", 
                     exprRange=0.33, survStatus="status"){

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


  # filter out genes not expressed in percentage of samples
  exprData <- as.matrix(exprData)
  # run cox proportional hazard on all expressed genes
  resList <- lapply(geneData,function(geneid){
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
    survData2$Class <- as.factor(survData2$Class)
    # get max of either days to death or days to last follow up for time
    # Censor the time data to generate survival data
    form <- paste0("Surv( ",survTime,",",survStatus,") ~ Class")
    # fit cox proportional hazards (non-parametric) survival model
    coxfit <- survival::coxph(as.formula(form),data=survData2)
    hiExpr <- unname(exprData[geneid,which(exprData[geneid,]>=hlim)])
    hiExpr <- hiExpr[order(hiExpr)]
    loExpr <- unname(exprData[geneid,which(exprData[geneid,]<=llim)])
    loExpr <- loExpr[order(loExpr)]
    list(surv=c(summary(coxfit)$conf.int,summary(coxfit)$sctest["pvalue"]))
  })#, BPPARAM=mcParam)
  names(resList) <- geneData
  # convert results to data frame
  resSurv<- plyr::ldply(lapply(resList,"[[","surv"))
  rownames(resSurv) <- resSurv[,1]
  resSurv <- resSurv[,-1]
  colnames(resSurv) <- c("HR","1/HR","lower95CI","upper95CI","pvalue")
  return(resSurv)
}

