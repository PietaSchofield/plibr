#' runCPH: runs cox proportional hazard models against patients with top and bottom n percent expression values
#'
#' The runCPH function will process three separate file one with sample ids one with survival data and one with
#' expression values. In test mode if a list of sample ids are supplied but not the other two files it assumes the
#' ids are from the TCGA SKCM data set and uses a cached version of the normalised expression values from the TCGA
#' SKCM data set and a currated set of survival data.
#'
#' It will also perform a limma differential gene expression test for the significance of the fold change between the
#' top and bottom n percent of expression values
#'
#' @param sampleDataFile name of the a delimited sample data file, first column should contain the
#'     sample id's for samples to match in survival and expression data
#' @param survivalDataFile name of the delimited survival data file, first column should contain the
#'     sample ids to match with sample data and expression data. If this file is not specified uses
#'     default data created for TCGA SKCM samples
#' @param expressionDataFile name of the delimited expression data file, second row should contain the
#'     sample ids as column headings to match with sample data and expression data. If this file is not specified uses
#'     default data created for TCGA SKCM samples normalised expression values. NB when this file is read in
#'     the first line will be skipped.
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
#'
#' @export
runCPH <- function(exprRange=0.33,percExpr=0.8,sep=";",exprSep=sep,outputDir=getwd(),silent=F,
                   survData=NULL, expressionData=NULL,simple=T,
                   analysisName=paste("run",gsub("([ ]|[:])","_",format(Sys.time()))),scan=T,
                   geneList=NULL){
  # check files
  if(is.null(survData)){
    stop("Sample data file not supplied using samples from survival data file")
  }
  if(is.null(expressionData)){
    stop("Sample data file not supplied using samples from survival data file")
  }

  # Filter by samples in sample data just incase
  exprData <- expressionData[,which(colnames(expressionData) %in% rownames(survData))]
  exprSamples <- colnames(exprData)
  survSamples <- rownames(survData)

  # Sanity check the sample ids are the same
  if(!identical(unique(sort(exprSamples)), sort(survSamples))){
    stop(paste0("The samples in survival data do not match those in the expression and ",
                "survival data files"))
  }
  if(!identical(sort(exprSamples), sort(survSamples))){
    warning(paste0("The samples in survival potentially have more than one expression",
                   " sample"))
  }

  # perform the analysis
  mcParam <- BiocParallel::MulticoreParam()

  # filter out genes not expressed in percentage of samples
  exprData <- as.matrix(exprData)
  if(is.null(geneList)){
    fGenes <- which(rowSums(apply(exprData,2,">",0))/ncol(exprData) > percExpr)
    exprGenes <- rownames(exprData)[fGenes]
  }else{
    exprGenes <- geneList[which(geneList%in%rownames(exprData))]
  }

  # run cox proportional hazard on all expressed genes
  resList <- BiocParallel::bplapply(exprGenes,function(geneid){
    # get Hi samples expression >= n centile
    hlim <- as.numeric(quantile(exprData[geneid,],1-exprRange))
    hi <- survData[unique(substr(names(which(exprData[geneid,]>=hlim)),1,12)),]
    hi$Class <- "high"
    # get Lo samples expression <= 1-n centile
    llim <- as.numeric(quantile(exprData[geneid,],exprRange))
    lo <- survData[unique(substr(names(which(exprData[geneid,]<=llim)),1,12)),]
    lo$Class <- "low"
    # make survival data frame
    survData2 <- rbind(hi,lo)
    # set expression class
    survData2$Class <- as.factor(survData2$Class)
    # get max of either days to death or days to last follow up for time
    # Censor the time data to generate survival data
    survData2$survival <- survival::Surv(survData2$time,as.numeric(survData2$status))
    # fit cox proportional hazards (non-parametric) survival model
    coxfit <- survival::coxph(survival~Class,data=survData2)
    fitdiff <- survival::survdiff(survival~Class,data=survData2)
    hiExpr <- unname(exprData[geneid,which(exprData[geneid,]>=hlim)])
    # the next line is fudge to fix an issue if all the low expression are zero
    loExpr <- unname(exprData[geneid,which(exprData[geneid,]<=llim)])[1:length(hiExpr)]
    list(surv=c(summary(coxfit)$conf.int,summary(coxfit)$sctest["pvalue"]),
         dif=fitdiff, expr=c(lo=loExpr,hi=hiExpr))
  }, BPPARAM=mcParam)
  names(resList) <- exprGenes
  if(simple){
    return(resList)
  }
  # convert results to data frame
  resSurv<- plyr::ldply(lapply(resList,"[[","surv"))
  rownames(resSurv) <- resSurv[,1]
  resSurv <- resSurv[,-1]
  colnames(resSurv) <- c("HR","1/HR","lower95CI","upper95CI","pvalue")
  if(!scan){
    write.csv(resSurv,file=file.path(outputDir,paste0("HR_results_",analysisName,".csv")))
  }
  # perform differential expression check using limma
  # between high and low samples for all expressed genes
  resExpr<- plyr::ldply(lapply(resList,"[[","expr"))
  rownames(resExpr) <- resExpr[,1]
  resExpr <- resExpr[,-1]
  design <- model.matrix(~as.factor(substr(colnames(resExpr),1,2)))
  dgeExpr <- edgeR::DGEList(resExpr,group=substr(colnames(resExpr),1,2))
  dgeExpr <- edgeR::calcNormFactors(dgeExpr)
  logCPM <- edgeR::cpm(dgeExpr,log=T,prior.count=2)
  fit <- limma::lmFit(logCPM,design)
  fit <- limma::eBayes(fit)
  tt <- limma::topTable(fit,n=2e5)
  if(!scan){
    write.csv(tt,file=file.path(outputDir,paste0("Statistics_high_vs_low_expression_",
                                                 analysisName, ".csv")))
  }
  # combine HR and DGE analysis
  res <- merge(tt,resSurv,by="row.names")
  if(!scan){
    write.csv(res,file=file.path(outputDir,paste0("Combined_statistics_HR_expression_",
                                                  analysisName, ".csv")))
  }

  # calculate corrected p.values for only those genes with significant DGE
  resHiCon <- res[which(res$adj.P.Val<=0.05),]
  resHiCon$HR.adjpvalue <- p.adjust(resHiCon$pvalue)
  if(!scan){
    write.csv(resHiCon,file=file.path(outputDir,paste0("Significant_DGE_statistica_",
                                                       analysisName, ".csv")))
  }
  if(!silent){
    return(res)
  }
}

