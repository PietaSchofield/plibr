#' use heatmap and tree cutting
#'
#' @export
hmAnalysis <- function(exprMat,rowV=c("correlation","mcquitty"),colV=c("canberra","average"),
                       ncuts=6,filename=NA){
   ah <- NMF::aheatmap(exprMat, scale="row",Rowv=rowV, Colv=colV,
                  main=paste0("PGS vs Untreated in LPS and Untreated CTR Sbmples \n",
                       length(rownames(exprMat))," significant DE genes "),filename=".tmp.pdf")
   ct <- as.data.frame(dendextend::cutree(as.dendrogram(ah$Rowv),ncuts))
   colnames(ct) <- "Group"
   ct$Group <- as.factor(ct$Group)
   ah <- NMF::aheatmap(exprMat, scale="row",Rowv=rowV, Colv=colV,
                  annRow=ct, main=paste0("PGS vs Untreated in LPS and Untreated CTR Samples \n",
                        length(rownames(exprMat))," significant DE genes "),filename=filename)
   return(ct)
}
