#' use heatmap and tree cutting
#'
#' @export
hmAnalysis <- function(exprMat,rowV=c("correlation","mcquitty"),colV=c("canberra","average"),
                       ncuts=6,filename=NA,mainTitle="Heatmap"){
   mainTitle <- paste0(mainTitle,"\n",length(rownames(exprMat))," significant DE genes")
   ah <- NMF::aheatmap(exprMat, scale="row",Rowv=rowV, Colv=colV, main=mainTitle, filename=".tmp.pdf")
   ct <- as.data.frame(dendextend::cutree(as.dendrogram(ah$Rowv),ncuts))
   colnames(ct) <- "Group"
   ct$Group <- as.factor(ct$Group)
   ah <- NMF::aheatmap(exprMat, scale="row",Rowv=rowV, Colv=colV, annRow=ct, main=mainTitle,
                       filename=filename)
   return(ct)
}
