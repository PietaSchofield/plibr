#' use heatmap and tree cutting
#'
#' @export
hmAnalysis <- function(exprMat,rowV=NA,colV=NA,
                       ncuts=6,filename=NA,mainTitle="Heatmap",cr=1.5,cc=0.5){
   mainTitle <- paste0(mainTitle,"\n",length(rownames(exprMat))," significant DE genes")
   if(!is.na(rowV)){
     ct <- as.data.frame(dendextend::cutree(rowV,ncuts))
     colnames(ct) <- "Group"
     ct$Group <- as.factor(ct$Group)
     ctr <- merge(exprMat[,c(1:3)],ct,by="row.names")
     rownames(ctr) <- ctr[,1]
     ctr <- ctr[,-1]
     anR <- ct$Group
     names(anR) <- rownames(anR)
     ah <- NMF::aheatmap(exprMat[,-c(1:3)], scale="row",Rowv=rowV, Colv=colV, annRow=ct, 
                       main=mainTitle, filename=filename,cexRow=cr,cexCol=cc,treeheight=30)
   }else{
     ah <- NMF::aheatmap(exprMat[,-c(1:3)], scale="row",Rowv=rowV, Colv=colV, 
                         main=mainTitle, filename=filename,cexRow=cr,cexCol=cc,treeheight=30)
   }
   return(ctr)
}
