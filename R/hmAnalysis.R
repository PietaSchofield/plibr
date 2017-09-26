#' use heatmap and tree cutting
#'
#' @export
hmAnalysis <- function(exprMat,rowV=NULL,colV=NULL,height=NA,width=NA,ch=NA,cw=NA,leg=T,al=T,
                       ncuts=NULL,filename=NA,mainTitle="Heatmap",cr=1.5,cc=0.5,scale=scale){
   mainTitle <- paste0(mainTitle,"\n",length(rownames(exprMat))," significant DE genes")
   if(is.dendrogram(rowV) & !is.null(ncuts)){
     ct <- as.data.frame(dendextend::cutree(rowV,ncuts))
     colnames(ct) <- "Group"
     ct$Group <- as.factor(ct$Group)
     ctr <- merge(exprMat[,c(1:3)],ct,by="row.names")
     rownames(ctr) <- ctr[,1]
     ctr <- ctr[,-1]
     anR <- ct$Group
     names(anR) <- rownames(anR)
     ah <- NMF::aheatmap(exprMat[,-c(1:3)], scale=scale,Rowv=rowV, Colv=colV, annRow=ct,
                         annLegend=al,
                         height=height,width=width,cellwidth=cw,cellheight=ch,legend=leg, 
                         main=mainTitle, filename=filename,cexRow=cr,cexCol=cc,treeheight=30)
   }else{
     ah <- NMF::aheatmap(exprMat[,-c(1:3)], scale=scale,Rowv=rowV, Colv=colV,legend=leg,
                         height=height,width=width,cellwidth=cw,cellheight=ch, 
                         main=mainTitle, filename=filename,cexRow=cr,cexCol=cc,treeheight=30)
   }
   return(ctr)
}
