#' evalCluster
#' 
#' function from etv1 projects not totally sure when I wrote this I have no memory of it
#'
#' @export
evalCluster <- function(expData,survData,hm=F,gcl=F,gcts=10,cmeth="ward.D2"){
  hcls <- hclust(as.dist(1-cor(expData)),method=cmeth)
  hclslm <- as.data.frame(cutree(hcls,2))
  colnames(hclslm) <- "Cluster"
  hclslm$Cluster <- as.factor(hclslm$Cluster)
  sipaug <- merge(survData,hclslm,by="row.names")
  rownames(sipaug) <- sipaug[,1]
  sipaug <- sipaug[,-1]
  fit <- survival::survfit(Surv(time,status)~Cluster,sipaug)
  plt <- plotKM(fit,sipaug)
  breaks=seq(-2, 2, by=0.1) 
  breaks=append(breaks, 10)
  breaks=append(breaks, -10, 0)
  colpal <- colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(length(breaks-1))
  if(gcl){
    hclg <- hclust(as.dist(1-cor(t(expData))),method=cmeth)
    hclglm <- as.data.frame(cutree(hclg,gcts))
    colnames(hclglm) <- "Cluster"
    hclglm$Cluster <- as.factor(hclglm$Cluster)
  }else{
    hclg <- FALSE
    hclglm <- NULL
  }
  if(hm){
    phm <- pheatmap::pheatmap(expData,scale="row",cluster_cols=hcls,color=colpal,
                      cluster_rows=hclg,annotation_col=hclslm,
                      cex=0.75,breaks=breaks,show_colnames=F,silent=T)
  }else{
    phm <- NULL
  }
  list(plt=plt,phm=phm)
}
