#' evalCluster
#' 
#' function from etv1 projects not totally sure when I wrote this I have no memory of it
#' It seems to take the results from the glmnet modelling to perform a KM survival analysis on a
#' set of expression values split by cluster don't really know what the gcl parameter is all abou
#' yet
#'
#' @param expData is an expression matrix rows=genes cols=samples
#' @param survData is a survival data frame with columns time and status
#' @param hm generate heatmap boolean
#' @param gcl if generating heatmap do or don't cluster the genes as well as the samples
#' @param gcts the number of cuts to attempt of the cluster
#' @param cmeth hierarchical clustering method (distance is correlation)
#'
#' @export
evalCluster2 <- function(expData,survData,hm=F,gcl=F,gcts=10,cmeth="ward.D2",titletxt=""){
  hcls <- hclust(as.dist(1-lsa::cosine(expData)),method=cmeth)
  hclslm <- as.data.frame(cutree(hcls,gcts))
  colnames(hclslm) <- "Cluster"
  hclslm$Cluster <- as.factor(hclslm$Cluster)
  sipaug <- merge(survData,hclslm,by="row.names")
  rownames(sipaug) <- sipaug[,1]
  sipaug <- sipaug[,-1]
  fitkm <- survival::survfit(Surv(time,status)~Cluster,sipaug)
  fitdf <- survival::survdiff(Surv(time,status)~Cluster,sipaug)
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
                      cluster_rows=hclg,annotation_col=sipaug[,c("Cluster","Subtype")],
                      cex=0.75,breaks=breaks,show_colnames=F,silent=T,main=titletxt)
  }else{
    phm <- NULL
  }
  list(dat=sipaug,phm=phm,km=fitkm,dif=fitdf)
}
