#' survival analysis of expression profile clustered data
#'
#' @export
survClustAnal <- function(ed,gd,pd,trim=0.25,scale="col",th=50,
                          dRow="canberra",cRow="average",scaleIt=T,
                          cCol="complete",dCol="correlation",st="time",ss="status",
                          rowA=NULL,rcuts=(nrow(pd)/10),ccuts=(length(gd)/10)){
  # fix gene names this shouldn't be necessary as rownames should be unique!
  rownames(ed) <- make.names(rownames(ed),unique=T)

  # select just genes of interest and antilog? (why antilog)
  if(min(apply(ed,2,min))<0){
    eipSig <- as.matrix(exp(ed[which(rownames(ed)%in%gd),]))
  }else{
    eipSig <- as.matrix(ed[which(rownames(ed)%in%gd),])
  }

  if(scaleIt){
    eipSig <- t(scale(t(eipSig)))
  }

  # generate clusters
  if(dRow=="correlation"){
    rowC <- hclust(as.dist(1-cor(t(eipSig))),method=cRow)
  }else if(dRow=="cosine"){
    rowC <- hclust(as.dist(1-lsa::cosine(t(eipSig))),method=cRow)
  }else{
    rowC <- hclust(dist(eipSig,dRow),method=cRow)
  }
  if(dCol=="correlation"){
    colC <- hclust(as.dist(1-cor(eipSig)),method=cCol)
  }else if(dCol=="cosine"){
    colC <- hclust(as.dist(1-lsa::cosine(eipSig)),method=cCol)
  }else{
    colC <- hclust(dist(t(eipSig),dCol),method=cCol)
  }

  # cut samples by cluster
  sClust <- as.data.frame(cutree(colC,rcuts))
  colnames(sClust) <- "sampleCl"
  sClust$sampleCl <- as.factor(sClust$sampleCl)
  
  # form sample data
  pdata <- merge(pd,sClust,by="row.names")
  rownames(pdata) <- pdata[,1]
  pdata <- pdata[,-1]
  
  # cut gene clusters
  gClust <- as.data.frame(cutree(rowC,ccuts))
  colnames(gClust) <- "geneCl"
  gClust$geneCl <- as.factor(gClust$geneCl)
  
  # filter genes of interest
  gid <- intersect(gd,rownames(eipSig))
  
  # call the fit
  fit <- survClust(survData=pdata,exprData=eipSig,geneData=gd,survStatus=ss,survTime=st,
                   clustCol="sampleCl")
  
  # normalise the data to nice output
  if(trim<1){
    eipPlot <- apply(eipSig,2,function(x){
      y<-x
      y[which(x>quantile(x,(1-trim)))] <- quantile(x,(1-trim))
      y[which(x<quantile(x,trim))] <- quantile(x,trim)
      y
    })
  }else{
    eipPlot <- apply(eipSig,2,function(x){
      y <- x
      y[which(x>trim)] <- trim
      y[which(x< -trim)] <- -trim
      y
    })
  }
  # generate heatmap
  hm=NMF::aheatmap(t(eipPlot),Colv=rowC,Rowv=colC,scale=scale,
                   treeheight=th,annRow=sClust,annCol=gClust)
  list(fit=fit,pdata=pdata,hmap=hm)
}
