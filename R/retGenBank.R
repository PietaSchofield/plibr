#' Retrieve GENBANK details
#'
#' @param gid genbank id
#'
#' @export
retGenBank <- function(gid,getfullseq=FALSE){
  xmlDoc <- reutils::content(reutils::efetch(reutils::esearch(gid,db="nuccore"),
                                             rettype="native",retmode="xml"))
  ft<-XML::xmlRoot(xmlDoc)[[1]][["GBSeq_feature-table"]]
  features <- .getFeatures()
  XML::xmlEventParse(as(ft,"character"), list(), branches=features,asText=T)
  df<- plyr::ldply(features$getStore())
  if(getfullseq){
    gr <- GenomicRanges::GRanges(
            seqnames="chromosome",
            ranges=IRanges::IRanges(
              start=as.numeric(as.character(df$start)),
              end=as.numeric(as.character(df$end))),
            strand=df$strand,name=df$product)

    sq<-Biostrings::DNAStringSet(XML::xmlValue(XML::xmlRoot(xmlDoc)[[1]][["GBSeq_sequence"]])[[1]])
    ft$dna <- sapply(gr, function(r) getSeq(sq,r))
  }
  df

}

.getFeatures <- function() {
  store <- new.env()
  GBFeature <- function(x, ...) {
    if(XML::xmlValue(x[["GBFeature_key"]])=="CDS"){
      start<-unlist(XML::xpathApply(x,".//GBInterval_from",XML::xmlValue))
      end<-unlist(XML::xpathApply(x,".//GBInterval_to",XML::xmlValue))
      if(as.numeric(start)>as.numeric(end)){
        tmp<-start
        start<-end
        end<-tmp
        strand="-"
      }else{
        strand="+"
      }
      quals<-unlist(XML::xpathApply(x,".//GBQualifier",function(y){
        t(unlist(XML::xpathApply(y,".//GBQualifier_value",XML::xmlValue)))
      }))
      names(quals)<-unlist(XML::xpathApply(x,".//GBQualifier",function(y){
        t(unlist(XML::xpathApply(y,".//GBQualifier_name",XML::xmlValue)))
      }))
      store[[unname(quals["product"])]] <- cbind(start=start,end=end,strand=strand,t(quals))
    }
  }
  getStore <- function() as.list(store)
  list(GBFeature=GBFeature, getStore=getStore)
}
