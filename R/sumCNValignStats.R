#' Summarise the mapping statistics information for a specific dataset
#'
#' @param projName project name
#' @param bamDir bam alignment stats directory
#' @param outDir output directory
#'
#' @export
sumCNValignStats <- function(projName,remRoot=file.path("/scratch/pschofield/Projects",projName),
                             mapStatDir="Analysis/mapping_stats",miSeq=TRUE,
                             locDir=file.path("/Users/pschofield/Projects",projName,"Data"),
                             asFilePat="_align.stats",fsFilePat="_insertSize.txt"){
  mapstatFiles <- rlsFiles(file.path(remRoot,mapStatDir,paste0("*",asFilePat)))
  # Combine the mapping statistics
  tmpFiles <- plib::getFiles(mapstatFiles,projName=projName,simple=T,force=T)
  res <- plyr::ldply(lapply(tmpFiles, function(msf){
    m <- read.delim(msf,head=FALSE,stringsAsFactors=F)
    if(miSeq){
      ret <- t(sapply(seq(1,9,2), function(i) c(m[i, 1],m[i+1,1]))) 
      rownames(ret) <- ret[,1]
      as.numeric(ret[,-1])
    }
  }))
  rownames(res) <- gsub(asFilePat, "", basename(mapstatFiles))
  fragSizeFiles <- rlsFiles(file.path(remRoot,mapStatDir,paste0("*",fsFilePat)))
  tmpFiles <- plib::getFiles(filenames=fragSizeFiles,projName=projName,simple=T,force=T)
  # Combine the mapping statistics
  res$median_frag_size <- sapply(tmpFiles, function(msf){
    m <- read.delim(msf,head=FALSE,stringsAsFactors=F)
    median(abs(m$V1))
  })
  colnames(res) <- c("total.reads", "mapped.reads", "unique.reads", "duplicated.reads",
                     "postdedup.unique.reads","median.fragment.size")
  res$`total.reads` <- 2 * res$`total.reads`
  # Create the final table
  outTable <- data.frame(id=rownames(res), subset(res, select=c("total.reads", "mapped.reads")))
  outTable$percent.mapped.reads <- res[,"mapped.reads"]/res[,"total.reads"] * 100
  outTable$prededup.unique.reads <- res[,"unique.reads"]
  outTable$percent.prededup.unique.reads <- signif(res[,"unique.reads"]/res[,"total.reads"] * 100,
                                                 4)
  outTable$duplicated.reads <- res[,"duplicated.reads"]
  outTable$percent.duplicated.reads <- signif(res[,"duplicated.reads"]/res[,"unique.reads"] * 100,
                                                 4)
  outTable$median.fragment.size <- res[,"median.fragment.size"]
  outTable$postdedup.unique.reads <- res[,"postdedup.unique.reads"]
  # Output the table
  require(xlsx)
  outfile = file.path(locDir, paste0(projName, "_QC_metrics.xlsx"))
  wb <- xlsx::createWorkbook(type="xlsx")
  sht <- xlsx::createSheet(wb,paste0(projName,"_align_stats"))
  xlsx::addDataFrame(x=outTable,sheet=sht)
  xlsx::saveWorkbook(wb,file=outfile)
  outTable
}
