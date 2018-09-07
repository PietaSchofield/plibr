#' gather alignment statistic on a directory of star alignments
#'
#' @param projName project names
#' @param fileList list of log files
#' @param locDir location for (temporary) local storage of log files
#' 
#' @export
summariseSTAR <- function(projName,fileList,localDir=T){
  ret <- lapply(fileList,function(fn){
    if(!localDir){
      tmpFile <- plib::getFiles(filenames=fn,projName=projName)
    }else{
      tmpFile <- fn
    }
    res <- read.delim(tmpFile,sep="\t",head=F,row.names=1,stringsAsFactors=F)
    rownames(res) <- gsub("[%]","Percent",gsub("[|]","",gdata::trim(rownames(res))))
    rownames(res) <- gdata::trim(gsub("[|]","",rownames(res)))
    rownames(res) <- gsub(" ","_",rownames(res))
    t(res)
  })
  names(ret) <- gsub("Log[.]final[.]out$","",basename(fileList))
  ret <- plyr::ldply(ret)
  rownames(ret) <- make.names(ret[,1],unique=T)
  ret <- ret[,-1]
  colnames(ret) <- c("Start_Job","Start_Mapping","Finish_Job","Mapping_Speed_MRPH",
                     "Input_Reads","Ave_Read_Length",
                     "UNIQUE","No_Unique_Map","Perc_Unique_Map","Ave_Map_Length",
                     "Splice_Total","Splice_Annotated","Splice_GT_AG","Splice_GC_AG",
                     "Splice_AT_AC","Splice_Non-canonical",
                     "Mismatch_rate_Perc","Deletion_rate","Ave_Del_Length","Insert_rate",
                     "Ave_Ins_Length",
                     "MULTIMAP","Multimap_read","Perc_Multimap","Overmap_reads","Perc_Overmap",
                     "UNMAPPED","Unmap_mismatches","Unmap_length","Unmap_other",
                     "CHIMERIC","Chimeric_reads","Perc_Chimeric")
  ret
}
