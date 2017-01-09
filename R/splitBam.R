#' a routine to split and filter by insert size strand specific bam files
#'
#' @param bamFiles and list of fully qualified bamfile names
#' @param len length of insert to split by
#'
#' @export 
splitBAMS <- function(bamFiles,len=150,projName,outDir=NULL,stranded=1){
  lapply(bamFiles,function(fn){
    if(!is.null(outDir)){
      ofn <- gsub(dirname(fn),outDir,fn)
    }else{
      ofn <- fn
    }
    if(stranded>0){
      fw1lng <- gsub("[.]bam$","_fw1_long.bam",ofn)
      fw2lng <- gsub("[.]bam$","_fw2_long.bam",ofn)
      rv1lng <- gsub("[.]bam$","_rv1_long.bam",ofn)
      rv2lng <- gsub("[.]bam$","_rv2_long.bam",ofn)
      fw1sht <- gsub("[.]bam$","_fw1_short.bam",ofn)
      fw2sht <- gsub("[.]bam$","_fw2_short.bam",ofn)
      rv1sht <- gsub("[.]bam$","_rv1_short.bam",ofn)
      rv2sht <- gsub("[.]bam$","_rv2_short.bam",ofn)
      fwlng <- gsub("[.]bam$","_forward_long.bam",ofn)
      rvlng <- gsub("[.]bam$","_reverse_long.bam",ofn)
      fwsht <- gsub("[.]bam$","_forward_short.bam",ofn)
      rvsht <- gsub("[.]bam$","_reverse_short.bam",ofn)
      script <- c(
        paste0('mkdir -p ',dirname(ofn)),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
               ' and second_of_pair and reverse_strand " -f bam ',fn,' -o ',fw1lng),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
              ' and first_of_pair and mate_is_reverse_strand " -f bam ',fn,' -o ',fw2lng),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
              ' and second_of_pair and mate_is_reverse_strand " -f bam ',fn,' -o ',rv1lng),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
               ' and first_of_pair and reverse_strand " -f bam ',fn,' -o ',rv2lng),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' and second_of_pair and reverse_strand " -f bam ',fn,' -o ',fw1sht),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' and first_of_pair and mate_is_reverse_strand " -f bam ',fn,' -o ',fw2sht),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' and second_of_pair and mate_is_reverse_strand " -f bam ',fn,' -o ',rv1sht),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' and first_of_pair and reverse_strand " -f bam ',fn,' > ',rv2sht),
        paste0("sambamba merge ",fwlng," ",fw1lng," ",fw2lng),
        paste0("sambamba merge ",rvlng," ",rv1lng," ",rv2lng),
        paste0("sambamba merge ",fwsht," ",fw1sht," ",fw2sht),
        paste0("sambamba merge ",rvsht," ",rv1sht," ",rv2sht),
        paste0("sambamba index ",fwlng),
        paste0("sambamba index ",rvlng),
        paste0("sambamba index ",fwsht),
        paste0("sambamba index ",rvsht),
        paste0("rm ",fw1lng," ",fw2lng," ",rv1lng," ",rv2lng),
        paste0("rm ",fw1sht," ",fw2sht," ",rv1sht," ",rv2sht)
      )
    }else if(stranded<0){
      fw1lng <- gsub("[.]bam$","_fw1_long.bam",ofn)
      fw2lng <- gsub("[.]bam$","_fw2_long.bam",ofn)
      rv1lng <- gsub("[.]bam$","_rv1_long.bam",ofn)
      rv2lng <- gsub("[.]bam$","_rv2_long.bam",ofn)
      fw1sht <- gsub("[.]bam$","_fw1_short.bam",ofn)
      fw2sht <- gsub("[.]bam$","_fw2_short.bam",ofn)
      rv1sht <- gsub("[.]bam$","_rv1_short.bam",ofn)
      rv2sht <- gsub("[.]bam$","_rv2_short.bam",ofn)
      fwlng <- gsub("[.]bam$","_forward_long.bam",ofn)
      rvlng <- gsub("[.]bam$","_reverse_long.bam",ofn)
      fwsht <- gsub("[.]bam$","_forward_short.bam",ofn)
      rvsht <- gsub("[.]bam$","_reverse_short.bam",ofn)
      script <- c(
        paste0('mkdir -p ',dirname(ofn)),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
               ' and second_of_pair and reverse_strand " -f bam ',fn,' -o ',rv1lng),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
              ' and first_of_pair and mate_is_reverse_strand " -f bam ',fn,' -o ',rv2lng),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
              ' and second_of_pair and mate_is_reverse_strand " -f bam ',fn,' -o ',fw1lng),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
               ' and first_of_pair and reverse_strand " -f bam ',fn,' -o ',fw2lng),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' and second_of_pair and reverse_strand " -f bam ',fn,' -o ',rv1sht),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' and first_of_pair and mate_is_reverse_strand " -f bam ',fn,' -o ',rv2sht),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' and second_of_pair and mate_is_reverse_strand " -f bam ',fn,' -o ',fw1sht),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' and first_of_pair and reverse_strand " -f bam ',fn,' > ',fw2sht),
        paste0("sambamba merge ",fwlng," ",fw1lng," ",fw2lng),
        paste0("sambamba merge ",rvlng," ",rv1lng," ",rv2lng),
        paste0("sambamba merge ",fwsht," ",fw1sht," ",fw2sht),
        paste0("sambamba merge ",rvsht," ",rv1sht," ",rv2sht),
        paste0("sambamba index ",fwlng),
        paste0("sambamba index ",rvlng),
        paste0("sambamba index ",fwsht),
        paste0("sambamba index ",rvsht),
        paste0("rm ",fw1lng," ",fw2lng," ",rv1lng," ",rv2lng),
        paste0("rm ",fw1sht," ",fw2sht," ",rv1sht," ",rv2sht)
      )
    }else{
      fnlng <- gsub("[.]bam$","_long.bam",ofn)
      fnsht <- gsub("[.]bam$","_short.bam",ofn)
      script <- c(
        paste0('mkdir -p ',dirname(ofn)),
        paste0('sambamba view -F "(template_length > ',len,' or template_length < ',-len,') ',
               ' " -f bam ',fn,' -o ',fnlng),
        paste0('sambamba view -F "(template_length <= ',len,' and template_length >= ',-len,') ',
               ' " -f bam ',fn,' -o ',fnsht),
        paste0("sambamba index ",fnlng),
        paste0("sambamba index ",fnsht)
      )
    }
    CRUKlib::runScript(jname=paste0("splitBams_",basename(fn)),jproj=projName,jscrp=script,
                     jdesc=paste0("Split strand specific bam files by fragment alignment"))
  })
}
