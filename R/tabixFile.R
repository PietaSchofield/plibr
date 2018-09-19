#' run tabix to produce compressed indexed file
#'
#' A simple function to tabix bed and gtf files it might work for other files but I have not tried any so
#' is likely that it will fail.
#'
#' @param infile input file
#' @param outfile output file
#' @param outpath output directory path
#'
#' @export
tabixFile <- function(infile,outfile=NULL,outpath=NULL,form=NULL,sortstr=NULL){
  # consider putting in some idiot proofing
  if(is.null(outpath)){
    outpath <- dirname(infile)
  }
  ext <- gsub("^.*[.]","",basename(infile))
  if(is.null(sortstr)){
    sortstr <- switch(ext,
      bed=paste0("-k1,1 -k2,2n"),
      gtf=paste0("-k1,1 -k4,4n"),
      gff=paste0("-k1,1 -k4,4n"),
      vcf=paste0("-k1,1 -k2,2n"))
  }
  if(is.null(outfile)){
    outfile <- gsub(paste0("[.]",ext,"$"),"",basename(infile))
  }
  outName <- file.path(outpath,paste0(outfile,".",ext,".gz"))
  tabixCmd <- paste0(
    '(grep ^"#" ',infile,'; grep -v ^"#" ',infile,') | sort ',sortstr,' | bgzip > ',outName
  )
  if(ext=="vcf"){
    tabixCmd <- paste0(
      '(grep ^"#" ',infile,'; (grep -v ^"#" ',infile,' | sort ',sortstr,')) | bgzip > ',outName
    )
  }
  system(tabixCmd)
  if(ext=="gtf"){
    ext <- "gff"
  }
  idxCmd <- paste0("tabix -p ",ext," ",outName)
  system(idxCmd)
}
