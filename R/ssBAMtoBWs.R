#' create strand specific bigwigs from a standed bamfile
#'
#' @param inFile input bam file
#' @param outDir output directory
#' @param projName name of the project
#' 
#' @export
ssBAMtoBWs <- function(inFile, outDir,projName,
                       outRoot=file.path("/scratch/pschofield/Projects", projName),
                       ucscMod="apps/ucscsuite/20150630/linux-x86_64",
                       samtoolsMod="apps/samtools/1.3.1/gcc-4.4.7",
                       bedtoolsMod="apps/bedtools/2.25.0/gcc-4.4.7"){
  intBed <- file.path(outRoot,outDir,gsub("[.]bam",".bed",basename(inFile)))
  plusBG <- file.path(outRoot,outDir,gsub("[.]bam","_plus.bg",basename(inFile)))
  minusBG <- file.path(outRoot,outDir,gsub("[.]bam","_minus.bg",basename(inFile)))
  plusBW <- file.path(outRoot,outDir,gsub("[.]bam","_plus.bw",basename(inFile)))
  minusBW <- file.path(outRoot,outDir,gsub("[.]bam","_minus.bw",basename(inFile)))
  chrSize <- file.path(outRoot,outDir,gsub("[.]bam","_chr.size",basename(inFile)))
  script <- c(
    paste0("mkdir -p ",file.path(outRoot,outDir)),
    paste0("module load ",ucscMod),
    paste0("module load ",samtoolsMod),
    paste0("module load ",bedtoolsMod),
    paste0("bamToBed -i ", inFile, " -split > ", intBed),
    paste0("samtools view -H ", inFile," | grep ^@SQ | sed -e 's/^.*SN://g' | sed -e 's/LN://g'",
           " > ",chrSize), 
    paste0("awk '{if($6==",'"+"',") print}' ", intBed, " | sort -k1,1 | bedItemOverlapCount hg19 ",
           "-chromSize=",chrSize," stdin | sort -k1,1 -k2,2n > ", plusBG),
    paste0("awk '{if($6==",'"-"',") print}' ", intBed, " | sort -k1,1 | bedItemOverlapCount hg19 ",
           "-chromSize=",chrSize," stdin | sort -k1,1 -k2,2n > ", minusBG),
    paste0("bedGraphToBigWig ",plusBG," ",chrSize," ",plusBW),
    paste0("bedGraphToBigWig ",minusBG," ",chrSize," ",minusBW),
    paste0("rm ",intBed),
    paste0("rm ",plusBG),
    paste0("rm ",minusBG),
    paste0("rm ",chrSize)
  )
  runScript(jproj=projName,jname=paste0("ssbam2bw_",basename(inFile)),
                     jdesc="strand specific bam to bigwigs",
                     nproc=1,mem="60Gb",jscrp=script,noSub=T)
}
