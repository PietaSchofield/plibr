#' Plot CNV profiles
#'
#' There are 6 states, which in a diploid sample corresponds to the following
#' chromosomal copies and biological state:
#'  1  <= 0 copies, homozogous deletion
#'  2  == 1 copy, heterozogous deletion
#'  3  == 2 copies, neutral
#'  4  == 3 copies, gain
#'  5  == 4 copies, amplification
#'  6  >= 5 copies, high level amplification
#' Output:
#' states - The state assigned to each copy number value
#' segs  - Non-overlapping segments and medians of each segment
#' mus  - Optimal median of of copy numbers in state
#' lambda - Optimal precision of copy numbers in state
#' pi   - Optimal state distribution
#' loglik - The likelihood values of each EM algorithm iteration
#' rho  - Posterior marginals (responsibilities) for each position and state
#'
#' @param copyData list of RangedData Objects with copy number
#' @param segData list of corresponding segmentation data
#' @param param list of initial parameter
#'
#' @export
plotCNV <- function(copyData,segData,paramList){
  pl <- paramList
  dsdf <- reshape2::dcast(
          plyr::ldply(lapply(copyData,function(c){
            as(c,"data.frame")[,c("space","start","end","copy")]
          }),stringsAsFactors=F),space+start+end~.id,value.var="copy")

  nsample <- ncol(dsdf)-3
  yLimit <- range(apply(dsdf[,4:ncol(dsdf)],2,max,na.rm=T))
  maxCNVToPlot <- max(abs(yLimit))
  if( maxCNVToPlot > 3 ){
    yLimit <- c(-2.5,2.5)
  }else{
    yLimit <- c(-maxCNVToPlot, maxCNVToPlot)
  }
  ploidyNumberToShow <- c(0,1,2,3,4,5)
  ratioPosition <- log2(as.numeric(unlist(strsplit(pl[["segMu"]],",", fixed=TRUE)))/2)

# Plot all CNV as individual track in one PDF
  height.compactPlot <- ceiling((ncol(dsdf)-3) * pl[["pltHFactor"]])
  width.compactPlot = 15
  outFile <- file.path(pl[["outDir"]],paste0(pl[["analysisType"]],".pdf"))
  pdf(outFile,height=height.compactPlot, width=width.compactPlot)
    layout(matrix(1:(ncol(dsdf)-3), nrow=(ncol(dsdf)-3), ncol=1))
    par(oma=c(2,4,3,12), cex.axis=0.8)
    for(sn in pl[["plotOrder"]]){
      # Linearise the copy number positions for plotting
      chrLength <- data.frame(pl[["seqLen"]])
      colnames(chrLength) <- c("size")
      chrLength$chr <- rownames(chrLength)
      chrLength$offset <- cumsum(c(0, chrLength$size[-nrow(chrLength)]))
      gpos <- dsdf$start
      for(i in 1:nrow(chrLength)){
        idx <- as.character(dsdf$space) == as.character(chrLength[i, "chr"])
        gpos[idx] <- gpos[idx] + chrLength[i, "offset"]        
      }
      midpoint <- vector("numeric", nrow(chrLength))
      for(n in 1:nrow(chrLength)){
        if(n == nrow(chrLength)){
          midpoint[n] <- 0.5 * (chrLength[n, "offset"] + max(cumsum(as.numeric(chrLength$size))))
        }else{
          midpoint[n] <- 0.5 * (chrLength[n, "offset"] + chrLength[n+1, "offset"])
        }
      }
      segs <- as(segData[[sn]]$segs,"data.frame") 
      # Linearise the segmentation position for plotting
      loc.start <- segs$start
      loc.end <- segs$end
      for(i in 1:nrow(chrLength)){
        idx <- as.character(segs$chr) == as.character(chrLength[i, "chr"])
        loc.start[idx] <- loc.start[idx] + chrLength[i, "offset"]
        loc.end[idx] <- loc.end[idx] + chrLength[i, "offset"]  
      }
      segsToPlot <- data.frame(chrom=segs$chr, loc.start, loc.end, loc.mean=segs$median)
      # Plot the corrected copy number
      par(mar=c(2,0,0,0), cex.main=0.9, cex.axis=0.7, las=2)
      plot(gpos, dsdf[,sn], pch=".", xlab="", ylab="", main="", 
           ylim=yLimit, col="grey60", xaxt="n", xaxs="i")
      myColorPalette <- RColorBrewer::brewer.pal(n=11, name="RdBu")
      myStates <- segData[[sn]]$state
      myCol <- rep("grey60", length(myStates))
      myCol[myStates == 1] <- myColorPalette[9]  # 0 copies, homozogous deletion
      myCol[myStates == 2] <- myColorPalette[10]  # 1 copy, heterozogous deletion
      myCol[myStates == 3] <- "grey60"       # 2 copies, neutral
      myCol[myStates == 4] <- myColorPalette[2]  # 3 copies, gain
      myCol[myStates == 5] <- "darkorange"     # 4 copies, amplification
      myCol[myStates == 6] <- myColorPalette[3]  # >= 5 copies, high-level of amplification
      points(gpos, dsdf[,sn], pch=".", col=myCol)
      # Add estimated ploidy number
      axis(side=4, at=ratioPosition, labels=ploidyNumberToShow)
      # Add segmentation trend lines
      chr <- unique(as.character(segsToPlot$chrom))
      for(j in 1:length(chr)){
        x <- segsToPlot[segsToPlot$chrom == chr[j], -1]
        colnames(x) <- c("pos", "pos", "median")
        y <- rbind(x[,-2], x[-1])
        y <- y[order(y$pos), ]
        points(y$pos, y$median, type="l", col="black", lwd=1.5)
      }

      # Add axis labels and additional annotation
      abline(h=0, lty=3, col="black")
      abline(v=chrLength$offset[-1], lty=3, col="black")
      mtext(sn, side=4, adj=0, line=2.8, las=2, cex=0.8)
      # Add chromosome name
      par(las=0)
      axis(side=1, at=midpoint, labels=as.character(chrLength$chr), tick=FALSE, line=-1)
      if(grep(sn,pl[["plotOrder"]]) == ncol(dsdf)){
        par(las=0, cex.axis=0.8)
        if(length(dsdf) > 3){
          mtext(expression(paste("Mappability and GC-corrected read counts (",
                    log[2], ")", sep="")), side=2, line=1.8, outer=TRUE)
        }else{
          # Plot a shorter y-lab, else will run out of space
          mtext(expression(paste("Corrected read counts (", log[2], ")", 
                      sep="")), side=2, line=1.8, outer=TRUE)
        }
      }

      if(grep(sn,pl[["plotOrder"]]) == 1 ){
        par(las=0) 
        mtext(paste("HMMcopy ", pl[["analysisType"]], " sample analysis (window size = ", 
            paramList[["windowSize"]], ")", sep=""), side=3, line=0.8, outer=TRUE)
      }
    }
  dev.off()
}
