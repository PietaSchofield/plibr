#' read a transfact PWM file
#'
#' @param file the file name
#'
#' @export
readTRANSFACT <- function (file) {
    table <- read.table(file = file, fill = T, stringsAsFactors=F)
    table$V1[is.na(table$V1)]<-"NM"
    table.vector <- cbind(as.vector(table[[1]]), as.vector(table[[2]]), 
        as.vector(table[[3]]), as.vector(table[[4]]), as.vector(table[[5]]),
        as.vector(table[[6]]))
    DT <- which(table.vector == "NM")
    names <- table.vector[DT,2]
    DT <- c(DT,length(table[[1]])-1)
    listPWM <- lapply(seq(length(DT)-1),
                 function(x){
                   st <- table.vector[DT[x]:DT[x+1],]
                   rt <- list()
                   rt[["motif"]] <- st[which(st[,1]=="DE"),2]
                   rt[["pwm"]] <- as.numeric(st[which(grepl("^[0-9]*$",st[,1])), c(2:5)])
                   rt[["pwm"]] <- matrix(rt[["pwm"]],nrow=4,byrow=T,
                                         dimnames = list(c("A", "C", "G", "T")))
                   rt
                 })
    names(listPWM) <- names
    return(listPWM)
}
