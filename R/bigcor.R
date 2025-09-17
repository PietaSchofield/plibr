#' large correlation matrix with ff
#'
#' @param x the matrix
#' @param nblocks required to be factor of ncols x
#' @param verbose report whats going on
#'
#' @export
bigcor <- function(x, nblocks = 10, verbose = TRUE, absolute=F )
{
  NCOL <- ncol(x)
  ## test if ncol(x) %% nblocks gives remainder 0
  if (NCOL %% nblocks != 0) stop("Choose different 'nblocks' so that ncol(x) %% nblocks = 0!")
  ## preallocate square matrix of dimension
  ## ncol(x) in 'ff' single format
  corMAT <- ff::ff(vmode = "single", dim = c(NCOL, NCOL))
  ## split column numbers into 'nblocks' groups
  SPLIT <- split(1:NCOL, rep(1:nblocks, each = NCOL/nblocks))
  ## create all unique combinations of blocks
  COMBS <- expand.grid(1:length(SPLIT), 1:length(SPLIT))
  COMBS <- t(apply(COMBS, 1, sort))
  COMBS <- unique(COMBS)
  ## iterate through each block combination, calculate correlation matrix
  ## between blocks and store them in the preallocated matrix on both
  ## symmetric sides of the diagonal
  for (i in 1:nrow(COMBS)) {
    COMB <- COMBS[i, ]
    G1 <- SPLIT[[COMB[1]]]
    G2 <- SPLIT[[COMB[2]]]
    if (verbose) cat("Block", COMB[1], "with Block", COMB[2], "\n")
    flush.console()
    if(absolute){
      COR <- 1 - abs(cor(x[, G1], x[, G2]))
    }else{
      COR <- 1 - cor(x[, G1], x[, G2])
    }
    corMAT[G1, G2] <- COR
    corMAT[G2, G1] <- t(COR)
    COR <- NULL
  }
  gc()
  return(corMAT)
}
