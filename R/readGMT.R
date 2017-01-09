#' read in an MSigDB file with gene set
#'
#' @param fileName name of the file
#'
#' @export
readGMT <- function(fileName="h.all.v5.2.symbols.gmt",
                    dbDir="/Users/pschofield/Data/GSEA/msigdb_v5.2/msigdb_v5.2_GMTs"){
  lines <- readLines(file.path(dbDir,fileName))  
  db <- lapply(lines,function(line){
    entry <- strsplit(line,"\t")[[1]]
    list(Name=entry[1],
         Link=entry[2],
         Genes=entry[c(-1,-2)])
  })
  names(db) <- sapply(db,"[[",1)
  db
}
