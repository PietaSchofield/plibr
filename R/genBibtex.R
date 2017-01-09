#' genbibtex
#'
#' @param packages package names to add citations from
#' @param outfile file of bibtex file to add citations to
#'
#' @export
genBibtex <- function(packages, db=F, overwrite=TRUE,
                      outfile="/Users/pschofield/git_tree/biblio/rpackages.bib"){
  if(overwrite){
    unlink(outfile)
  }
  res <- lapply(packages,
           function(cc){
            if(!db & !is.null(outfile)){
              cit <- .addcitekey(cc)
              lapply(cit,
                function(cn){
                  write(cn,file=outfile,append=T)
                })
            } else{
              paste0(cc,listcitekeys(cc))
            }
           })
  if(db) res
}



