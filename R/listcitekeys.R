#' list the citation keys for a package
#'
#' @param the package name
#'
#' @export
lck <- function(packages,title=F){
  if(title){
    unlist(lapply(packages,
      function(pkg){
        cit <- toBibtex(suppressWarnings(citation(pkg)),style="Bibtex")
        begin <- as.list(which(grepl("@",cit)))
        endin <- as.list(c(which(grepl("@",cit)),(length(cit)+1))[-1])
        ncit <- Map(c,begin,endin)
        lapply(ncit,
          function(n){
            cn <- unname(cit[n[1]:(n[2]-1)])
            paste0(.getcitkey(cn)," - ",.getfield(cn,"title"))
          })
      }))
  }else{
    unlist(unname(sapply(packages,
      function(pkg){
        cit <- toBibtex(suppressWarnings(citation(pkg)),style="Bibtex")
        begin <- as.list(which(grepl("@",cit)))
        endin <- as.list(c(which(grepl("@",cit)),(length(cit)+1))[-1])
        ncit <- Map(c,begin,endin)
        unname(sapply(ncit,
          function(n){
            cn <- unname(cit[n[1]:(n[2]-1)])
            .getcitkey(cn)
          }))
      })))
  }
}


