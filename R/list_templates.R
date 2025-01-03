#' list tempates
#'
#' @export
list_templates <- function(package="plibr") {
  if(is.null(package)){
    all_packages <- installed.packages()[, "Package"]
  }else{
    all_packages <- c(package)
  }
  templates <- lapply(all_packages, function(pkg) {
    package_path <- find.package(pkg, quiet = TRUE)
    templates_path <- file.path(package_path, "rmarkdown", "templates")
    if (dir.exists(templates_path)) {
      return(list(pkg = pkg, templates = list.dirs(templates_path, recursive = FALSE, full.names = FALSE)))
    } else {
      return(NULL)
    }
  })
  
  templates <- do.call(rbind,
        lapply(templates, function(x){
          if (!is.null(x)) data.frame(Package = x$pkg, Template = x$templates)
        }))
  
  return(templates)
}
