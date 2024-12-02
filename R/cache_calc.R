#' cache an value for more efficient rendering
#'
#' get project cache
#'
#' @export
get_cache <- function(projName,rootDir=file.path(Sys.getenv("HOME"),"Projects")){
  cache_dir <- file.path(rootDir,projName,".cache")
  if(!dir.exists(cache_dir)) dir.create(cache_dir, recur=T)
  cachem::cache_disk(dir=cache_dir)
}



