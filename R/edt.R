#' edit a file
#'
#' @param filename filename to edit
#' @param rstudio open in Rstudio (if not open in MacVim)
#'
#' @export
edt <- function(filename,editor="rs"){
  switch(editor,
    ri={utils::file.edit(filename)},
    vi={system(paste0("open -a /Applications/MacVim.app ",filename))},
    {file.edit(filename)}
  )
}
