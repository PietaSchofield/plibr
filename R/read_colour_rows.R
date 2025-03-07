#' Read excel rows of a given colour
#'
#' @import reticulate
#'
#' @export
read_colour_rows <- function(xlsxFile,sheets,colour_code){
  if(!reticulate::py_module_available("pandas")){
    reticulate::py_install("pandas")
  }
  if(!reticulate::py_module_available("openpyxl")){
    reticulate::py_install("openpyxl")
  }
  reticulate::source_python(system.file("py","get_colour_rows.py",package="plibr"))
  read_colour_rows(xlsxFile,sheets,colour_code)
}
