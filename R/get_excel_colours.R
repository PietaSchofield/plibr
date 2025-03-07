#' Get excel sheet colours
#'
#' @import reticulate
#'
#' @export
get_excel_colours <- function(xlsxFile,sheets){
  if(!reticulate::py_module_available("openpyxl")){
    reticulate::py_install("openpyxl")
  }
  reticulate::source_python(system.file("py","find_colour_codes.py",package="plibr"))
  find_colour_codes(xlsxFile,sheets)
}
