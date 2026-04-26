#' Get and excel sheet with colours
#'
#'
#' @export
get_sheet_with_colour <- function(file, sheet) {

  # read the sheet data normally
  dat <- readxl::read_excel(file, sheet = sheet, col_types="text")

  # read cell metadata
  cells <- tidyxl::xlsx_cells(file, sheets = sheet)
  formats <- tidyxl::xlsx_formats(file)

  # colour lookup table
  fills <- formats$local$fill$patternFill$fgColor$rgb

  # get colours for column A
  colA <- cells |>
    filter(col == 1) |>
    mutate(fill_colour = fills[local_format_id],
           fill_colour = substr(fill_colour, 3, 8),) |>
    select(row, fill_colour)

  # join colour to data
  dat |>
    mutate(row = row_number() + 1) |>
    left_join(colA, by = "row") |>
    select(-row)
}
