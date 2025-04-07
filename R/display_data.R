#' Display Data Table in Multiple Formats
#'
#' This function renders a dataset as an interactive datatable (DT), a kable table, or a flextable,
#' depending on the specified output type.
#'
#' @param dataset The data to display.
#' @param number The maximum number of rows to display. Defaults to NULL (displays all rows).
#' @param table_type The type of table output. Options are "DT" (interactive table), "kable" (Markdown/PDF), and "flextable" (Word/PowerPoint).
#' @param limited Logical. If TRUE, constrains the table to a fixed container size. Defaults to FALSE.
#' @param buttons Logical. If TRUE, includes export buttons (only applies to "DT" output).
#' @param plen The number of rows per page in DT tables. Defaults to 10.
#' @param caption Optional character string to include as a table caption.
#' @param fixh Logical. If TRUE, enables fixed headers in DT tables.
#' @param fixc List specifying column fixing options for DT tables. Defaults to left column fixed.
#' @param sigf Numeric. Number of significant figures for numeric formatting.
#'
#' @return A data table formatted as specified by `table_type`.
#' @importFrom DT datatable
#' @importFrom knitr kable
#' @importFrom flextable qflextable
#' @export

display_data <- function(dataset, number = NULL, table_type = "DT", limited = FALSE, 
                         buttons = FALSE, plen = NULL, caption = NULL, 
                         fixh = TRUE, fixc = list(leftColumns = 1), sigf = 3,dbug=FALSE) {
  if(dbug){
    dataset = cvdall
    number = NULL
    table_type = "DT"
    limited = FALSE
    buttons = FALSE
    plen = NULL
    caption = NULL 
    fixh = TRUE
    fixc = list(leftColumns = 1)
    sigf = 3
  }
  if (!is.null(number)) {
    dataset <- dataset %>% tibble::as_tibble() %>% head(number)
  }

  if (is.null(plen)) {
    plen = 10
  } else if (!is.numeric(plen)) {
    plen = nrow(dataset)
  }

  non_int_numeric_cols <- names(dataset)[sapply(dataset, function(x) {
    is.numeric(x) && any(x != as.integer(x))
  })]

  if (table_type == "DT") {
    # Validate fixc to prevent errors
    if (!is.list(fixc) || is.null(fixc$leftColumns) || any(is.na(fixc$leftColumns))) {
      fixc <- NULL
    }
    
    if (buttons) {
      ext <- 'Buttons'
      btns <- c("copy", "csv")
      dom <- 'Blfrtip'
    } else {
      ext <- character(0)
      btns <- NULL
      dom <- 'lfrtip'
    }

    options_list <- list(
      dom = dom,
      lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
      paging = TRUE, scrollX = TRUE, scrollY = TRUE, pageLength = plen,
      fixHeader = fixh, fixColumns = if (!is.null(fixc)) fixc else NULL
    )
    
    if (buttons) {
      options_list$buttons <- btns
    }

    widget <- DT::datatable(dataset, caption = caption, options = options_list, extension=ext,
                         fillContainer = limited, escape = FALSE, rownames = FALSE)

    if (!is.null(non_int_numeric_cols) && length(non_int_numeric_cols) > 0
        && !any(is.na(non_int_numeric_cols))) {
      widget <- DT::formatRound(widget, columns = non_int_numeric_cols, digits = sigf)
    }

    is_rendering <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))

    if(!is_rendering && interactive()){
      widgetfile <- tempfile(fileext=".html")
      htmlwidgets::saveWidget(widget, widgetfile,selfcontained=TRUE)
      displayURL(widgetfile)
    }else{
      return(widget)
    }
  } else if (table_type == "kable") {
    knitr::kable(dataset, format = "pipe")
  } else if (table_type == "flextable") {
    flextable::qflextable(dataset)
  } else {
    dataset
  }
}

