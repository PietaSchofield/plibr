#' display datatable with different table types
#'
#' @param dataset the data to display
#' @param number the maximum number of rows to display
#' @param table_type the type of table to use ("DT", "kable", "flextable")
#' @param limited constrain to container
#' @param row_names display row names
#' @param buttons include export buttons
#' @param plen page length for DT
#' @param caption table caption
#' @param dbug debugging flag
#' @param fixh fix header
#' @param fixc fix column options
#' @param sigf significant figures for numeric formatting
#'
#' @importFrom DT datatable formatRound
#' @importFrom tibble as_tibble
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom flextable qflextable
#'
#' @export

display_data <- function(dataset, number = NULL, table_type = "DT", limited = FALSE, 
                         row_names = FALSE, buttons = FALSE, plen = NULL, caption = NULL, 
                         dbug = FALSE, fixh = TRUE, fixc = list(leftColumns = 1), sigf = 3) {
  
  # Limit rows if specified
  if (!is.null(number)) {
    dataset <- tibble::as_tibble(dataset) %>% head(number)
  }

  # Set default pagination length
  plen <- ifelse(is.null(plen) || !is.numeric(plen), 10, plen)
  
  # Identify numeric columns with non-integer values
  non_int_numeric_cols <- names(dataset)[sapply(dataset, function(x) {
    is.numeric(x) && any(!is.na(x) & x != as.integer(x))
  })]
  
  if (dbug) print(non_int_numeric_cols)  # Debugging output
  
  # Select table rendering method
  if (table_type == "DT") {
    options_list <- list(
      dom = ifelse(buttons, 'Blfrtip', 'lfrtip'),
      buttons = if (buttons) c("copy", "csv") else NULL,
      lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
      paging = TRUE, scrollX = TRUE, scrollY = TRUE, pageLength = plen,
      fixHeader = fixh, fixColumns = fixc,
      initComplete = htmlwidgets::JS("function(settings, json) {",
                                    "$(this.api().table().header()).css({'color': '#A1B2B2'});",
                                    "$(this.api().table().body()).css({'color': '#93A1A1'});",
                                    "}")
    )
    
    dt_table <- DT::datatable(dataset, caption = caption, options = options_list,
                              fillContainer = limited, escape = FALSE, rownames = row_names)
    
    if (length(non_int_numeric_cols) > 0) {
      return(DT::formatRound(dt_table, columns = non_int_numeric_cols, digits = sigf))
    }
    return(dt_table)
  } 
  else if (table_type == "kable") {
    return(knitr::kable(dataset, format = "html", caption = caption) %>%
             kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")))
  } 
  else if (table_type == "flextable") {
    return(flextable::qflextable(dataset))
  } 
  else {
    stop("Invalid table_type. Choose 'DT', 'kable', or 'flextable'.")
  }
}

