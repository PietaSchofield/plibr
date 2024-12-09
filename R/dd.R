#' New display data
#'
#' @export
disp_tab <- function(dataset, number = NULL, disp = TRUE, limited = FALSE, buttons = FALSE,
                         plen = NULL, caption = NULL, fixh = TRUE, fixc = list(leftColumns = 1),
                         sigf = 3, wrap_columns = NULL) {
  
  if (!is.null(number)) {
    dataset <- dataset %>% tibble::as_tibble() %>% head(number)
  }
  if (is.null(plen)) {
    plen <- 10
  } else if (!is.numeric(plen)) {
    plen <- nrow(dataset)
  }
  
  # Identify non-integer numeric columns for rounding
  non_int_numeric_cols <- names(dataset)[sapply(dataset, function(x) {
    is.numeric(x) && any(x != as.integer(x))
  })]
  
  # Prepare columnDefs for wrapping
  column_defs <- list()
  if (!is.null(wrap_columns)) {
    wrap_targets <- which(names(dataset) %in% wrap_columns) - 1 # Adjust for zero-based indexing
    column_defs <- lapply(wrap_targets, function(target) {
      list(targets = target, className = "wrap-column")
    })
  }
  
  # Define common options
  dom <- if (buttons) "Blfrtip" else "lfrtip"
  extensions <- if (buttons) "Buttons" else NULL
  buttons_list <- if (buttons) c("copy", "csv") else NULL
  
  options_list <- list(
    dom = dom,
    buttons = buttons_list,
    lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
    paging = TRUE,
    scrollX = TRUE,
    scrollY = TRUE,
    pageLength = plen,
    fixedHeader = fixh,
    fixedColumns = fixc,
    columnDefs = column_defs,
    initComplete = htmlwidgets::JS("function(settings, json) {
      $(this.api().table().header()).css({'color': '#93B2B2'});
      $(this.api().table().body()).css({'color': '#93A1A1'});
    }")
  )
  
  if (disp) {
    dataset %>%
      DT::datatable(
        extensions = extensions,
        caption = caption,
        options = options_list,
        fillContainer = limited,
        escape = FALSE,
        rownames = FALSE
      ) %>%
      DT::formatRound(columns = non_int_numeric_cols, digits = sigf)
  } else {
    dataset %>% tibble::as_tibble()
  }
}

