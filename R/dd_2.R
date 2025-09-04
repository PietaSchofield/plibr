#' Display data as DT in HTML and kable in PDF (auto-switch)
#'
#' @title dd2
#'
#' @description
#' Renders a data frame as an interactive DT table when knitting to HTML,
#' and as a LaTeX-friendly kable when knitting to PDF (or other non-HTML outputs).
#' Falls back to kable for non-HTML formats. Optionally limits rows, adds export
#' buttons in HTML, formats non-integer numerics to a fixed number of significant
#' figures, and supports fixed headers/columns in DT.
#'
#' Adaptation of display_data
#'
#' @details
#' Output type selection:
#' - If `table_type = "auto"` (default), the function uses
#'   `knitr::is_html_output()` to choose DT for HTML, otherwise kable.
#' - For LaTeX/PDF, kable is produced with `booktabs = TRUE` and `longtable = TRUE`.
#'
#' HTML behaviour (DT):
#' - If `limited = TRUE`, a fixed-height scrolling area is used (`scrollY = "50vh"`).
#' - `buttons = TRUE` adds "copy" and "csv" export buttons.
#' - Fixed header uses the FixedHeader extension (`fixh = TRUE`).
#' - Fixed columns use the FixedColumns extension via `fixc` (e.g., `list(leftColumns = 1)`).
#' - All non-integer numeric columns are rounded with `DT::formatRound()` to `sigf` digits.
#'
#' Interactive preview:
#' - When running interactively *outside* a knit (e.g., in the console),
#'   the widget is saved to a temporary HTML file and opened in a browser.
#'
#' @param dataset A data frame or tibble to display.
#' @param number Integer or NULL. If not NULL, shows only the first `number` rows.
#' @param table_type Character. One of `"auto"`, `"DT"`, `"kable"`, or `"flextable"`.
#'   `"auto"` chooses DT for HTML and kable otherwise.
#' @param limited Logical. If TRUE, constrain HTML tables to a fixed-height container.
#' @param buttons Logical. If TRUE, include export buttons (HTML/DT only).
#' @param plen Integer. Rows per page in DT (HTML). If NULL, defaults to 10.
#' @param caption Optional character caption for the table.
#' @param fixh Logical. Fixed header for DT (HTML).
#' @param fixc List or NULL. Fixed columns options for DT (e.g., `list(leftColumns = 1)`).
#'   Set `NULL` to disable.
#' @param sigf Integer. Significant figures for non-integer numeric columns in DT.
#' @param dbug Logical. If TRUE, sets internal defaults useful for manual testing.
#'
#' @return
#' For HTML: an `htmlwidget` (DT datatable).
#' For PDF/LaTeX or non-HTML: a `knitr_kable` object.
#' For `table_type = "flextable"`: a `flextable` object.
#'
#' @importFrom DT datatable formatRound
#' @importFrom knitr kable
#' @importFrom htmlwidgets saveWidget
#' @importFrom utils browseURL
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-select: DT in HTML, kable in PDF
#' display_data(mtcars)
#'
#' # Limit to 15 rows and sort/format in DT (HTML)
#' display_data(mtcars, number = 15, buttons = TRUE, sigf = 2)
#'
#' # Force kable regardless of output format
#' display_data(mtcars, table_type = "kable")
#' }
dd2 <- function(dataset, number = NULL, table_type = "auto", limited = FALSE,
                         buttons = FALSE, plen = NULL, caption = NULL,
                         fixh = TRUE, fixc = list(leftColumns = 1), sigf = 3, dbug = FALSE) {

  if (dbug) {
    number <- NULL
    table_type <- "DT"
    limited <- FALSE
    buttons <- FALSE
    plen <- NULL
    caption <- NULL
    fixh <- TRUE
    fixc <- list(leftColumns = 1)
    sigf <- 3
  }

  # Row limiting
  if (!is.null(number)) {
    dataset <- dataset %>% tibble::as_tibble() %>% head(number)
  }

  # Page length for DT
  if (is.null(plen)) {
    plen <- 10
  } else if (!is.numeric(plen)) {
    plen <- nrow(dataset)
  }

  # Auto-select output type
  if (identical(table_type, "auto")) {
    table_type <- if (knitr::is_html_output()) "DT" else "kable"
  }

  # Identify non-integer numeric columns (for DT rounding)
  non_int_numeric_cols <- names(dataset)[sapply(dataset, function(x) {
    is.numeric(x) && any(x != as.integer(x))
  })]

  if (table_type == "DT") {
    # Validate fixed columns option
    if (!is.list(fixc) || is.null(fixc$leftColumns) || any(is.na(fixc$leftColumns))) {
      fixc <- NULL
    }

    # Decide which DT extensions to activate
    exts <- character(0)
    if (buttons) exts <- c(exts, "Buttons")
    if (isTRUE(fixh)) exts <- c(exts, "FixedHeader")
    if (!is.null(fixc)) exts <- c(exts, "FixedColumns")

    dom <- if (buttons) "Blfrtip" else "lfrtip"
    scrollY_val <- if (isTRUE(limited)) "50vh" else NULL

    options_list <- Filter(Negate(is.null), list(
      dom = dom,
      lengthMenu = list(c(10, 50, 100, -1), c("10", "50", "100", "All")),
      paging = TRUE,
      scrollX = TRUE,
      scrollY = scrollY_val,
      pageLength = plen,
      fixedHeader = isTRUE(fixh),
      fixedColumns = if (!is.null(fixc)) fixc else NULL
    ))
    if (buttons) options_list$buttons <- c("copy", "csv")

    widget <- DT::datatable(
      dataset,
      caption = caption,
      options = options_list,
      extensions = exts,
      fillContainer = !isTRUE(limited),
      escape = FALSE,
      rownames = FALSE
    )

    if (length(non_int_numeric_cols) > 0) {
      widget <- DT::formatRound(widget, columns = non_int_numeric_cols, digits = sigf)
    }

    is_rendering <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))
    if (!is_rendering && interactive()) {
      widgetfile <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, widgetfile, selfcontained = TRUE)
      utils::browseURL(widgetfile)
      invisible(widget)
    } else {
      return(widget)
    }

  } else if (table_type == "kable") {
    if (knitr::is_latex_output()) {
      return(knitr::kable(dataset, format = "latex", booktabs = TRUE, longtable = TRUE, caption = caption))
    } else {
      return(knitr::kable(dataset, format = "pipe", caption = caption))
    }

  } else if (table_type == "flextable") {
    return(flextable::qflextable(dataset))

  } else {
    return(dataset)
  }
}

