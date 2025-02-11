#' display datatable 
#'
#' displays a datatable (DT) either as raw tibble if not compiling to avoid trying to fire up unnecessary
#' browers or as a DT if rendering to html
#' 
#' @param dataset the data to display
#' @param number the maxium number of rows to display
#' @param disp the flag which decides the whole shebang
#' @param limited contrain to container
#'
#' @import DT
#'
#' @export
display_data <- function(dataset, number = NULL, disp = TRUE, limited = FALSE,
                         buttons = FALSE, plen = NULL, caption = NULL,dbug = FALSE,
                         fixh = TRUE, fixc = list(leftColumns = 1), sigf = 3) {

  if (!is.null(number)) {
    dataset <- dataset %>% tibble::as_tibble() %>% head(number)
  }

  if (is.null(plen)) {
    plen <- 10
  } else if (!is.numeric(plen)) {
    plen <- nrow(dataset)
  }

  # Fix: Ensure no NA values in column selection
  non_int_numeric_cols <- names(dataset)[sapply(dataset, function(x) {
    is.numeric(x) && any(!is.na(x) & x != as.integer(x))
  })]
  non_int_numeric_cols <- non_int_numeric_cols[!is.na(non_int_numeric_cols)]  # Prevent NA issues

  if(dbug){
    print(non_int_numeric_cols)  # Debugging step: See what columns are selected
  }

  if (disp) {
    if (buttons) {
      ext <- 'Buttons'
      btns <- c("copy", "csv")
      dom <- 'Blfrtip'
      dataset %>%
        DT::datatable(extensions = ext, caption = caption,
                      options = list(dom = dom, buttons = btns,
                                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
                                     paging = TRUE, scrollX = TRUE, scrollY = TRUE, pageLength = plen,
                                     fixHeader = fixh, fixColumns = fixc,
                                     initComplete = htmlwidgets::JS("function(settings, json) {",
                                        "$(this.api().table().header()).css({'color': '#93B2B2'});",
                                        "$(this.api().table().body()).css({'color': '#93A1A1'});",
                                                                     "}")),
                      fillContainer = limited, escape = FALSE, rownames = FALSE) %>%
        {
          if (length(non_int_numeric_cols) > 0) {
            DT::formatRound(., columns = non_int_numeric_cols, digits = sigf)
          } else {
            .
          }
        }
    } else {
      dom <- 'lfrtip'
      dataset %>%
        DT::datatable(caption = caption,
                      options = list(dom = dom,
                                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
                                     paging = TRUE, scrollX = TRUE, scrollY = TRUE, pageLength = plen,
                                     fixHeader = fixh, fixColumns = fixc,
                                     initComplete = htmlwidgets::JS("function(settings, json) {",
                                           "$(this.api().table().header()).css({'color': '#A1B2B2'});",
                                           "$(this.api().table().body()).css({'color': '#93A1A1'});",
                                                                     "}")),
                      fillContainer = limited, escape = FALSE, rownames = FALSE) %>%
        {
          if (length(non_int_numeric_cols) > 0) {
            DT::formatRound(., columns = non_int_numeric_cols, digits = sigf)
          } else {
            .
          }
        }
    }
  } else {
    dataset %>% tibble::as_tibble()
  }
}

