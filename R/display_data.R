
# datatable 
#'
#' displays a datatable (DT) either as raw tibble if not compiling to avoid trying to fire up unnecessary
#' browers or as a DT if rendering to html
#' 
#' @param dataset the data to display
#' @param number the maxium number of rows to display
#' @param disp the flag which decides the whole shebang
#' @param limited contrain to container
#'
#' @export
display_data <- function(dataset, number = NULL, table_type = "DT", limited = FALSE, 
                         buttons = FALSE, plen = NULL, caption = NULL, 
                         fixh = TRUE, fixc = list(leftColumns = 1), sigf = 3){
  if (!is.null(number)) {
    dataset <- dataset %>% tibble::as_tibble() %>% head(number)
  }
  if(is.null(plen)){
    plen=10
  }else if(!is.numeric(plen)){
    plen=nrow(dataset)
  }else{
    plen=plen
  }

  non_int_numeric_cols <- names(dataset)[sapply(dataset,function(x){
    is.numeric(x) && any(x!=as.integer(x))
    })]
  if (table_type == 'DT') {
    if(buttons){
      ext <- 'Buttons'
      btns <- c("copy","csv")
      dom <- 'Blfrtip'
      dataset %>% 
        DT::datatable(extensions = ext, caption=caption,
          options = list(dom = dom, buttons = btns, 
            lengthMenu = list(c(10,50,100, -1), c('10', '50', '100', 'All')),
            paging = T, scrollX = T, scrollY=T, pageLength = plen,
            fixHeader=fixh,fixColumns=fixc,
            initComplete = htmlwidgets::JS("function(settings, json) {",
              "$(this.api().table().header()).css({'color': '#93B2B2'});",  # Header text color
              "$(this.api().table().body()).css({'color': '#93A1A1'});",   # Body text color
            "}"
          )),
          fillContainer=limited,escape=F,rownames=F) %>%
        DT::formatRound(columns = non_int_numeric_cols,digits=sigf) 
    }else{
      dom <- 'lfrtip'
      dataset %>%
        DT::datatable(caption=caption, 
          options = list(dom = dom, 
            lengthMenu = list(c(10,50,100, -1), c('10', '50', '100', 'All')),
            paging = T, scrollX = T, scrollY=T,pageLength = plen,
            fixHeader=fixh,fixColumns=fixc,
            initComplete = htmlwidgets::JS("function(settings, json) {",
              "$(this.api().table().header()).css({'color': '#A1B2B2'});",  # Header text color
              "$(this.api().table().body()).css({'color': '#93A1A1'});",   # Body text color
            "}"
          )),
          fillContainer=limited,escape=F,rownames=F) %>%
        DT::formatRound(columns = non_int_numeric_cols,digits=sigf) 
    }
  } else if (table_type == 'kable') {
    knitr::kable(dataset, format = 'pipe')
  } else if (table_type == 'flextable') {
    flextable::qflextable(dataset)
  } else {
    dataset %>% tibble::as_tibble()
  }

}
