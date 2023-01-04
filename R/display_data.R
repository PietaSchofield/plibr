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
#' @export
display_data <- function(dataset,number=NULL,disp=T,limited=T){
  if(!is.null(number)){
    dataset <- dataset %>% head(number)
  }
  if(disp){
    dataset %>% DT::datatable(extensions = 'Buttons', 
                              options = list(dom = 'Blfrtip', 
                                             buttons = c('copy', 'csv', 'excel', 'pdf'), 
                                             lengthMenu = list(c(10,50,100, -1), 
                                                               c('10', '50', '100', 'All')),
                                             paging = T,
                                             scrollX = T),
                              fillContainer=limited)
  }else{
    dataset %>% tibble::as_tibble()
  }
}
