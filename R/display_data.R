#' @export
display_data <- function(dataset,number=NULL,disp=T){
  if(disp){
    if(is.null(number)){
      dataset %>% DT::datatable()
    }else{
      dataset %>% head(number) %>% DT::datatable()
    }
  }
}
