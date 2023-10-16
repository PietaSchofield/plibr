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
display_data <- function(dataset,number=NULL,disp=T,limited=F,buttons=F,plen=10){
  if(!is.null(number)){
    dataset <- dataset %>% tibble::as_tibble() %>% head(number) 
  }
  if(disp){
    if(buttons){
      ext <- 'Buttons'
      btns <- c("copy","csv")
      dom <- 'Blfrtip'
      dataset %>% DT::datatable(extensions = ext, 
                              options = list(dom = dom, 
                                             buttons = btns, 
                                             lengthMenu = list(c(10,50,100, -1), 
                                                               c('10', '50', '100', 'All')),
                                             paging = T,
                                             scrollX = T,
                                             pageLength = plen),
                              fillContainer=limited,escape=F,rownames=F)
    }else{
      dom <- 'lfrtip'
      dataset %>% DT::datatable( options = list(dom = dom, 
                                             lengthMenu = list(c(10,50,100, -1), 
                                                               c('10', '50', '100', 'All')),
                                             paging = T,
                                             scrollX = T,
                                             pageLength = plen),
                              fillContainer=limited,escape=F,rownames=F)
    }
}else{
    dataset %>% tibble::as_tibble()
  }
}
