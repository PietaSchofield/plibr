#' kniter hook for figure numbering 
#'
#' @export
knithook_plot <- function(x, options) {
  fig_fn <- paste0(opts_knit$get("base.url"), paste(x, collapse = "."))
  fig.cap <- knitr:::.img.cap(options)
  style=c("display: block",
          sprintf("margin: %s;",
                   switch(options$fig.align, 
                          left = 'auto auto auto 0', 
                          center = 'auto',
                          right = 'auto 0 auto auto')))
  addon_args = ""
  if(any(grepl("^out.(height|width)", names(options)))){
    on <- names(options)[grep("^out.(height|width)", names(options))]
    for(out_name in on){
      dimName <- substr(out_name, 5, nchar(out_name))
      if(grepl("[0-9]+(em|px|%|pt|pc|in|cm|mm)", out_name))
        style <- append(style, paste0(dimName, ": ", options[[out_name]]))
      else if(length(options$out.width) > 0)
        addon_args <- paste0(addon_args, dimName, "='", options[[out_name]], "'")
      }
  }
  fig_number_txt <- ""
  cntr <- getOption("figure_counter", FALSE)
  if(cntr != FALSE){
    if(is.logical(cntr))
      cntr <- 1
    fig_number_txt <- 
      sprintf(getOption("figure_counter_str", "Figure %s: "), 
              ifelse(getOption("figure_counter_roman", FALSE), 
                     as.character(as.roman(cntr)), as.character(cntr)))
    if(is.numeric(cntr))
      options(figure_counter = cntr + 1)
  }
  paste0("<figure><img src='", fig_fn, "'", 
         " ", addon_args,
         paste0(" style='", paste(style, collapse="; "), "'"),
         ">",
         "<figcaption class='centre'>", fig_number_txt, fig.cap, 
         "</figcaption></figure>")
}
