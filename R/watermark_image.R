#' Water mark and scale image
#' 
#' @export
watermark_image <- function(img_path, 
                            text = "© Pietà Georgie Schofield ", 
                            opacity = 0.3, 
                            width=800,
                            ret=NULL,
                            saveimg=NULL) {

  img <- magick::image_read(file.path(img_path))
  img <- magick::image_scale(img, paste0(width,"x"))
  img <- magick::image_annotate(img, text, 
                 gravity = "southeast",
                 font = "Arial",
                 size = 20,
                 color = "white")
  if(!is.null(saveimg)){
    wmimg <- file.path(saveimg,gsub("[.]*$","_wm.png",basename(img_path)))
    magick::image_write(img, wmimg, format="png")
  }
  if(is.null(ret)){
    return(invisible(img))
  }else if(ret=="name"){
    return(wmimg)
  }else if(ret=="image"){
    return(img)
  }else{
    return()
  }
}
