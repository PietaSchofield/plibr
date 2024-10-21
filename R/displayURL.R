#' display a web page nicely in chrome
#'
#' @export
displayURL <- function(url) 
{
    chrome_path <- "/usr/bin/google-chrome-stable"
    profile_path <- "/home/pietas/.config/google-chrome"
    system(paste0("\"", chrome_path, "\" --user-data-dir=\"", 
        profile_path, "\" --profile-directory=\"Default\" ", 
        url))
}

