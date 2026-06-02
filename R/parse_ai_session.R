#' parse ai session log
#'
#' @export
parse_ai_session <- function(logfile, output = "vim") {
  log <- yaml::read_yaml(logfile)
  messages <- log$messages
  
  # Skip system message, format user/assistant
  content <- lapply(messages[-1], function(msg) {
    sprintf("**%s:**\n%s\n", toupper(msg$role), msg$content)
  })
  
  text <- paste(content, collapse = "\n---\n\n")
  
  if (output == "vim") {
    tmpfile <- tempfile(fileext = ".md")
    writeLines(text, tmpfile)
    return(tmpfile)
  } else if (output == "html") {
    return(text)
  }
}
