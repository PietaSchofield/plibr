#' Output AI Chat Dialogue as Markdown
#'
#' Formats and outputs each prompt/response pair from a dialogue tibble as visually separated 
#' markdown blocks. Prompts are shown as blockquotes; responses retain any embedded markdown 
#' formatting (e.g., lists, headings, bold, code). Can output to file or console.
#'
#' @param dialogue A tibble or data.frame with columns \code{prompt} and \code{response} (as produced by \code{parse_aichat_log()$dialogue}).
#' @param outfile Optional character path. If supplied, output will be written to this file as a UTF-8 markdown document; otherwise, output is sent to console via \code{cat()}.
#'
#' @return Invisibly returns a character vector of markdown-formatted dialogue blocks (for further use if needed). 
#'
#' @details
#' Each dialogue turn produces:
#' \itemize{
#'   \item A divider (\code{---}) separating entries
#'   \item The prompt, formatted as a Markdown blockquote
#'   \item The response, with any internal Markdown untouched
#'   \item Repeated for each dialogue row in order.
#' }
#' This format is suitable for copy-paste into a Markdown editor or GitHub/Gitlab issues, and supports neurodivergent-friendly reading.
#'
#' @examples
#' \dontrun{
#'   result <- parse_aichat_log("mylog.yaml")
#'   output_dialogue_as_markdown(result$dialogue)
#'   output_dialogue_as_markdown(result$dialogue, outfile = "mychat.md")
#' }
#' 
#' @export
output_dialogue_as_markdown <- function(dialogue, outfile = NULL) {
  output <- character()
  for (i in seq_len(nrow(dialogue))) {
    prompt_block <- paste0(
      "\n---\n\n",
      "**Prompt:**\n\n",
      "> ", gsub("\n", "\n> ", dialogue$prompt[i]),
      "\n\n**Response:**\n\n",
      dialogue$response[i],
      "\n"
    )
    output <- c(output, prompt_block)
  }
  output <- c(output, "\n---\n")
  
  if (!is.null(outfile)) {
    writeLines(output, outfile)
    invisible(output)
  } else {
    cat(paste(output, collapse = ""))
    invisible(output)
  }
}

