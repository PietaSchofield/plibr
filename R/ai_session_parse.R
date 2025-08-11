#' Parse an AI Chat YAML Log
#'
#' Parses a YAML-formatted AI chat session log, extracting the first system
#' prompt and all subsequent user/assistant turns in alternating prompt/response pairs.
#'
#' Assumes a flat sequence where each entry is a map with "role" and "content" elements.
#' Only the first system prompt is kept; all subsequent system messages are ignored.
#' User and assistant roles are paired as prompt (user) and response (assistant).
#'
#' @param file Path to the YAML chat log file.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{system_prompt}{Character scalar: the content of the first system prompt.}
#'     \item{dialogue}{A tibble with two columns: prompt (user message), response (assistant reply).}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- parse_aichat_log("your_chat_log.yaml")
#' cat(result$system_prompt)
#' print(result$dialogue)
#' }
#'
#' @import yaml
#' @importFrom tibble tibble
#' @export
parse_aichat_log <- function(file) {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Please install the 'yaml' package.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Please install the 'tibble' package.")

  y <- yaml::read_yaml(file)

  # Prefer compressed_messages if present; otherwise, try messages; else, fail
  if (!is.null(y$compressed_messages)) {
    chat <- y$compressed_messages
    source_field <- "compressed_messages"
  } else if (!is.null(y$messages)) {
    chat <- y$messages
    source_field <- "messages"
  } else if (is.list(y) && !is.null(y[[1]]) && is.list(y[[1]]) && !is.null(y[[1]]$role)) {
    chat <- y
    source_field <- "top-level"
  } else {
    stop("Could not find a list of messages in compressed_messages or messages at top-level.")
  }

  safe_get <- function(x, name) {
    if (is.list(x) && !is.null(x[[name]])) x[[name]] else NA_character_
  }

  # Find the first system prompt (if present)
  first_system <- NULL
  for (entry in chat) {
    roleval <- safe_get(entry, "role")
    if (is.list(entry) && !is.na(roleval) && roleval == "system") {
      first_system <- safe_get(entry, "content")
      break
    }
  }
  if (is.null(first_system)) warning("No system prompt found in log.")

  # Only keep user/assistant exchanges
  ua <- Filter(
    function(x) {
      roleval <- safe_get(x, "role")
      is.list(x) && !is.na(roleval) && roleval %in% c("user", "assistant")
    },
    chat
  )

  # Prompt-response pairs (user followed by assistant)
  prompts <- character()
  responses <- character()

  i <- 1
  while (i < length(ua)) {
    this_role  <- safe_get(ua[[i]], "role")
    next_role  <- if (i+1 <= length(ua)) safe_get(ua[[i+1]], "role") else NA_character_
    if (this_role == "user" && next_role == "assistant") {
      prompts   <- c(prompts,   safe_get(ua[[i]], "content"))
      responses <- c(responses, safe_get(ua[[i+1]], "content"))
      i <- i + 2
    } else {
      i <- i + 1
    }
  }

  dialogue_tbl <- tibble::tibble(prompt = prompts, response = responses)

  list(
    system_prompt = first_system,
    dialogue = dialogue_tbl,
    source_field = source_field
  )
}

