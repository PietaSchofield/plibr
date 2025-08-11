#' Extract Prompt-Response Pairs from Both Sections, Output as Markdown
#'
#' @param dat Parsed YAML chat log (from yaml.load_file)
#' @param outfile Output file (NULL = print)
#'
#' @return Invisibly returns markdown (character vector)
#' @export
aichat_yaml_as_markdown <- function(dat, outfile=NULL) {
  blocks <- list()

  section_md <- function(msgs, label) {
    n <- length(msgs)
    output <- character()
    i <- 1
    while (i < n) {
      if (!is.null(msgs[[i]]$role) && msgs[[i]]$role == "user" &&
          (i+1) <= n && !is.null(msgs[[i+1]]$role) && msgs[[i+1]]$role == "assistant") {
        prompt <- msgs[[i]]$content
        response <- msgs[[i+1]]$content
        block <- paste0("\n---\n\n",
                        "### ", label, " Pair ", (i+1)%/%2, "\n\n",
                        "**Prompt:**\n\n",
                        "> ", gsub("\n", "\n> ", prompt), "\n\n",
                        "**Response:**\n\n",
                        response, "\n"
        )
        output <- c(output, block)
        i <- i + 2
      } else {
        i <- i + 1
      }
    }
    output
  }

  if (!is.null(dat$compressed_messages)) {
    blocks[["Compressed"]] <- section_md(dat$compressed_messages, "Compressed")
  }

  # Some YAMLs store these as $messages, $dialogue, etc.
  # Check if present and not literally the same object as compressed
  if (!is.null(dat$messages)) {
    # Protect against literal pointer identity / accidental duplication
    same <- identical(dat$compressed_messages, dat$messages)
    if (!same) {
      blocks[["Full messages"]] <- section_md(dat$messages, "Full")
    }
  }

  # Optionally join with section headers for clarity
  output <- character()
  for (nm in names(blocks)) {
    output <- c(output,
      paste0("\n\n# ", nm, " Section\n"),
      blocks[[nm]]
    )
  }
  output <- c(output, "\n---\n")
  if (is.null(outfile)) cat(paste(output, collapse = "")) else writeLines(output, outfile)
  invisible(output)
}

