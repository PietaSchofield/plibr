#' Knit a Single Bookdown Chapter to Standalone HTML
#'
#' Renders a single chapter or page from a **bookdown/gitbook** project as a standalone HTML file,
#' preserving cross-references and using your existing project setup.
#'
#' This function temporarily creates a wrapper `.Rmd` that sources shared setup code
#' (e.g. data paths, global chunk options) and includes the target chapter as a child document.
#'
#' @param chap_path Path to the R Markdown file of the chapter (e.g. `"chapters/03-methods.Rmd"`).
#' @param title Optional title for the standalone HTML. Defaults to `"Standalone: <basename>"`.
#' @param self_contained Logical. Should the HTML embed all resources (images, CSS, JS)?
#'   Default is `TRUE`.
#' @param toc Logical. Include a table of contents? Default is `TRUE`.
#' @param number_sections Logical. Number the sections? Default is `TRUE`.
#' @param output_file Optional name for the rendered HTML output. If `NULL`, a default name
#'   based on the chapter is used.
#' @param root Project root directory (used for relative paths). Defaults to the current working directory.
#'
#' @details
#' The function calls [rmarkdown::render()] on a temporary wrapper file using
#' `bookdown::html_document2` as the output format, so cross-references
#' (`(\#label)`) and citations are preserved.
#'
#' You can customise shared data and chunk options by editing the file
#' `"R/common_setup.R"` in your project.
#'
#' @return Invisibly returns the path to the rendered HTML file.
#'
#' @examples
#' \dontrun{
#' knit_one_chapter("chapters/03-methods.Rmd",
#'                  title = "Methods (standalone)",
#'                  output_file = "methods-standalone.html")
#' }
#'
#' @export
knit_one <- function(chap_path,
                     title = NULL,
                     self_contained = TRUE,
                     toc = TRUE,
                     number_sections = TRUE,
                     code_folding = "hide",
                     output_file = NULL,
                     highlight = "espresso",
                     mathjax = "null",
                     output_dir = "/srv/http/standalone/",
                     css = system.file('css', 'solarized_dark.css', package = 'plibr'),
                     fig_path = NULL,
                     root = getwd(),
                     force_solarized = TRUE) {  # Added parameter

  stopifnot(file.exists(chap_path))
  title <- title %||% paste0("Standalone: ", basename(chap_path))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # If fig_path given, force non-self-contained; if not, force self-contained
  if (is.null(fig_path)) {
    self_contained <- TRUE
  } else {
    self_contained <- FALSE
    fig_path <- paste0(rtrim(fig_path), "/")            # ensure trailing slash
  }

  # Build setup block with fig.path only when provided
  setup_lines <- c(
    "```{r setup, include=FALSE}",
    sprintf("knitr::opts_knit$set(root.dir = %s)", shQuote(normalizePath(root)))
  )
  if (!is.null(fig_path)) {
    setup_lines <- c(setup_lines,
      sprintf("knitr::opts_chunk$set(fig.path = %s)", shQuote(fig_path)))
  }
  setup_lines <- c(setup_lines, "```")

  wrapper <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    sprintf('title: "%s"', title),
    "output:",
    "  bookdown::html_document2:",
    sprintf("    self_contained: %s", tolower(self_contained)),
    sprintf("    toc: %s", tolower(toc)),
    sprintf("    number_sections: %s", tolower(number_sections)),
    sprintf("    code_folding: %s", tolower(code_folding)),
    sprintf("    highlight: %s", tolower(highlight)),
    sprintf("    mathjax: %s", tolower(mathjax)),
    sprintf('    css: %s', css), 
    "",
    "---",
    "",
    setup_lines,
    "",
    sprintf("```{r build_it, child=%s}", shQuote(chap_path)),
    "```"
  ), wrapper)

  rmarkdown::render(
    wrapper,
    output_file = output_file,
    output_dir  = output_dir,
    clean = TRUE,
    envir = new.env(parent = globalenv())
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
rtrim <- function(x) sub('[/\\\\]+$', '', x)
