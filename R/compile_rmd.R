#' Compile and Serve RMarkdown Documents with a Temporary Build Directory
#'
#' This function compiles an RMarkdown (`.Rmd`) file into multiple output formats,
#' using a temporary build directory to avoid cluttering the main output directory.
#'
#' @param input_file Character. The name of the `.Rmd` file (without extension).
#' @param formats Character vector. The output formats to render.
#'   Supported formats include:
#'   - `"html_document"` (default HTML output)
#'   - `"powerpoint_presentation"` (PPTX output)
#'   - `"beamer_presentation"` (PDF via Beamer)
#'   - `"gitbook"` (multi-page HTML book)
#'   - `"tufte::tufte_handout"` (Tufte-style PDF)
#' @param gitRepo Character. The Git repository directory (`"uol"` or `"pers"`).
#' @param projName Character. The project name.
#' @param codeDir Character. The directory where the source `.Rmd` is located.
#' @param output_dir Character. The root directory where final outputs are stored. Defaults to `"/srv/http"`.
#' @return Invisibly returns the file paths of the generated outputs.
#' @importFrom rmarkdown render
#' @importFrom servr httd
#' @examples
#' \dontrun{
#' # Compile only Distill and Tufte outputs
#' compile_rmd("my_presentation", formats = c("bookdown::gitbook", "tufte::tufte_handout"))
#' }
#' @export
compile_rmd <- function(fileName,
                        formats = c("bookdown::gitbook", "tufte::tufte_handout"),
                        gitRepo = "uol", projName = "atlasps",
                        codeDir = getwd(), output_dir = "/srv/http") {

  input_file <- file.path(codeDir, paste0(fileName, ".Rmd"))
  output_dir <- file.path(output_dir, gitRepo, projName)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Define a build directory (_book/)
  build_dir <- file.path(codeDir, "_book")
  if (!dir.exists(build_dir)) dir.create(build_dir, recursive = TRUE)

  output_files <- list()

  # Loop through requested formats
  for (format in formats) {
    message(paste("Rendering", input_file, "as", format, "..."))

    temp_output <- tryCatch({
      if (format == "bookdown::gitbook") {
        # Ensure GitBook dependencies are copied
        rmarkdown::render(input = input_file,
                          output_format = format,
                          output_dir = build_dir,
                          clean = TRUE,
                          envir = new.env())

        # Move necessary files to `output_dir`
        file.copy(from = file.path(build_dir, "libs"),
                  to = file.path(output_dir, "libs"), recursive = TRUE)

        file.copy(from = file.path(build_dir, "index.html"),
                  to = file.path(output_dir, "index.html"), overwrite = TRUE)
      } else {
        # Handle other formats (Tufte, Beamer, PowerPoint, etc.)
        rmarkdown::render(input_file,
                          output_format = format,
                          output_dir = output_dir,
                          clean = TRUE,
                          envir = new.env())
      }
    }, error = function(e) {
      warning(paste("Error rendering", format, ":", e$message))
      return(NULL)
    })

    output_files[[format]] <- temp_output
  }

  # Start local server if HTML-based output is present
  if ("bookdown::gitbook" %in% formats) {
    plibr::displayURL(file.path("http:/","localhost",gitRepo,projName,paste0(fileName,".html")))
  }

  message("Rendering complete! Output stored in:", output_dir)
  return(invisible(output_files))
}

