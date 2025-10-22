#' List HTML files without corresponding Rmd source files
#'
#' @description
#' Scans two directories: one containing rendered `.html` files, and one
#' containing source `.Rmd` files. Returns a tibble of `.html` files that do not
#' have a matching `.Rmd` (same base name) in the code directory.
#'
#' @param html_dir Path to the directory containing `.html` files.
#' @param rmd_dir Path to the directory containing `.Rmd` files.
#' @param as_url Logical. If TRUE, `linkaddress` is returned as an HTTP-style
#'   path using `base_url`. If FALSE (default), file paths are used.
#' @param base_url Optional base URL to prepend when `as_url = TRUE`.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{name}{Base name of the HTML file (without extension).}
#'   \item{linkaddress}{Either a file path or an HTTP link to the HTML file.}
#' }
#'
#' @examples
#' list_html_without_rmd(
#'   html_dir = "/srv/http/uol/project",
#'   rmd_dir = "/home/me/GitLab/liverpool/project"
#' )
#'
#' @export
html_without_rmd <- function(projName,
                            rmd_dir=.codeDir,
                            html_dir=file.path("/srv","http","uol",projName)
                            ) {

  html_files <- list.files(html_dir, pattern = "\\.html$", full.names = FALSE)
  rmd_files  <- list.files(rmd_dir, pattern = "\\.Rmd$", full.names = FALSE)

  html_base <- tools::file_path_sans_ext(html_files)
  rmd_base  <- tools::file_path_sans_ext(rmd_files)

  missing_rmd <- html_files[!html_base %in% rmd_base]

  tibble(
    name = tools::file_path_sans_ext(missing_rmd)
  ) |>
  mutate(orphanpage=sprintf('<a href="%s.html" target="_blank">%s</a>',name,name)) |>
  select(orphanpage)
}
