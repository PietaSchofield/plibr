#' Set current file (symlinks Rmd + html to a "current" pointer)
#'
#' @param fileName Name (without extension) of the target file
#' @param projName Project/subfolder name, e.g. "notes"
#' @param repoName repositories repo name, e.g. "uol"
#' @param htmlName Top-level folder under /srv/http, default "uol"
#' @param html_path Path to rendered html output
#' @param rmd_path Path to Rmd source
#' @param linkName Name to give the symlink (default "current")
#'
#' @export
setcurrent <- function(fileName, projName = "notes", repoName = "uol",
                        htmlName = "uol",
                        html_path = file.path("/srv", "http", htmlName, projName),
                        rmd_path = file.path(Sys.getenv("HOME"), "repositories", repoName, projName),
                        linkName = "current") {

  link_it <- function(target_path, link_path) {
    if (!file.exists(target_path)) {
      warning("Target does not exist: ", target_path)
    }
    unlink(link_path)  # safe even if nothing there
    ok <- file.symlink(from = target_path, to = link_path)
    if (!ok) warning("Failed to create symlink: ", link_path)
    invisible(ok)
  }

  rmd_target_path  <- file.path(rmd_path,  paste0(fileName, ".Rmd"))
  rmd_link_path    <- file.path(rmd_path,  paste0(linkName, ".Rmd"))
  html_target_path <- file.path(html_path, paste0(fileName, ".html"))
  html_link_path   <- file.path(html_path, paste0(linkName, ".html"))

  link_it(rmd_target_path, rmd_link_path)
  link_it(html_target_path, html_link_path)

  invisible(list(rmd = rmd_link_path, html = html_link_path))
}
