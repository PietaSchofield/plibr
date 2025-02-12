#' Deploy a GitBook to a Web Server
#'
#' This function wraps `bookdown::render_book()` to render a GitBook and 
#' then automatically moves necessary assets (such as images and HTML pages) 
#' to the correct location on a web server.
#'
#' @param book_path Character. The local path to the GitBook source directory. Defaults to `"."` (current directory).
#' @param book_name Character. The name of the book directory.
#' @param server_user Character. The SSH username for the remote server. Defaults to `"pietas"`.
#' @param server_host Character. The hostname or IP address of the remote server. Defaults to `"localhost"`.  
#' @param server_path Character. The destination path on the web server where the book should be deployed. 
#'
#' @return No return value. This function performs rendering and deployment as a side effect.
#' 
#' @import bookdown
#' @import glue
#'
#' @export
deploy_gitbook <- function(book_path = ".", book_name = NULL, server_user = "pietas", 
                           server_host = "localhost", server_path = file.path("/srv/http/uol")) {
  
  # Step 1: Render the book
  message("Rendering GitBook...")
  bookdown::render_book(input = book_path, output_format = "bookdown::gitbook")

  # Define paths
  book_output_dir <- file.path(book_path, "_book")  # Where the book is built
  local_assets <- file.path(book_path, "assets")  # Folder containing extra files
  html_files <- list.files(local_assets, "\\.html$", full.names = TRUE)
  image_files <- list.files(local_assets, pattern = "\\.png$", full.names = TRUE)  # Get all images

  # Step 2: Copy necessary files into `_book/`
  message("Copying timevis_page.html and images to _book/")
  if (length(html_files) > 0) {
    file.copy(html_files, book_output_dir, overwrite = TRUE)
  }
  if (length(image_files) > 0) {
    file.copy(image_files, book_output_dir, overwrite = TRUE)
  }

  # Define the deployment path
  book_deploy_path <- file.path(server_path, book_name)

  # Step 3: Check if local deployment or remote deployment
  if (server_host == "localhost") {

    if (dir.exists(book_deploy_path)) {
      message("Removing existing book directory...")
      unlink(book_deploy_path, recursive = TRUE, force = TRUE)
    }

    message("Moving _book content to local web server directory...")

    dir.create(book_deploy_path, recursive = TRUE, showWarnings = FALSE)
    file.copy(from = list.files(book_output_dir, full.names = TRUE),
              to = book_deploy_path,
              recursive = TRUE,
              overwrite = TRUE)
    
    message("Local deployment complete!")
  } else {
    message("Uploading to remote web server...")
    system(glue::glue("scp -r {book_output_dir}/* {server_user}@{server_host}:{book_deploy_path}"))
    message("Remote deployment complete!")
  }

  message(paste0("GitBook is live at http://",server_host,"/",book_name))
}

