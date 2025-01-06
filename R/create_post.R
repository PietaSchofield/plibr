#' create blog post
#'
#' @export
create_post <- function(title, project_dir, use_date = TRUE, ext = ".Rmd", open = FALSE) {
  # Clean and format the title
  slug <- gsub(" ", "-", tolower(title))  # Replace spaces with hyphens
  date_prefix <- if (use_date) format(Sys.Date(), "%Y-%m-%d-") else ""

  # Define the post filename
  filename <- file.path(project_dir, "content/post", paste0(date_prefix, slug))

  # Check if the file or directory already exists
  if (!file.exists(filename)) {
    # Create the new post
    blogdown::new_post(
      title = title,
      ext = ext,
      filename = filename,
      open = open
    )
    message("Created new post: ", filename)
  } else {
    message("Post already exists: ", filename)
  }
}

