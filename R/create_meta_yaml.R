#' create project meta data
#'
#' create a YAML document with meta data about the project for indexing
#' 
#' @export
create_meta_yaml <- function(projDir,projName,projDesc,owner,contact,overwrite=F){
  meta <- list(
    project_name = toupper(projName),
    description = projDesc,
    status = 'active',
    web_status = 'missing',
    start_date = format(Sys.Date(),"%Y-%m-%d %H:%M:%S"),
    last_updated = format(Sys.Date(),"%Y-%m-%d %H:%M:%S"),
    owner = owner,
    contact = owner)

  yaml_file <- file.path(projDir,"meta.yaml")
  if(overwrite){
    writeLines(yaml::as.yaml(meta), yaml_file)
  }
  message("Created metadata file ", yaml_file)
}

#' update meta data
#'
#' @export
update_meta_yaml <- function(repo_path) {
  
  project_dirs <- list.dirs(path = repo_path, recursive = F)
  ret <- lapply(project_dirs, function(dirn) {
    meta_file <- file.path(dirn, "meta.yaml")
    if(file.exists(meta_file)){
  
      meta <- yaml::yaml.load_file(meta_file)
      prev_updated <- meta$last_updated

      project_files <- list.files(dirn, pattern=".*Rmd",recursive = F, full.names = TRUE)
      if (length(project_files) > 0) {
        last_modified <- max(file.info(project_files[grepl(".*Rmd$",project_files)])$mtime)
        meta$last_updated <- format(last_modified,"%Y-%m-%d %H:%M:%S")
      }

      # 5. Save the updated metadata back to meta.yaml
      if(prev_updated!=meta$last_updated){
        writeLines(yaml::as.yaml(meta), meta_file)
        message("Updated metadata file: ", meta_file)
      }
    }
  })
}

#' build master list
#f
#' @export
build_master_list <- function(repoName,
                              git_directory=file.path(Sys.getenv("HOME"),"GitLab"),
                              recur=FALSE,
                              localhost="http://localhost",
                              htmlroot="uol",
                              html_directory=file.path("/srv","http")) {
  repo_path <- file.path(git_directory,repoName)
  project_dirs <- list.dirs(path = repo_path, recursive = recur)
  master_list <- lapply(project_dirs, function(dirn) {
    yaml_file <- file.path(dirn, "meta.yaml")
    if (file.exists(yaml_file)) {
      metadata <- yaml::yaml.load_file(yaml_file)
      metadata$directory <- dirn
      # 4. Check for the index.html file on the website
      project_name <- toupper(basename(dirn))
      index_file <- file.path(html_directory,htmlroot,basename(dirn),"index.html")
      html_exists <- fs::file_exists(index_file)
      if (html_exists) {
        metadata$web_status <- "exists"
        metadata$name <- sprintf('<a href="%s">%s</a>',
              file.path(localhost,htmlroot,basename(dirn),"index.html"),project_name)
      } else {
        metadata$web_status <- "missing"
        metadata$name <- project_name
      }
      return(metadata)
    }
  }) %>% bind_rows()  # Combine all metadata into a single data frame
  return(master_list)
}

#' build project index
#'
#' @export
build_project_index <- function(project,
                                git_directory=file.path(Sys.getenv("HOME"),"GitLab"), 
                                html_directory=file.path("/srv","http"),
                                localhost="http://localhost",
                                repo="liverpool",
                                htmlroot="uol") {
  if(F){
    project <- "notes"
    git_directory <- file.path(Sys.getenv("HOME"),"GitLab","liverpool")
    html_directory <- file.path("/srv","http","uol",project)
    localhost <- "http://localhost"
    repo <- "uol"
  }

  rmd_directory <- file.path(git_directory,repo,project)
  html_directory <- file.path(html_directory,htmlroot,project)
  # Helper function to extract YAML metadata

  # List all Rmd files
  rmd_files <- list.files(rmd_directory, pattern = "\\.Rmd$", full.names = TRUE)
  rmd_files <- rmd_files[basename(rmd_files) != "index.Rmd"]

  # Extract metadata and other information
  index_data <- lapply(rmd_files, function(rmd_file) {
    metadata <- plibr::extract_metadata(rmd_file)
    rmd_name <- basename(rmd_file)
    base_name <- gsub("[.]Rmd","",rmd_name)
    html_file <- file.path(html_directory, sub("\\.Rmd$", ".html", rmd_name))

    # Check if HTML file exists
    html_exists <- fs::file_exists(html_file)

    # File dates
    last_modified <- format(fs::file_info(rmd_file)$modification_time,"%Y-%m-%d %H:%M")
    html_created <- ifelse(html_exists,
        format(fs::file_info(html_file)$modification_time,"%Y-%m-%d %H:%M"),NA)

    # Build a row for the index
    list(
      Name = if (html_exists) {
        sprintf('<a href="%s">%s</a>',file.path(localhost,htmlroot,project,basename(html_file)),base_name)
      } else {
        base_name
      },
      Description = metadata$description %||% "No description",
      Date_Updated = as.character(last_modified),
      HTML_Created = if (!is.na(html_created)) as.character(html_created) else "Not available"
    )
  })

  # Convert to a data frame
  index_df <- do.call(rbind, lapply(index_data, as.data.frame))

  return(index_df)
}

#' extrct metadata
#'
#' @export
extract_metadata <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  start <- which(lines == "---")[1]
  end <- which(lines == "---")[2]
  if (is.na(start) || is.na(end)) return(NULL)
  yaml_block <- paste(lines[start:end], collapse = "\n")
  
  # Load YAML and evaluate expressions
  metadata <- yaml::yaml.load(yaml_block, eval.expr = TRUE)
  return(metadata)
}

#' Display Project Index
#'
#' @export
display_project_index <- function(idx_df){
  idx_df %>% arrange(desc(Name)) %>% plibr::display_data(disp=T,plen=nrow(idx_df))
}
