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
update_meta_yaml <- function(projDir, base_url = "http://localhost/uol/",active=NULL) {
  
  # 1. Check if meta.yaml exists
  meta_file <- file.path(projDir, "meta.yaml")
  if (!file.exists(meta_file)) {
    stop("No meta.yaml file found in: ", projDir)
  }
  
  # 2. Load the existing metadata
  meta <- yaml::yaml.load_file(meta_file)
  
  # 3. Get the most recent file modification date
  project_files <- list.files(projDir, recursive = TRUE, full.names = TRUE)
  if (length(project_files) > 0) {
    last_modified <- max(file.info(project_files)$mtime)
    meta$last_updated <- format(last_modified,"%Y-%m-%d %H:%M:%S")
  } else {
    warning("No files found in the project directory. Keeping the last_updated date unchanged.")
  }
  
  # 4. Check for the index.html file on the website
  project_name <- basename(projDir) # Assume project name is the directory name
  url <- paste0(base_url, project_name, "/index.html")
  response <- httr::HEAD(url)
  
  if (httr::status_code(response) == 200) {
    meta$web_status <- "exists"
  } else {
    meta$web_status <- "missing"
  }

  if(!is.null(active)){
    meta$status <- active
  }

  meta$project_name <- toupper(meta$project_name)
  
  # 5. Save the updated metadata back to meta.yaml
  writeLines(yaml::as.yaml(meta), meta_file)
  message("Updated metadata file: ", meta_file)
}

#' build master list
#'
#' @export
build_master_list <- function(repo_path,recur=FALSE,wwwroot="http://localhost/uol/") {
  project_dirs <- list.dirs(path = repo_path, recursive = recur)
  master_list <- lapply(project_dirs, function(dir) {
    yaml_file <- file.path(dir, "meta.yaml")
    if (file.exists(yaml_file)) {
      metadata <- yaml::yaml.load_file(yaml_file)
      metadata$directory <- dir
      metadata$link <- gsub(dirname(dir),wwwroot,dir)
      return(metadata)
    }
    NULL
  }) %>% bind_rows()  # Combine all metadata into a single data frame
  return(master_list)
}

