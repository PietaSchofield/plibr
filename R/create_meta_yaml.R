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
update_meta_yaml <- function(repo_paths, codeDir=file.path(Sys.getenv("HOME"),"repositories"),db=F) {
  if(db){
    codeDir <- file.path(Sys.getenv("HOME"),"repositories")
    repo_paths <- c("uol","sprint-analysis")
    repo_path <- repo_paths[1]
  }
  lapply(repo_paths,function(repo_path){
    repo_dir <- file.path(codeDir,repo_path)
    if(file.exists(file.path(repo_dir,"meta.yaml"))){
      project_dirs <- repo_dir
    }else{
      project_dirs <- list.dirs(path = repo_dir, full=T, recursive = F)
    }
    project_dirs <- list.dirs(path = file.path(codeDir,repo_path), full=T,recursive = F)
    ret <- lapply(project_dirs, function(dirn) {
      meta_file <- file.path(dirn, "meta.yaml")
      if(file.exists(meta_file)){
    
        meta <- yaml::yaml.load_file(meta_file)
        prev_updated <- meta$last_updated

        project_files <- list.files(dirn, pattern=".*md",recursive = F, full.names = TRUE)
        if (length(project_files) > 0) {
          last_modified <- max(file.info(project_files[grepl(".*md$",project_files)])$mtime)
          meta$last_updated <- format(last_modified,"%Y-%m-%d %H:%M:%S")
        }

        # 5. Save the updated metadata back to meta.yaml
        if(prev_updated!=meta$last_updated){
          writeLines(yaml::as.yaml(meta), meta_file)
        }
      }else{
        cat(paste0('Missing meta.yaml: ',meta_file,'\n'))
      }
    })
  })
}

#' discover top-level repos/projects automatically
#'
#' @param exclude character vector of folder names to skip (case-insensitive)
#' @export
discover_repos <- function(git_directory = file.path(Sys.getenv("HOME"), "repositories"),
        exclude = c(".git", ".Rproj.user", 
                    "pers", "archive","protocols","public","public_pages","dotfiles")) {
  top_dirs <- list.dirs(git_directory, recursive = FALSE, full.names = FALSE)
  top_dirs[!tolower(top_dirs) %in% tolower(exclude)]
}

#' build master list
#'
#' @export
build_master_list <- function(repoNames,
                              git_directory=file.path(Sys.getenv("HOME"),"repositories"),
                              recur=FALSE,
                              htmlroot="uol",
                              html_directory=file.path("/srv","http")) {

  master_lists <- lapply(repoNames, function(repoName){
    repo_path <- file.path(git_directory,repoName)

    # NEW: is this repo itself a project (meta.yaml at its own root)?
    if(file.exists(file.path(repo_path,"meta.yaml"))){
      project_dirs <- repo_path
    }else{
      project_dirs <- list.dirs(path = repo_path, recursive = recur)
    }

    master_list <- lapply(project_dirs, function(dirn) {
      yaml_file <- file.path(dirn, "meta.yaml")
      if (file.exists(yaml_file)) {
        metadata <- yaml::yaml.load_file(yaml_file)
        metadata$directory <- dirn
        project_name <- toupper(basename(dirn))
        index_file <- file.path(html_directory,htmlroot,basename(dirn),"index.html")
        html_exists <- fs::file_exists(index_file)
        if (html_exists) {
          metadata$web_status <- "exists"
          metadata$name <- sprintf('<a href="%s">%s</a>',
                file.path(basename(dirn),"index.html"),project_name)
        } else {
          metadata$web_status <- "missing"
          metadata$name <- project_name
        }
        return(metadata)
      }
    }) |> bind_rows()
  })

  master_lists %>% bind_rows()
}

#' build project index
#'
#' @export
build_project_index <- function(project,
                                git_directory=file.path(Sys.getenv("HOME"),"repositories"), 
                                html_directory=file.path("/srv","http"),
                                repo="uol",
                                coderoot=NULL,
                                htmlroot="uol") {
  if(F){
    project <- "tutorials"
    htmlroot <- "uol"
    coderoot <- NULL
    git_directory <- file.path(Sys.getenv("HOME"),"repositories")
    html_directory <- file.path("/srv","http",htmlroot)
    repo <- "uol"
  }

  if(is.null(coderoot)){
    rmd_directory <- file.path(git_directory,repo,project)
  }else{
    rmd_directory <- file.path(git_directory,coderoot,project)
  }
  html_directory <- file.path(html_directory,htmlroot,project)

  # List all Rmd files
  rmd_files <- list.files(rmd_directory, pattern = ".*md$", full.names = TRUE)
  rmd_files <- rmd_files[basename(rmd_files) != "index.Rmd"]

  # Extract metadata and other information
  index_data <- lapply(rmd_files, function(rmd_file) {
    metadata <- plibr::extract_metadata(rmd_file)
    rmd_name <- basename(rmd_file)
    base_name <- gsub("[.].md","",rmd_name)
    html_file <- file.path(html_directory, sub("[.].md$", ".html", rmd_name))

    # Check if HTML file exists
    html_exists <- fs::file_exists(html_file)

    # File dates
    last_modified <- format(fs::file_info(rmd_file)$modification_time,"%Y-%m-%d")
    html_created <- ifelse(html_exists,
        format(fs::file_info(html_file)$modification_time,"%Y-%m-%d"),"Not Available")

    # Build a row for the index
    list(
      Name = if (html_exists) {
        sprintf('<a href="%s">%s</a>',
             file.path(basename(html_file)),base_name)
      } else {
        base_name
      },
      Description = metadata$description %||% "No description",
      Date_Updated = last_modified,
      HTML_Created = html_created
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
  
  # Load YAML and evaluate expressions with error handling
  tryCatch(
    {
      metadata <- yaml::yaml.load(yaml_block, eval.expr = TRUE)
      return(metadata)
    },
    error = function(e) {
      message("YAML parse error in: ", file_path)
      message("Error: ", e$message)
      return(NULL)
    }
  )
}

#' Display Project Index
#'
#' @export
display_project_index <- function(idx_df,sortover=NULL,buts=FALSE){
  if(!is.null(sortover)){
    idx_df <- idx_df %>% 
      dplyr::arrange(desc(.data[[sortover]]))
  }
  idx_df %>%  plibr::display_data(plen=nrow(idx_df),buttons=buts)
}
