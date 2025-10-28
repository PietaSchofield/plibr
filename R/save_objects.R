#' Save objects from the global knitting environment
#'
#' This helper saves named R objects (created in an R Markdown document)
#' to individual `.rds` files inside a specified directory.
#' It is designed for separating analysis artefacts from the writing process.
#'
#' @param names Character vector of object names to save.
#' @param dir Character string giving the directory to save into.
#'   The directory will be created if it does not exist.
#' @param envir Environment to search for objects. Defaults to the
#'   global knitting environment if available, otherwise the caller's environment.
#'
#' @return Invisibly returns the file paths of the saved objects.
#' @examples
#' \dontrun{
#'   df <- data.frame(x = 1:3, y = 4:6)
#'   save_objects(c("df"), dir = "artefacts")
#' }
#' @export
save_objects <- function(names, dir = "artefacts", envir = NULL) {
  if (is.null(envir)) {
    if ("package:knitr" %in% search() && !is.null(getOption("knitr.in.progress")))
      envir <- knitr::knit_global()
    else
      envir <- parent.frame()
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  paths <- vapply(names, function(nm) {
    obj <- get(nm, envir = envir)
    path <- file.path(dir, paste0(nm, ".rds"))
    saveRDS(obj, path)
    path
  }, FUN.VALUE = character(1))

  invisible(paths)
}

#' Load saved R objects from a directory
#'
#' This helper reads `.rds` files previously saved with [save_objects()]
#' and loads them into the specified environment.
#'
#' @param names Optional character vector of object names (without `.rds` extension)
#'   to load. If `NULL`, all `.rds` files in `dir` are loaded.
#' @param dir Directory containing the `.rds` files. Defaults to `"artefacts"`.
#' @param envir Environment into which objects should be loaded.
#'   Defaults to the caller's environment.
#'
#' @return Invisibly returns a named list of loaded objects.
#' @examples
#' \dontrun{
#'   # load all saved artefacts
#'   load_objects(dir = "artefacts")
#'
#'   # or just specific ones
#'   load_objects(c("tbl_summary", "model1"))
#' }
#' @export
load_objects <- function(names = NULL, dir = "artefacts", 
                         envir = parent.frame()) {
  if (!dir.exists(dir)) stop("Directory not found: ", dir)

  if (is.null(names)) {
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    names <- tools::file_path_sans_ext(basename(files))
  } else {
    files <- file.path(dir, paste0(names, ".rds"))
    missing <- files[!file.exists(files)]
    if (length(missing)) stop("Missing files: ", paste(missing, collapse = ", "))
  }

  objs <- setNames(vector("list", length(names)), names)
  for (i in seq_along(names)) {
    objs[[i]] <- readRDS(files[i])
    assign(names[i], objs[[i]], envir = envir)
  }

  invisible(objs)
}
