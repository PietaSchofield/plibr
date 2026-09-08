#' Connect to DuckDB with ICU pre-configured and resilient extension caching
#'
#' Sets duckdb.home explicitly (so DuckDB never nags or silently switches to
#' a temp dir), disables network autoinstall, and — if the ICU extension
#' isn't already cached — restores it from a local backup .gz instead of
#' hitting the network.
#'
#' @param dbfile Path to db file, or ":memory:" (default).
#' @param config Extra duckdb() config, merged with sensible defaults.
#' @param extension_backup_dir Where your durable backup .gz files live.
#'   Point this at somewhere that survives a wipe of ~/.duckdb — a dotfiles
#'   repo, a synced drive, wherever you keep "never lose this" files.
#' @param duckdb_platform Override auto-detected platform string if needed.
#' @param ... Passed through to dbConnect().
#' @export
duckdb_connect <- function(dbfile = ":memory:",
                            config = list(),
                            extension_backup_dir = "~/duckdb_extension_backups",
                            duckdb_platform = NULL,
                            ...) {

  if (is.null(getOption("duckdb.home"))) {
    options(duckdb.home = "~/.duckdb")
  }

  full_config <- utils::modifyList(
    list(autoinstall_known_extensions = "false"),
    config
  )

  con <- DBI::dbConnect(
    duckdb::duckdb(config = full_config),
    dbdir = dbfile,
    ...
  )

  tryCatch({
    DBI::dbExecute(con, "LOAD icu;")
  }, error = function(e) {
    message("ICU extension not cached locally — attempting restore from backup...")
    restored <- .restore_extension_from_backup(
      extension_name = "icu",
      backup_dir = extension_backup_dir,
      platform_override = duckdb_platform
    )
    if (restored) {
      DBI::dbExecute(con, "LOAD icu;")
      message("ICU extension restored from backup and loaded.")
    } else {
      stop(
        "ICU extension unavailable: not cached, autoinstall disabled, ",
        "no backup found in '", extension_backup_dir, "'.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  })

  con
}

# --- internals, not exported ---

.restore_extension_from_backup <- function(extension_name, backup_dir, platform_override = NULL) {

  backup_dir <- path.expand(backup_dir)

  status <- tryCatch(duckdb::duckdb_storage_status(), error = function(e) NULL)
  if (is.null(status)) return(FALSE)

  ext_dir <- status$directory[grepl("xtension", status$kind, ignore.case = TRUE)][1]
  if (is.na(ext_dir) || !nzchar(ext_dir)) return(FALSE)

  version  <- paste0("v", as.character(utils::packageVersion("duckdb")))
  platform <- platform_override %||% .detect_duckdb_platform()

  target_dir  <- file.path(ext_dir, version, platform)
  target_file <- file.path(target_dir, paste0(extension_name, ".duckdb_extension"))

  backup_gz <- file.path(
    backup_dir,
    paste0(version, "_", platform, "_", extension_name, ".duckdb_extension.gz")
  )

  if (!file.exists(backup_gz)) {
    message("No backup found at expected path: ", backup_gz)
    return(FALSE)
  }

  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  con_in  <- gzfile(backup_gz, "rb")
  con_out <- file(target_file, "wb")
  repeat {
    chunk <- readBin(con_in, "raw", n = 1e6)
    if (length(chunk) == 0) break
    writeBin(chunk, con_out)
  }
  close(con_in); close(con_out)

  file.exists(target_file)
}

.detect_duckdb_platform <- function() {
  sys  <- Sys.info()[["sysname"]]
  arch <- R.version$arch
  os <- switch(sys,
    "Linux" = "linux", "Darwin" = "osx", "Windows" = "windows",
    stop("Unrecognised OS: ", sys)
  )
  cpu <- if (grepl("aarch64|arm64", arch)) "arm64" else "amd64"
  paste0(os, "_", cpu)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
