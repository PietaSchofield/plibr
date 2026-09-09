#' Parse a single monthly worklog note for daily hours
#'
#' Scans a plibr-style monthly notes \code{.Rmd} file for day-header lines
#' (\verb{# Weekday Nth _(status Xhrs)_}) and extracts a tidy day-level
#' record. Every day header must carry an explicit status tag - nothing is
#' inferred from surrounding prose. Recognised statuses are \code{work},
#' \code{we} (weekend, not worked), \code{we-work} (weekend, worked),
#' \code{leave}, \code{sick}, \code{bh} (bank holiday), and \code{off}
#' (flexi day, not worked). Anything else - missing tag, unrecognised
#' status word, or a stated weekday that doesn't match the calendar - is
#' flagged for manual review rather than guessed.
#'
#' @param path Character. Path to a single monthly notes \code{.Rmd} file.
#'
#' @return A tibble with one row per detected day header, columns:
#'   \code{date}, \code{weekday_stated}, \code{hours}, \code{status},
#'   \code{raw} (the original header line), \code{review} (logical).
#'
#' @keywords internal
.parse_worklog_file <- function(path) {

  lines <- readr::read_lines(path)

  fn_line <- lines[stringr::str_detect(lines, "\\.fileName\\s*<-\\s*\"[0-9]{6}\"")]
  if (length(fn_line) == 0) {
    warning("No .fileName <- \"YYYYMM\" chunk found in ", path,
            " — skipping file.")
    return(tibble::tibble())
  }
  yyyymm <- stringr::str_extract(fn_line[1], "[0-9]{6}")
  yr  <- as.integer(substr(yyyymm, 1, 4))
  mon <- as.integer(substr(yyyymm, 5, 6))

  weekdays_full <- c("Monday","Tuesday","Wednesday","Thursday",
                      "Friday","Saturday","Sunday")

  valid_statuses <- c("work", "we", "we-work", "leave", "sick", "bh", "off")

  day_header_pattern <- paste0("^#\\s+(", paste(weekdays_full, collapse = "|"), ")\\b")
  header_idx <- which(stringr::str_detect(lines, day_header_pattern))

  if (length(header_idx) == 0) return(tibble::tibble())

  tag_pattern <- "_\\(([a-z-]+)\\s+([0-9]+(?:\\.[0-9]+)?)\\s*hrs?\\)_"

  results <- vector("list", length(header_idx))

  for (i in seq_along(header_idx)) {
    header_line <- lines[header_idx[i]]

    stated_weekday <- stringr::str_extract(header_line, paste(weekdays_full, collapse = "|"))
    day_num <- as.integer(stringr::str_extract(header_line, "\\d{1,2}"))
    date <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yr, mon, day_num)))
    actual_weekday <- weekdays(date)
    weekday_mismatch <- !is.na(date) && !is.na(stated_weekday) && actual_weekday != stated_weekday

    tag <- stringr::str_match(header_line, tag_pattern)

    if (is.na(tag[1, 1])) {
      status <- "REVIEW [no status tag found]"
      hours  <- NA_real_
      review <- TRUE
    } else {
      raw_status <- tag[1, 2]
      hours      <- as.numeric(tag[1, 3])

      if (raw_status %in% valid_statuses) {
        status <- raw_status
        review <- FALSE
      } else {
        status <- paste0("REVIEW [unrecognised status: '", raw_status, "']")
        review <- TRUE
      }
    }

    if (weekday_mismatch) {
      review <- TRUE
      status <- paste0(status, " [weekday mismatch: stated ", stated_weekday,
                        ", actually ", actual_weekday, "]")
    }

    results[[i]] <- tibble::tibble(
      date = date, weekday_stated = stated_weekday, hours = hours,
      status = status, raw = stringr::str_trim(header_line), review = review
    )
  }

  dplyr::bind_rows(results)
}


#' Build a day-level nominal-vs-actual hours ledger
#'
#' Converts a parsed day-level worklog tibble into a running ledger against
#' a flat daily nominal (default \code{5.25}hrs, i.e. a 5-day 26.25hr week),
#' applied to every weekday regardless of which days are actually worked -
#' correct for flexible/flexi-time working where "the working week" isn't a
#' fixed pattern of days.
#'
#' Leave, sick, and bank holiday days are treated as satisfying that day's
#' nominal (diff = 0) rather than counting as time owed. A flexi \code{off}
#' day does \emph{not} satisfy the nominal - it's a deliberate choice not to
#' work, so the shortfall carries forward to be made up whenever, not
#' forgiven. Weekend days carry a nominal of zero; weekend work (\code{we-work})
#' is pure credit on top.
#'
#' Days with untrustworthy hours (missing/unrecognised status tag) are
#' excluded from the running total (contribute \code{NA}/\code{0} rather
#' than being guessed), and listed separately for manual review.
#'
#' @param day_tbl Tibble as returned by \code{.parse_worklog_file} (or a
#'   row-bound combination across several months).
#' @param nominal_day Numeric. Nominal hours owed per weekday (Mon-Fri).
#'   Default \code{5.25} (i.e. 26.25 / 5).
#' @param dock_sick Logical. Should sick days count as satisfying the daily
#'   nominal (i.e. you don't owe the time back)? Default \code{TRUE}.
#'
#' @return A tibble with one row per day: \code{date}, \code{weekday},
#'   \code{status}, \code{actual_hours}, \code{nominal_hours}, \code{diff},
#'   \code{cumulative_diff}, \code{hard_review} (logical - excluded from
#'   totals), \code{review} (logical - flagged but still counted), \code{raw}.
#'
#' @keywords internal
.build_daily_ledger <- function(day_tbl, nominal_day = 5.25, dock_sick = TRUE) {

  dock_statuses <- c("leave", "bh")
  if (dock_sick) dock_statuses <- c(dock_statuses, "sick")

  day_tbl |>
    dplyr::filter(!is.na(date)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      weekday = weekdays(date),
      is_weekend = weekday %in% c("Saturday", "Sunday"),
      is_workday = !is_weekend & status %in% c("work","off"),
      # Untrustworthy = hours never resolved to a number at all (no tag /
      # unrecognised status). A weekday-name typo alone does NOT make hours
      # untrustworthy - the number parsed fine, only the label was wrong.
      hard_review = is.na(hours),

      nominal_hours = dplyr::if_else(!is_workday, 0, nominal_day),

      actual_hours = dplyr::case_when(
        hard_review ~ NA_real_,
        status %in% c("work", "we-work") |
          stringr::str_starts(status, "work ") |
          stringr::str_starts(status, "we-work ") ~ hours,
        TRUE ~ 0
      ),

                  
      dock_matched = stringr::str_starts(status, paste(dock_statuses, collapse = "|")),

      diff = dplyr::case_when(
        hard_review ~ NA_real_,
        dock_matched ~ 0,
        TRUE ~ actual_hours - nominal_hours
      )
    ) |>
    dplyr::mutate(cumulative_diff = cumsum(dplyr::coalesce(diff, 0))) |>
    dplyr::select(date, weekday, status, hours, actual_hours, nominal_hours,
                  diff, cumulative_diff, hard_review, review, raw)
}


#' Roll a daily ledger up to monthly totals
#'
#' @param ledger Tibble as returned by \code{.build_daily_ledger}.
#'
#' @return A tibble with one row per month: \code{month}, \code{actual_hours},
#'   \code{nominal_hours}, \code{diff}, \code{cumulative_diff}.
#'
#' @keywords internal
.summarise_monthly <- function(ledger) {
  ledger |>
    dplyr::filter(!hard_review) |>
    dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      actual_hours  = sum(actual_hours, na.rm = TRUE),
      nominal_hours = sum(nominal_hours, na.rm = TRUE),
      diff          = sum(diff, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(month) |>
    dplyr::mutate(cumulative_diff = cumsum(diff))
}


#' Report worked hours vs nominal daily target across one or more monthly notes
#'
#' Parses plibr-style monthly worklog notes for day-header status/hour tags
#' and builds a day-level running ledger against a flat \code{nominal_day}
#' (default \code{5.25}hrs) applied to every weekday - correct for
#' flexible/flexi-time working, since there's no fixed "working week
#' pattern" to compare against. Leave/sick/bank-holiday days satisfy that
#' day's nominal; flexi days off do not (shortfall carries forward). A
#' monthly roll-up is also produced. Any day whose header can't be
#' confidently classified is excluded from totals and listed separately for
#' manual review rather than silently guessed.
#'
#' Only files from \code{min_yyyymm} onward are parsed by default - notes
#' predating the current tagging convention are simply not compatible with
#' it, and are skipped rather than surfaced as false review items.
#'
#' @param path Character vector. Single file, directory, or vector of paths.
#' @param pattern Character. Regex used to find monthly note files when
#'   \code{path} is a directory. Default \code{"^[0-9]{6}.*\\.Rmd$"}.
#' @param min_yyyymm Character. Earliest \code{YYYYMM} to include when
#'   scanning a directory. Default \code{"202608"} (the tagging convention's
#'   start). Set to \code{NULL} to disable filtering.
#' @param nominal_day Numeric. Nominal hours owed per weekday. Default
#'   \code{5.25}.
#' @param dock_sick Logical. Should sick days satisfy the daily nominal?
#'   Default \code{TRUE}.
#' @param quiet Logical. Suppress console output? Default \code{FALSE}.
#' @param start_from Date or character (\code{"YYYY-MM-DD"}). Optional -
#'   drop all day-records before this date before the ledger is built, so
#'   the running total begins cleanly at this point.
#'
#' @return (Invisibly) a list with elements \code{days} (raw parsed day
#'   tibble), \code{ledger} (daily running ledger), and \code{monthly}
#'   (monthly roll-up).
#'
#' @examples
#' \dontrun{
#' worklog_report("~/repositories/uol/notes/")
#' res <- worklog_report("~/repositories/uol/notes/", quiet = TRUE)
#' }
#'
#' @export
worklog_report <- function(path,
                            pattern = "^[0-9]{6}.*\\.Rmd$",
                            min_yyyymm = "202608",
                            nominal_day = 5.25,
                            dock_sick = TRUE,
                            start_from = NULL,
                            quiet = FALSE) {

  files <- character()

  for (p in path) {
    if (dir.exists(p)) {
      found <- list.files(p, pattern = pattern, full.names = TRUE)

      if (!is.null(min_yyyymm)) {
        found_yyyymm <- stringr::str_extract(basename(found), "^[0-9]{6}")
        found <- found[!is.na(found_yyyymm) & found_yyyymm >= min_yyyymm]
      }

      files <- c(files, found)
    } else if (file.exists(p)) {
      files <- c(files, p)
    } else {
      warning("Path not found, skipping: ", p)
    }
  }

  if (length(files) == 0) stop("No worklog files found for: ", paste(path, collapse = ", "))

  files <- sort(files)

  day_tbl <- purrr::map_dfr(files, .parse_worklog_file)

  if (nrow(day_tbl) == 0) {
    if (!quiet) cat("No day headers found in supplied file(s).\n")
    return(invisible(list(days = day_tbl, ledger = tibble::tibble(), monthly = tibble::tibble())))
  }

  if (!is.null(start_from)) {
    start_from <- as.Date(start_from)
    n_before <- sum(day_tbl$date < start_from, na.rm = TRUE)
    day_tbl <- dplyr::filter(day_tbl, is.na(date) | date >= start_from)
    if (!quiet && n_before > 0) {
      cat("Trimmed", n_before, "day-record(s) before", format(start_from),
          "\u2014 ledger starts fresh from here.\n\n")
    }
  }

  ledger  <- .build_daily_ledger(day_tbl, nominal_day, dock_sick)
  monthly <- .summarise_monthly(ledger)

  if (!quiet) {
    cat("==== MONTHLY SUMMARY (", length(files), "file(s) parsed) ====\n", sep = " ")
    print(monthly, n = Inf)

    weekend_flag <- ledger |> dplyr::filter(stringr::str_starts(status, "we-work"))
    if (nrow(weekend_flag) > 0) {
      cat("\n\U0001f6a9 Weekend work (banked as credit):\n")
      print(dplyr::select(weekend_flag, date, hours), n = Inf)
    }

    hard <- ledger |> dplyr::filter(hard_review)
    soft <- ledger |> dplyr::filter(!hard_review, review)

    if (nrow(hard) > 0) {
      cat("\n==== EXCLUDED FROM TOTALS (needs your eyes) ====\n")
      for (i in seq_len(nrow(hard))) cat("-", hard$raw[i], "\n")
    }
    if (nrow(soft) > 0) {
      cat("\n==== FLAGGED BUT STILL COUNTED (weekday-label mismatch only) ====\n")
      for (i in seq_len(nrow(soft))) cat("-", soft$raw[i], "\n")
    }
    if (nrow(hard) == 0 && nrow(soft) == 0) {
      cat("\n\u2705 No ambiguous days \u2014 everything parsed cleanly.\n")
    }

    if (nrow(ledger) > 0) {
      final <- ledger$cumulative_diff[nrow(ledger)]
      cat("\nRunning balance as of", format(ledger$date[nrow(ledger)]), ":",
          round(final, 2), "hrs",
          if (final > 0) "(owed back / in credit)" else "(owing time)", "\n")
    }
  }

  invisible(list(days = day_tbl, ledger = ledger, monthly = monthly))
}


#' Export parsed worklog data to CSV or Excel
#'
#' Writes the tibbles produced by \code{worklog_report()} out to disk.
#'
#' @param log List. Output of \code{worklog_report()} (containing
#'   \code{days}, \code{ledger}, \code{monthly}). Optional if those tibbles
#'   are supplied directly instead.
#' @param path Character. Output file path. \code{.csv} writes \code{ledger}
#'   to \code{path} and \code{monthly} to a sibling \code{_monthly.csv}.
#'   \code{.xlsx} writes \code{days}/\code{ledger}/\code{monthly} as three
#'   sheets in one file (requires \pkg{openxlsx}).
#' @param days,ledger,monthly Optional tibbles, used instead of \code{log}.
#'
#' @return Invisibly, the path(s) written to.
#'
#' @examples
#' \dontrun{
#' res <- worklog_report("~/repositories/uol/notes/", quiet = TRUE)
#' worklog_export(res, "~/Desktop/worklog.csv")
#' worklog_export(res, "~/Desktop/worklog.xlsx")
#' }
#'
#' @export
worklog_export <- function(log = NULL, path, days = NULL, ledger = NULL, monthly = NULL) {

  if (!is.null(log)) {
    if (is.null(days))    days    <- log$days
    if (is.null(ledger))  ledger  <- log$ledger
    if (is.null(monthly)) monthly <- log$monthly
  }

  if (is.null(ledger) || nrow(ledger) == 0) {
    stop("No ledger data to export (log$ledger or `ledger` is empty/NULL).")
  }

  ext <- tolower(tools::file_ext(path))

  if (ext == "xlsx") {

    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is needed for .xlsx export.\n",
           "Install with install.packages(\"openxlsx\"), or export as .csv instead.")
    }

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "ledger")
    openxlsx::writeData(wb, "ledger", ledger)

    if (!is.null(monthly) && nrow(monthly) > 0) {
      openxlsx::addWorksheet(wb, "monthly")
      openxlsx::writeData(wb, "monthly", monthly)
    }
    if (!is.null(days) && nrow(days) > 0) {
      openxlsx::addWorksheet(wb, "days_raw")
      openxlsx::writeData(wb, "days_raw", days)
    }

    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    message("Written: ", path)
    return(invisible(path))

  } else if (ext == "csv") {

    readr::write_csv(ledger, path)
    written <- path

    if (!is.null(monthly) && nrow(monthly) > 0) {
      monthly_path <- sub("\\.csv$", "_monthly.csv", path, ignore.case = TRUE)
      readr::write_csv(monthly, monthly_path)
      written <- c(written, monthly_path)
    }

    message("Written: ", paste(written, collapse = ", "))
    return(invisible(written))

  } else {
    stop("Unrecognised file extension '.", ext, "' \u2014 use .csv or .xlsx.")
  }
}

