#' Parse a single monthly worklog note for daily hours
#'
#' Scans a plibr-style monthly notes \code{.Rmd} file for day-header lines
#' (\verb{# Weekday Nth _(Xhrs)_}) and extracts a tidy day-level record.
#' Only the canonical \verb{_(Xhrs)_} tag, or one of the recognised off-day
#' keywords (weekend/not working/off/leave/bank holiday/sick), is trusted
#' for automatic counting. Anything else is flagged for manual review
#' rather than guessed.
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

  off_kw   <- c("weekend", "not working", "\\boff\\b")
  leave_kw <- c("leave", "annual leave", "holiday")
  bh_kw    <- c("bank holiday", "\\bbh\\b")
  sick_kw  <- c("sick")

  day_header_pattern <- paste0("^#\\s+(", paste(weekdays_full, collapse = "|"), ")\\b")
  header_idx <- which(stringr::str_detect(lines, day_header_pattern))

  if (length(header_idx) == 0) return(tibble::tibble())

  results <- vector("list", length(header_idx))

  for (i in seq_along(header_idx)) {
    header_line <- lines[header_idx[i]]

    weekday_hits <- stringr::str_extract_all(
      header_line, paste(weekdays_full, collapse = "|"))[[1]]
    is_range <- length(weekday_hits) >= 2

    if (is_range) {
      results[[i]] <- tibble::tibble(
        date = as.Date(NA), weekday_stated = NA_character_, hours = 0,
        status = "range_off", raw = header_line, review = FALSE
      )
      next
    }

    stated_weekday <- stringr::str_extract(header_line, paste(weekdays_full, collapse = "|"))
    day_num <- as.integer(stringr::str_extract(header_line, "\\d{1,2}"))
    date <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yr, mon, day_num)))
    actual_weekday <- weekdays(date)
    weekday_mismatch <- !is.na(date) && !is.na(stated_weekday) && actual_weekday != stated_weekday

    hrs_match <- stringr::str_extract(header_line, "[0-9]+(\\.[0-9]+)?\\s*hrs?")
    hours <- if (!is.na(hrs_match)) {
      as.numeric(stringr::str_extract(hrs_match, "[0-9]+(\\.[0-9]+)?"))
    } else NA_real_

    lower_line <- stringr::str_to_lower(header_line)
    status <- NA_character_
    review <- FALSE

    if (!is.na(hours)) {
      status <- "worked"
    } else if (stringr::str_detect(lower_line, paste(off_kw, collapse = "|"))) {
      status <- "off"; hours <- 0
    } else if (stringr::str_detect(lower_line, paste(leave_kw, collapse = "|"))) {
      status <- "leave"; hours <- 0
    } else if (stringr::str_detect(lower_line, paste(bh_kw, collapse = "|"))) {
      status <- "bh"; hours <- 0
    } else if (stringr::str_detect(lower_line, paste(sick_kw, collapse = "|"))) {
      status <- "sick"; hours <- 0
    } else {
      status <- "REVIEW"; hours <- NA_real_; review <- TRUE
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


#' Summarise parsed worklog days into a weekly balance table
#'
#' Aggregates a day-level worklog tibble (as produced by
#' \code{.parse_worklog_file}) into weekly totals against a
#' 4 x 6.5625hr (26.25hr) target, docking the target for any
#' leave/bank-holiday/sick day, and carrying a running cumulative
#' balance forward across all weeks supplied.
#'
#' @param day_tbl Tibble as returned by \code{.parse_worklog_file}
#'   (or a row-bound combination across several months).
#' @param weekly_target Numeric. Standard weekly target hours.
#'   Default \code{26.25} (i.e. 4 x 6.5625).
#' @param day_unit Numeric. Hours docked from the target per
#'   leave/bh/sick day. Default \code{6.5625}.
#' @param dock_sick Logical. Should sick days reduce the target
#'   (i.e. you don't owe the time back)? Default \code{TRUE}.
#'
#' @return A tibble with one row per week: \code{week}, \code{actual_hours},
#'   \code{dock_units}, \code{target}, \code{balance}, \code{running_balance}.
#'
#' @keywords internal
.summarise_worklog <- function(day_tbl, weekly_target = 26.25,
                                day_unit = 6.5625, dock_sick = TRUE) {

  dock_statuses <- c("leave", "bh")
  if (dock_sick) dock_statuses <- c(dock_statuses, "sick")

  # True bounds of the data actually supplied — critical for spotting
  # boundary weeks that are only partially represented in this call.
  data_min <- min(day_tbl$date, na.rm = TRUE)
  data_max <- max(day_tbl$date, na.rm = TRUE)

  dock_days <- day_tbl |>
    dplyr::filter(status %in% dock_statuses) |>
    dplyr::mutate(week = lubridate::floor_date(date, "week", week_start = 1)) |>
    dplyr::count(week, name = "dock_units")

  weekly <- day_tbl |>
    dplyr::filter(status == "worked") |>
    dplyr::mutate(week = lubridate::floor_date(date, "week", week_start = 1)) |>
    dplyr::group_by(week) |>
    dplyr::summarise(actual_hours = sum(hours, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(dock_days, by = "week") |>
    dplyr::mutate(
      dock_units = dplyr::coalesce(dock_units, 0),
      week_end = week + lubridate::days(6),
      # A week is only trustworthy if the WHOLE week lies inside the
      # actual date range we were given — otherwise "no hours found"
      # might just mean "no data for that day", not "day off".
      partial = week < data_min | week_end > data_max,
      target = dplyr::if_else(partial, NA_real_,
                               weekly_target - dock_units * day_unit),
      balance = dplyr::if_else(partial, NA_real_, actual_hours - target)
    ) |>
    dplyr::arrange(week)

  # Running balance only accumulates over scored (non-partial) weeks
  weekly$running_balance <- NA_real_
  scored <- !weekly$partial
  weekly$running_balance[scored] <- cumsum(weekly$balance[scored])

  weekly
}

#' Report worked hours vs target across one or more monthly notes
#'
#' Parses plibr-style monthly worklog notes for day-header hour tags,
#' builds a weekly balance table against a 26.25hr (4 x 6.5625) target,
#' and prints a running cumulative balance so overwork is visible
#' immediately rather than being hidden by a weekly reset. Any day
#' whose header can't be confidently classified is listed separately
#' for manual review rather than silently guessed.
#'
#' @param path Character vector. Either:
#'   \itemize{
#'     \item a single file path to one monthly notes \code{.Rmd}, or
#'     \item a directory containing several monthly notes files, or
#'     \item a vector of explicit file paths.
#'   }
#'   If a directory is given, files matching \code{pattern} are found
#'   and parsed in date order, so the running balance persists across
#'   months.
#' @param pattern Character. Regex used to find monthly note files when
#'   \code{path} is a directory. Default matches plibr's
#'   \code{YYYYMM..._notes.Rmd}-style naming: \code{"^[0-9]{6}.*\\.Rmd$"}.
#' @param weekly_target Numeric. Standard weekly target hours.
#'   Default \code{26.25}.
#' @param day_unit Numeric. Hours docked per leave/bh/sick day.
#'   Default \code{6.5625}.
#' @param dock_sick Logical. Should sick days reduce the target?
#'   Default \code{TRUE}.
#' @param quiet Logical. If \code{TRUE}, suppress console output and
#'   just return the data. Default \code{FALSE}.
#' @param start_from Date or character (\code{"YYYY-MM-DD"}). Optional.
#'   If supplied, all day-records before this date are dropped before
#'   weekly scoring — so the running balance ledger begins cleanly at
#'   this point (e.g. the start of the academic year) rather than at
#'   the first date found in the files, or from an arbitrary calendar
#'
#' @return (Invisibly) a list with elements \code{days} (day-level tibble)
#'   and \code{weekly} (weekly summary tibble), for further use if needed.
#'
#' @examples
#' \dontrun{
#' # Single month
#' worklog_report("~/repositories/uol/notes/202609_notes.Rmd")
#'
#' # Whole notes directory — running balance across all months found
#' worklog_report("~/repositories/uol/notes/")
#' }
#'
#'
#' @export
worklog_report <- function(path,
                            pattern = "^[0-9]{6}.*\\.Rmd$",
                            weekly_target = 26.25,
                            day_unit = 6.5625,
                            dock_sick = TRUE,
                            start_from = NULL,
                            quiet = FALSE) {

  files <- character()

  for (p in path) {
    if (dir.exists(p)) {
      found <- list.files(p, pattern = pattern, full.names = TRUE)
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
    return(invisible(list(days = day_tbl, weekly = tibble::tibble())))
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

  weekly <- .summarise_worklog(day_tbl, weekly_target, day_unit, dock_sick)

  if (!quiet) {
    cat("==== WEEKLY SUMMARY (", length(files), "file(s) parsed) ====\n", sep = " ")
    print(weekly, n = Inf)

    if (any(weekly$partial, na.rm = TRUE)) {
      cat("\n\u26a0\ufe0f  Partial boundary week(s) excluded from scoring",
          "(week not fully covered by the data range):\n")
      print(dplyr::filter(weekly, partial), n = Inf)
    }

    review <- day_tbl |> dplyr::filter(review | status == "REVIEW")
    if (nrow(review) > 0) {
      cat("\n==== NEEDS YOUR EYES (not auto-counted) ====\n")
      for (i in seq_len(nrow(review))) cat("-", review$raw[i], "\n")
    } else {
      cat("\n\u2705 No ambiguous days \u2014 everything parsed cleanly.\n")
    }

    scored_weeks <- dplyr::filter(weekly, !partial)
    if (nrow(scored_weeks) > 0) {
      final_balance <- scored_weeks$running_balance[nrow(scored_weeks)]
      cat("\nRunning balance as of last fully-scored week:",
          round(final_balance, 2), "hrs",
          if (final_balance > 0) "(owed back / in credit)" else "(owing time)", "\n")
    }
  }

  invisible(list(days = day_tbl, weekly = weekly))
}
