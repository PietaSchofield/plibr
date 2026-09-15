#' Parse migraine log entries from worklog Rmd files
#'
#' Scans a directory of monthly worklog `.Rmd` files and extracts every
#' MIGRAINE-flagged bullet line, attributing each to the day-header it falls
#' under. Designed to sit alongside \code{\link{worklog_report}} and read the
#' same files.
#'
#' Expected line format (anywhere in the body, not just headers):
#'
#' \preformatted{
#' - __<span class="hl-yellow">MIGRAINE</span>__ _(M 5am-8:30am capr 80%)_ — optional note
#' }
#'
#' Type is \code{M} (migraine — would escalate to unmedicated vomiting
#' wipe-out) or \code{H} (headache/fog that doesn't cross that line). Meds are
#' any combination of \code{c} caffeine, \code{a} aspirin, \code{p}
#' paracetamol, \code{r} rizatriptan, \code{m} metoclopramide. Percentage is
#' function remaining that day. Anything after an em-dash or hyphen following
#' the closing \code{)_} is captured as a free-text note — text on a separate
#' line underneath is intentionally *not* parsed (human-readable only).
#'
#' @param dir Path to the directory containing monthly worklog `.Rmd` files.
#' @param pattern Regex used to select files, passed to \code{list.files}.
#'   Defaults to the same monthly-note naming convention as
#'   \code{worklog_report}.
#' @param quiet Logical; if \code{FALSE}, reports files with no MIGRAINE lines
#'   found (useful for sanity-checking the log itself, not the parser).
#'
#' @return A data frame (one row per migraine/headache event) with columns:
#'   \code{date}, \code{type} (\code{"M"}/\code{"H"}), \code{descriptor}
#'   (raw time text), \code{meds} (raw letter string), \code{function_pct},
#'   \code{note}.
#'
#' @importFrom stringr str_match
#' @export
parse_migraine_log <- function(dir,
                                pattern = "^202[0-9]{4}.*\\.Rmd$",
                                quiet = TRUE) {

  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    warning("No files matched pattern in ", dir)
    return(data.frame())
  }

  day_re <- "^#\\s+(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)\\s+([0-9]{1,2})(st|nd|rd|th)?"
  mig_re <- "\\(([MH])\\s+(.*?)\\s+([caprm]+)\\s+([0-9]+)%\\)_\\s*(?:[—-]\\s*(.*?))?\\s*(?:__)?\\s*$"

  entries <- list()

  for (f in files) {
    ym <- sub("^([0-9]{6}).*", "\\1", basename(f))
    if (!grepl("^[0-9]{6}$", ym)) next
    year  <- as.integer(substr(ym, 1, 4))
    month <- as.integer(substr(ym, 5, 6))

    lines <- readLines(f, warn = FALSE)
    current_date <- as.Date(NA)
    found_any <- FALSE

    for (ln in lines) {
      dm <- regmatches(ln, regexec(day_re, ln))[[1]]
      if (length(dm) > 1 && nzchar(dm[1])) {
        current_date <- as.Date(sprintf("%04d-%02d-%02d", year, month,
                                         as.integer(dm[3])))
        next
      }

      mm <- regmatches(ln, regexec(mig_re, ln, perl = TRUE))[[1]]
      if (length(mm) > 1 && nzchar(mm[1])) {
        found_any <- TRUE
        entries[[length(entries) + 1]] <- data.frame(
          date         = current_date,
          type         = mm[2],
          descriptor   = trimws(mm[3]),
          meds         = mm[4],
          function_pct = as.integer(mm[5]),
          note         = if (nzchar(mm[6])) trimws(mm[6]) else NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }

    if (!quiet && !found_any) {
      message("No MIGRAINE lines found in ", basename(f))
    }
  }

  if (length(entries) == 0) {
    warning("No MIGRAINE entries parsed from any file.")
    return(data.frame())
  }

  out <- do.call(rbind, entries)
  out[order(out$date), ]
}


#' Build a migraine diary (clear / headache / migraine day counts) per month
#'
#' Collapses multiple same-day entries down to a single worst-case daily
#' classification, then rolls that up to monthly counts of clear days,
#' headache days and migraine days — the standard categories a neurologist
#' will ask for.
#'
#' Where more than one entry falls on the same date, the day is classified
#' as \code{"Migraine"} if any entry that day was type \code{M}, otherwise
#' \code{"Headache"} if any was type \code{H}. The daily function score used
#' is the *minimum* (worst) value recorded that day.
#'
#' @param entries A data frame as returned by \code{\link{parse_migraine_log}}.
#' @param range_start,range_end Optional \code{Date} bounds. If supplied,
#'   clear-day counts are computed against every calendar day in this range,
#'   not just days with logged events — days present in the range but absent
#'   from \code{entries} are treated as clear. If omitted, only clear days
#'   cannot be inferred (there's nothing to infer them from) and
#'   \code{clear_days} will be \code{NA}.
#'
#' @return A list with:
#'   \item{daily}{one row per event-day, with \code{date}, \code{type},
#'     \code{function_pct} (worst that day).}
#'   \item{monthly}{one row per month, with \code{month}, \code{migraine_days},
#'     \code{headache_days}, and \code{clear_days} (\code{NA} unless a date
#'     range was supplied).}
#'
#' @export
migraine_diary <- function(entries, range_start = NULL, range_end = NULL) {

  if (nrow(entries) == 0) {
    return(list(daily = data.frame(), monthly = data.frame()))
  }

  daily <- do.call(rbind, lapply(split(entries, entries$date), function(d) {
    data.frame(
      date         = d$date[1],
      type         = if (any(d$type == "M")) "Migraine" else "Headache",
      function_pct = min(d$function_pct),
      stringsAsFactors = FALSE
    )
  }))
  daily <- daily[order(daily$date), ]
  daily$month <- format(daily$date, "%Y-%m")

  monthly <- as.data.frame(table(
    month = daily$month,
    type  = daily$type
  ))
  monthly <- reshape(monthly, idvar = "month", timevar = "type",
                      direction = "wide")
  names(monthly) <- gsub("Freq\\.", "", names(monthly))
  names(monthly)[names(monthly) == "Migraine"] <- "migraine_days"
  names(monthly)[names(monthly) == "Headache"] <- "headache_days"
  if (!"migraine_days" %in% names(monthly)) monthly$migraine_days <- 0
  if (!"headache_days" %in% names(monthly)) monthly$headache_days <- 0

  monthly$clear_days <- NA_integer_
  if (!is.null(range_start) && !is.null(range_end)) {
    all_days <- data.frame(date = seq(as.Date(range_start), as.Date(range_end),
                                       by = "day"))
    all_days$month <- format(all_days$date, "%Y-%m")
    total_by_month <- table(all_days$month)
    logged_by_month <- table(daily$month)
    monthly$clear_days <- as.integer(
      total_by_month[monthly$month] - logged_by_month[monthly$month]
    )
    monthly$clear_days[is.na(monthly$clear_days)] <-
      as.integer(total_by_month[monthly$month[is.na(monthly$clear_days)]])
  }

  monthly <- monthly[order(monthly$month), ]
  rownames(monthly) <- NULL

  list(daily = daily, monthly = monthly)
}


#' Combined migraine report — parse + diary in one call
#'
#' Convenience wrapper mirroring \code{\link{worklog_report}}: point it at
#' the same notes directory and get back parsed entries and the rolled-up
#' diary in one call.
#'
#' @param dir Path to worklog `.Rmd` files.
#' @param pattern File-matching regex, as in \code{\link{parse_migraine_log}}.
#' @param quiet Logical, passed through to \code{\link{parse_migraine_log}}.
#' @param range_start,range_end Optional bounds passed to
#'   \code{\link{migraine_diary}} for clear-day inference.
#'
#' @return A list with \code{entries}, \code{daily}, \code{monthly} — same
#'   shape convention as \code{worklog_report()$monthly}.
#'
#' @examples
#' \dontrun{
#' res <- migraine_report("~/repositories/uol/notes/",
#'                         pattern = "^202[6-9](0[89]|1[0-2]).*\\.Rmd$")
#' res$monthly
#' }
#'
#' @export
migraine_report <- function(dir,
                             pattern = "^202[0-9]{4}.*\\.Rmd$",
                             quiet = TRUE,
                             range_start = NULL,
                             range_end = NULL) {
  entries <- parse_migraine_log(dir, pattern = pattern, quiet = quiet)
  diary   <- migraine_diary(entries, range_start = range_start,
                             range_end = range_end)
  list(entries = entries, daily = diary$daily, monthly = diary$monthly)
}

#' Debug helper: show why MIGRAINE lines are/aren't matching
#'
#' Scans files for any line containing "MIGRAINE" and reports whether the
#' migraine-entry regex matched it. Use this when \code{parse_migraine_log()}
#' returns nothing and you need to see what the file actually contains versus
#' what the parser expects.
#'
#' @param dir Path to worklog files.
#' @param pattern File-matching regex.
#'
#' @export
migraine_log_debug <- function(dir, pattern = "^202[0-9]{4}.*\\.Rmd$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  message("Files matched: ", length(files))
  if (length(files) == 0) {
    message("-> Nothing matched the file pattern itself. Check `pattern` and `dir`.")
    return(invisible(NULL))
  }

  mig_re <- "\\(([MH])\\s+(.*?)\\s+([caprm]+)\\s+([0-9]+)%\\)_\\s*(?:[—-]\\s*(.*?))?\\s*(?:__)?\\s*$"

  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep("MIGRAINE", lines, value = TRUE)
    if (length(hits) == 0) next
    message("\n---- ", basename(f), " ----")
    for (h in hits) {
      matched <- grepl(mig_re, h, perl = TRUE)
      cat(if (matched) "[MATCH]   " else "[NO MATCH] ", h, "\n")
    }
  }
}

#' Print the HIT-6 question set
#'
#' HIT-6 is a recall instrument, not something derivable from a log — it has
#' to be answered fresh, in the moment, at the appointment. This function
#' doesn't score anything; it exists purely so you never have to remember the
#' six questions or the scoring scale under pressure in the room.
#'
#' @return Invisibly, a character vector of the six questions plus the
#'   scoring key. Also printed to console.
#'
#' @export
hit6_prompt <- function() {
  qs <- c(
    "1. How often does pain from your headaches reach a severe intensity?",
    "2. How often do headaches limit your ability to do usual daily activities?",
    "3. How often did you wish you could lie down?",
    "4. In the past 4 weeks, how often did headaches make you too tired for usual work/activities?",
    "5. In the past 4 weeks, how often did you feel fed up or irritated because of headaches?",
    "6. How often did headaches limit your ability to concentrate?",
    "",
    "Scale: Never=6 · Rarely=8 · Sometimes=10 · Very Often=11 · Always=13",
    "Sum of 6 answers = total score, range 36-78."
  )
  cat(qs, sep = "\n")
  invisible(qs)
}

