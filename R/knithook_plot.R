#' knitr hook for figure numbering (HTML)
#'
#' @export
#' @importFrom knitr opts_knit knit_hooks
knithook_plot <- function(x, options) {
  # file path (knitr passes a character vector of image paths)
  base_url <- knitr::opts_knit$get("base.url")
  fig_fn   <- paste0(base_url, paste(x, collapse = "."))

  # ---- caption (public API: options$fig.cap) ----
  fig_cap <- options$fig.cap
  if (is.function(fig_cap)) fig_cap <- fig_cap()
  if (length(fig_cap) > 1)  fig_cap <- paste(fig_cap, collapse = " ")
  if (is.null(fig_cap))     fig_cap <- ""

  # ---- style from chunk options ----
  align_css <- switch(options$fig.align %||% "center",
                      left   = "auto auto auto 0",
                      right  = "auto 0 auto auto",
                      center = "auto")
  style <- c("display: block", sprintf("margin: %s;", align_css))

  # honour out.width / out.height if present
  addon_args <- character()
  for (nm in c("out.width", "out.height")) {
    if (!is.null(options[[nm]]) && nzchar(options[[nm]])) {
      dim <- sub("^out\\.", "", nm)
      val <- options[[nm]]
      # pass through if looks like a CSS length; otherwise add as attr
      if (grepl("^[0-9.]+(em|px|%|pt|pc|in|cm|mm)$", val)) {
        style <- c(style, sprintf("%s: %s", dim, val))
      } else {
        addon_args <- c(addon_args, sprintf("%s='%s'", dim, val))
      }
    }
  }

  # ---- simple figure counter (optional) ----
  fig_number_txt <- ""
  cntr <- getOption("figure_counter", FALSE)
  if (!identical(cntr, FALSE)) {
    if (is.logical(cntr)) cntr <- 1
    tmpl <- getOption("figure_counter_str", "Figure %s: ")
    if (isTRUE(getOption("figure_counter_roman", FALSE))) {
      # roman numerals; needs utils::as.roman
      fig_label <- as.character(utils::as.roman(cntr))
    } else {
      fig_label <- as.character(cntr)
    }
    fig_number_txt <- sprintf(tmpl, fig_label)
    if (is.numeric(cntr)) options(figure_counter = cntr + 1)
  }

  # ---- HTML output ----
  paste0(
    "<figure>",
      "<img src='", fig_fn, "' ",
      paste(c(addon_args), collapse = " "),
      " style='", paste(style, collapse = "; "), "'",
      ">",
      "<figcaption class='centre'>",
        fig_number_txt, fig_cap,
      "</figcaption>",
    "</figure>"
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

