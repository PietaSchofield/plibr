#' Extract Posts and Pages from a WordPress Archive (WXR XML)
#'
#' This function reads a WordPress export file (WXR XML) and extracts items of 
#' type "post" and/or "page", including their titles, publication dates, and 
#' HTML content. Comments, attachments, and other non-specified item types are ignored.
#'
#' @param xml_file Character. Path to the WordPress XML export file.
#' @param type Character vector. One or both of "post" and "page" specifying which
#'   item types to extract. Default is "post".
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{title}{Item title (character).}
#'   \item{date}{Publication date (POSIXct).}
#'   \item{content}{Item content in HTML (character).}
#' }
#'
#' @examples
#' blog_file <- file.path(Sys.getenv("HOME"), 
#'                        "Downloads", 
#'                        "nofacenonamenonumber.WordPress.2026-04-03.xml")
#' # Extract posts only
#' posts <- extract_wp_posts(blog_file)
#' head(posts)
#'
#' # Extract posts and pages
#' all_items <- extract_wp_posts(blog_file, type = c("post", "page"))
#'
#' @export
extract_wp_posts <- function(xml_file, type = c("post")) {
  doc <- xml2::read_xml(xml_file)
  items <- xml2::xml_find_all(doc, ".//item")
  
  # Filter by type (e.g., post, page)
  post_items <- items[xml2::xml_text(xml2::xml_find_first(items, ".//wp:post_type")) %in% type]
  
  tibble::tibble(
    title   = xml2::xml_text(xml2::xml_find_first(post_items, "title")),
    date    = xml2::xml_text(xml2::xml_find_first(post_items, "wp:post_date")),
    content = xml2::xml_text(xml2::xml_find_first(post_items, "content:encoded"))
  ) |>
    dplyr::mutate(
      title = trimws(title),
      date  = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
}
