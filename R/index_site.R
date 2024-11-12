#' index my site
#'
#' index the website
#'
#' @export
index_site <- function(html_dir=file.path("/srv","http"),
                       index_file=file.path(html_dir,"search_index.json"),db=F){
  if(db){
    html_dir <- file.path("/srv","http")
    index_file <- file.path(html_dir,"search_index.json")
  }

  # Initialize an empty list to store page data
  pages <- list()

# Loop through each HTML file in the directory
  pagelist <- list.files(html_dir,pattern=".html$",full.names=T,recur=T)
  for (file in pagelist){ 
    page <- xml2::read_html(file, options="HUGE")
    title <- rvest::html_node(page, "title") %>% rvest::html_text(trim = TRUE)
  
    # Extract the first paragraph only (or another specific section you want)
    content <- rvest::html_node(page, "p") %>% rvest::html_text(trim = TRUE)
  
    # Limit content length to avoid overly large JSON
    if (!is.na(content)) {
      content <- substr(content, 1, 200)  # Limiting content to the first 150 characters
    }  
  
    # Add data to the list
    pages <- append(pages, list(list(id = file, title = title, content = content)))
  }
# Convert to JSON and save
  jsonlite::write_json(pages, path = index_file, pretty = TRUE)
}
