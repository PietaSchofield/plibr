knitr::opts_hooks$set(cache = function(opts) {
  if (isTRUE(opts$cache) && grepl("^unnamed-chunk", opts$label)) {
    # try to report exact file/line via srcref (if knitr captured it)
    sr <- opts$srcref
    file <- knitr::current_input()
    if (!is.null(sr)) {
      line <- tryCatch(sr[[1]], error = function(e) NA)
      stop(sprintf("Unnamed cached chunk: %s @ %s:%s", opts$label, file, line))
    } else {
      stop(sprintf("Unnamed cached chunk: %s in %s", opts$label, file))
    }
  }
  opts
})

cache_base <- file.path(path.expand('~/.cache/knitr'), .gitRepo, .projName, .fileName)
dir.create(cache_base, recursive = TRUE, showWarnings = FALSE)

# enable knitr caching to that location
knitr::opts_chunk$set(cache = TRUE,
                      cache.path = paste0(cache_base, "/"), 
                      fig.path = paste0(file.path(Sys.getenv("HOME"),"Projects",.projName,"figs"),"/"),
                      dev="png", 
                      dpi=96)

knitr::opts_knit$set(root.dir = .proj)


