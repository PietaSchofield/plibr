#' Render the current Quarto document
#'
#' This function renders a Quarto document and saves the output in a local location.
#'
#' @param fileName file name (without extension)
#' @param projName project name
#' @param gitRepo Git repository (default "liverpool")
#' @param sysRoot home directory
#' @param user User id
#' @param outPath local directory where rendered files will be written
#' @param nbPath location for HTML output intended for server use
#' @param codePath path to source code (the Quarto document)
#' @param docPath (unused) document tree path
#' @param silent if TRUE, do not automatically display the output URL
#' @param setHome if TRUE, the rendered HTML file will be named "index.html"
#' @param toPDF if TRUE, render PDF output
#' @param toDOCX if TRUE, render DOCX output
#' @param toHTML if TRUE, render HTML output
#' @param htmlUP if TRUE, push the HTML file to the nbPath location (if available)
#' @param ext file extension, default is "qmd" for Quarto documents
#' @param dbg debug flag (unused)
#'
#' @export
compile_quarto <- function(fileName = .fileName,
                                  projName = .projName,
                                  gitRepo = "liverpool",
                                  sysRoot = Sys.getenv("HOME"),
                                  user = Sys.getenv("USER"),
                                  outPath = file.path(Sys.getenv("HOME"), "Projects"),
                                  nbPath = file.path("/srv", "http"),
                                  codePath = file.path(sysRoot, "GitLab", gitRepo),
                                  docPath = file.path(sysRoot, "Projects"),
                                  silent = FALSE,
                                  setHome = FALSE,
                                  toPDF = FALSE,
                                  toDOCX = FALSE,
                                  toHTML = TRUE,
                                  ext = "qmd",
                                  dbg = FALSE) {
  # Set paths based on the repository type
  if (gitRepo == "liverpool") {
    nbPath <- file.path(nbPath, "uol")
    outPath <- file.path(outPath)
  } else if (gitRepo == "personal") {
    nbPath <- file.path(nbPath, "pers")
    outPath <- file.path(outPath, "pers")
  } else {
    nbPath <- file.path(nbPath, gitRepo)
    outPath <- file.path(outPath, gitRepo)
  }
  
  if (!is.null(projName)) {
    codePath <- file.path(codePath, projName)
    nbPath   <- file.path(nbPath, projName)
    outPath  <- file.path(outPath, projName, "pubs")
  }
  
  dir.create(nbPath, showWarnings = FALSE, recursive = TRUE)
  dir.create(outPath, showWarnings = FALSE, recursive = TRUE)
  infile <- file.path(codePath, paste0(fileName, ".", ext))
  if(setHome){
    fileName <- "index"
  }else{
    fileName <- fileName
  }
  
  # Render DOCX if requested
  if (toDOCX) {
    outFile <- file.path(outPath,paste0(fileName,".docx"))
    out <- quarto::quarto_render(
      input = basename(infile),
      output_format = "docx",
      output_file = basename(outFile)
    )
    tmpFile <- file.path(codePath,basename(outFile))
    file.rename(from=tmpFile,to=outFile)
  }
  # Render HTML if requested
  if (toHTML) {
    outFile <- file.path(nbPath,paste0(fileName,".html"))
    out <- quarto::quarto_render(
      input = basename(infile),
      output_format = "html",
      output_file = basename(outFile)
      )
    tmpFile <- file.path(codePath,basename(outFile))
    file.rename(from=tmpFile,to=outFile)
  }
  # Render PDF if requested
  if (toPDF) {
    outFile <- file.path(outPath,paste0(fileName,".pdf"))
    out <- quarto::quarto_render(
      input = basename(infile),
      output_format = "pdf",
      output_file = basename(outFile)
    )
    tmpFile <- file.path(codePath,basename(outFile))
    file.rename(from=tmpFile,to=outFile)
  }
 
  # Optionally display the output URL (uses RCurl to check URL existence)
  if (!silent) {
    urlout <- gsub("/srv/http/", "http://localhost/", basename(tmpFile))
    if (RCurl::url.exists(urlout)) {
      displayURL(urlout)
    } else {
      displayURL(tmpFile)
    }
  }
}

