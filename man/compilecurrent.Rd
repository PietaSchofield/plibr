% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compilecurrent.R
\name{compilecurrent}
\alias{compilecurrent}
\title{render the current document}
\usage{
compilecurrent(
  fileName = .fileName,
  projName = .projName,
  gitRepo = "liverpool",
  sysRoot = Sys.getenv("HOME"),
  user = Sys.getenv("USER"),
  oneDrive = F,
  nbPath = file.path("/srv", "http"),
  codePath = file.path(sysRoot, "GitLab", gitRepo),
  docPath = file.path(sysRoot, "Projects"),
  silent = F,
  setHome = F,
  toPDF = F,
  toDOCX = F,
  toHTML = T,
  htmlUP = T,
  ext = "Rmd",
  dbg = F
)
}
\arguments{
\item{fileName}{file name}

\item{sysRoot}{home directory}

\item{projDir}{project name}

\item{codeDir}{code tree path}

\item{userid}{User id}

\item{outRoot}{local temporary file creation location}
}
\description{
Latest changes are to stop pushing files up to an html server and to keep them local by default
}
\details{
This function has evolved way beyond the parameter list. It has also had to adapt from the original
dundee version to a version that worked with GoDaddy and then worked at Manchester CRUK and then cope
with the changes at CRUK and the whole reconfiguration due to the fire and then the  move to Liverpool
and the introduction of the virtual machine at Liverpook running a shiny server and the final dropping
of GoDaddy.
}
