#
# This file is highly specific and only works with my file structure
# 
require(conflicted)
require(duckdb)
require(plibr)
require(knitr)
require(tidyverse)
require(dbplyr)
require(DT)
#
#
.homeDir <- file.path(Sys.getenv("HOME"))
.sysRoot <- .homeDir
.mntPnt <- file.path("/","media","pietas")
.oneDrive <- file.path(.homeDir,"OneDrive")
.projDrive <- file.path(.mntPnt,.projName)
.uDrive <- file.path("/media","pietas","unix")
.codeDir <- file.path(.sysRoot,"GitLab",.gitRepo,.projName)
.locDir <- file.path(.homeDir,"Projects")
.projLoc <- file.path(.locDir,.projName)
.locData <- file.path(.projLoc,".data")
.tmpDir <- file.path(.homeDir,".tmp")
.tmpFile <- file.path(.tmpDir,paste0(.fileName,".rdata"))
.credDir <- file.path(.oneDrive,".config")
.linksFile <- file.path(.homeDir,"GitLab",.gitRepo,"include","link.Rmd")
#
opts_chunk$set( message=F, warning=F,comment=NA,class.output="code")
options(scipen=999)
.cf <- T
if(file.exists(.tmpFile)) load(.tmpFile)
conflicts_prefer(DT::JS)
