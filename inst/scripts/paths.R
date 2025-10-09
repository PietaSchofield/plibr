#
# This file is highly specific and only works with my file structure
# 

.homeDir <- file.path(Sys.getenv("HOME"))
.sysRoot <- .homeDir
.mntPnt <- file.path("/","media","pietas")
.mDrive <- file.path(.mntPnt,"mdrive")
.oneDrive <- file.path(.homeDir,"OneDrive")
.cprdDrive <- file.path(.mntPnt,"cprdgi")
.projDrive <- file.path(.mntPnt,.projName)
.uDrive <- file.path("/media","pietas","unix")
.codeDir <- file.path(.sysRoot,"GitLab",.gitRepo,.projName)
.projDir <- file.path(.uDrive,"Projects",.projName)
.oneDir <- file.path(.oneDrive,"ul","Projects",.projName)
.locDir <- file.path(.homeDir,"Projects")
.projLoc <- file.path(.locDir,.projName)
.dataDir <- file.path(.projDir,".data")
.locData <- file.path(.projLoc,".data")
.tmpDir <- file.path(.homeDir,".tmp")
.credDir <- file.path(.oneDrive,".config")
#
.tmpFile <- file.path(.tmpDir,paste0(.fileName,".rdata"))
if(file.exists(.tmpFile)) load(.tmpFile)
