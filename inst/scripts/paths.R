#
# This file is highly specific and only works with my file structure
# 

.homeDir <- file.path(Sys.getenv("HOME"))
.sysRoot <- .homeDir
.mntPnt <- file.path("/","media","pietas")
.uDrive <- file.path("/media","pietas","unix")
.mDrive <- file.path(.mntPnt,"mdrive")
.oneDrive <- file.path(.homeDir,"OneDrive")
.cprdDrive <- file.path(.mntPnt,"cprdgi")
.locDir <- file.path(.homeDir,"Projects")
if(!is.null(.projName)){
  .projDrive <- file.path(.mntPnt,.projName)
  .codeDir <- file.path(.sysRoot,"repositories",.gitRepo,.projName)
  .projDir <- file.path(.uDrive,"Projects",.projName)
  .oneDir <- file.path(.oneDrive,"ul","Projects",.projName)
  .projLoc <- file.path(.locDir,.projName)
}else{
  .projDrive <- file.path(.mntPnt)
  .codeDir <- file.path(.sysRoot,"repositories",.gitRepo)
  .projDir <- file.path(.uDrive,"Projects")
  .oneDir <- file.path(.oneDrive,"ul","Projects")
  .projLoc <- file.path(.locDir)
}
.dataDir <- file.path(.projDir,".data")
.locData <- file.path(.projLoc,".data")
.tmpDir <- file.path(.homeDir,".tmp")
.credDir <- file.path(.oneDrive,".config")
.proj <- .codeDir
#


