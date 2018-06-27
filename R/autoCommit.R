#' Loop through Project code committing and pulling as necessary
#' 
#' currently my build of libgit is not coping with HTTPs protocol need to fix this at some point
#' until then dropping to system shell to push and pull
#'
#' @param projDir Project Root
#' @param pat pattern for directory names
#' @param subdir names of subdirectories that include code
#'
#' @export
autoCommit <- function(projDir="~/Projects",pat="*",subdir="^(Code|R|package)$",
                       commitMessage="Automated "){
  dirs <- list.files(projDir,pattern=pat, include.dirs=T, no..=T, full=T)
  retValue <- writeLines(unlist(lapply(dirs,function(d){
    pushRes <- "No Changes"
    codeDir <- list.files(d,pattern="^(Code|R|package)$",full=T)
    if(length(codeDir)==0){
      return(paste0("No repository ",d))
    }
    lapply(codeDir,function(cdir){
      if(basename(cdir)=="R"){
        repDir <- d
      }else{
        repDir <- cdir
      }
      repo <- git2r::repository(repDir)
      #
      # Kludge to get round lake of protocol for fetch
      #
      system(paste0("cd ",repDir,"; git fetch origin"),intern=F)
      stat <- git2r::status(repo)
      if(length(stat$unstaged)>0 | length(stat$untracked)>0 ){
        git2r::add(repo=repo,file.path("*.*"))
      }
      stat <- git2r::status(repo)
      if(length(stat$staged)>0){
        git2r::commit(repo=repo,message=paste0(commitMessage," ",date()))
        #
        # Kludge to get round lake of protocol for push
        #
        if(system(paste0("cd ",repDir,"; git push"),intern=F)==0){
          pushRes <- "Pushed"
        }else{
          pushRes <- "Push Error"
        }
      }
      #
      # Kludge to get round lake of protocol for pull
      #
      system(paste0("cd ",repDir,"; git pull"),intern=T)
      paste0(pushRes," ",repDir)
    })
  })))
}

