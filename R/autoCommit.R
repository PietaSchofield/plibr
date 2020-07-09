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
autoCommit <- function(projDir=file.path(Sys.getenv("HOME"),"GitLab"),
                       commitMessage="Automated ",oncampus=F, mws=F){
  if(Sys.info()["sysname"]!="Windows") mws=T
  if(oncampus & mws){
    projDir="M:/Gitlab"
  }
  dirs <- list.files(projDir,pattern=".*", include.dirs=T, no..=T, full=T)
  retValue <- lapply(dirs,function(d){
    pushRes <- "No Changes"
      repDir <- d
      repo <- git2r::repository(repDir)
      #
      # Kludge to get round lake of protocol for fetch
      #
      if(mws){
        system(paste0("cd ",repDir,"; git fetch origin"),intern=F)
      }else{
        git2r::fetch(repo,name="origin")
      }
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
  return(writeLines(unlist(retValue)))
}

