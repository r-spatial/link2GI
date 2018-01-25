if ( !isGeneric("initProj") ) {
  setGeneric("initProj", function(x, ...)
    standardGeneric("initProj"))
}

#'@title Defines and creates folders and variables
#'@name initProj
#'@description Defines and creates (if necessary) all folders variables
#' set the SAGA path variables and other system variables
#' exports all variables to the global environment
#'
#'@param projRootDir  project github root directory (your github name)
#'@param projFolders list of subfolders in project
#'
#'@export initProj
#'   


initProj <- function(projRootDir=getwd(), GRASSlocation = "tmp/", projFolders=c("data/","result/","run/","log/")) {
  
  # switch backslash to slash and expand path to full path
  projRootDir <- gsub("\\\\", "/", path.expand(projRootDir))  
  
  # check  tailing / and if not existing append
  if (substr(projRootDir,nchar(projRootDir) - 1,nchar(projRootDir)) != "/") {
    projRootDir <- paste0(projRootDir,"/")
  }

    # create directories if needed
    for (folder in projFolders) {
      if (!file.exists(file.path(projRootDir,folder))) {
        dir.create(file.path(projRootDir,folder), recursive = TRUE)
        p<-gsub("/", "_", substr(folder,1,nchar(folder) - 1))
        name <- paste0("path_",p)
        value <- paste0(projRootDir,folder)
        makGlobalVar(name, value)
        } else {
        p<-gsub("/", "_", substr(folder,1,nchar(folder) - 1))
        name <- paste0("path_",p)          
        value <- paste0(projRootDir,folder)
        makGlobalVar(name, value)
        } 
    }
  if (!file.exists(file.path(projRootDir,GRASSlocation))) {
    dir.create(file.path(projRootDir,GRASSlocation), recursive = TRUE)
    p<-gsub("/", "_", substr(GRASSlocation,1,nchar(GRASSlocation) - 1))
    name <- paste0("path_",p)
    value <- paste0(projRootDir,GRASSlocation)
    makGlobalVar(name, value)
  } else {
    p<-gsub("/", "_", substr(GRASSlocation,1,nchar(GRASSlocation) - 1))
    name <- paste0("path_",p)
    value <- paste0(projRootDir,GRASSlocation)
    makGlobalVar(name, value)
  } 
  
}
