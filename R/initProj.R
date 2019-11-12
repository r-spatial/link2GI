if ( !isGeneric("initProj") ) {
  setGeneric("initProj", function(x, ...)
    standardGeneric("initProj"))
}

#'@title Defines and creates folders and variables
#'@name initProj
#'@description Defines and creates (if necessary) all folders variables. Returns a list with the project folder pathes. Optionally exports all pathes to a global sub environment.
#'
#'@param projRootDir  project github root directory (your github name)
#'@param projFolders list of subfolders in project
#'@param GRASSlocation folder for GRASS data
#'@param global boolean esport path strings as global variables default is false
#'@param path_prefix character a prefix for the path variables names default is ""
#'
#'@export initProj
#'@examples 
#'\dontrun{
#'
#'link2GI::initProj(projRootDir = tempdir(),
#'                  projFolders = c("data/",
#'                                  "data/level0/",
#'                                  "data/level1/",
#'                                   "output/",
#'                                   "run/",
#'                                   "fun/") )
#'}   


initProj <- function(projRootDir=tempdir(), 
                     GRASSlocation = "tmp/", 
                     projFolders=c("data/","result/","run/","log/"),
                     path_prefix="",
                     global = FALSE) {
  
  # switch backslash to slash and expand path to full path
  projRootDir <- gsub("\\\\", "/", path.expand(projRootDir))  
  projRootDir <- gsub("///", "/", path.expand(projRootDir))  
  
  pth<-list()
  # check  tailing / and if not existing append
  if (substr(projRootDir,nchar(projRootDir) - 1,nchar(projRootDir)) != "/") {
    projRootDir <- paste0(projRootDir,"/")
    #name = ifelse(Sys.info()["sysname"]=="Windows", sub("/$", "",projRootDir),projRootDir)
  }

    # create directories if needed
    for (folder in projFolders) {
      if (!file.exists(file.path(projRootDir,folder))) {
        dir.create(file.path(projRootDir,folder), recursive = TRUE)
        p<-gsub("/", "_", substr(folder,1,nchar(folder) - 1))
        name <- paste0(path_prefix,p)
        value <- paste0(projRootDir,folder)
       # value = ifelse(Sys.info()["sysname"]=="Windows", sub("/$", "",value),value)
        assign(name, value)
        pth[[name]]<- value
        if (global) makGlobalVar(name, value)
        } else {
        p<-gsub("/", "_", substr(folder,1,nchar(folder) - 1))
        name <- paste0(path_prefix,p)          
        value <- paste0(projRootDir,folder)
       # value = ifelse(Sys.info()["sysname"]=="Windows", sub("/$", "",value),value)
        assign(name, value)
        pth[[name]]<- value
        if (global) makGlobalVar(name, value)
        } 
    }
  if (!file.exists(file.path(projRootDir,GRASSlocation))) {
    dir.create(file.path(projRootDir,GRASSlocation), recursive = TRUE)
    p<-gsub("/", "_", substr(GRASSlocation,1,nchar(GRASSlocation) - 1))
    name <- paste0(path_prefix,p)
    value <- paste0(projRootDir,GRASSlocation)
    #value = ifelse(Sys.info()["sysname"]=="Windows", sub("/$", "",value),value)
    assign(name, value)
    pth[[name]]<- value
    if (global) makGlobalVar(name, value)
  } else {
    p<-gsub("/", "_", substr(GRASSlocation,1,nchar(GRASSlocation) - 1))
    name <- paste0(path_prefix,p)
    value <- paste0(projRootDir,GRASSlocation)
    #value = ifelse(Sys.info()["sysname"]=="Windows", sub("/$", "",value),value)
    assign(name, value)
    pth[[name]]<- value
    
    if (global) makGlobalVar(name, value)
  } 
  
  if(Sys.info()["sysname"]=="Windows")  pth <-stringr::str_replace(pth, "/$", "")
  return(pth) 
}
