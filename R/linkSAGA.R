if ( !isGeneric("linkSAGA") ) {
  setGeneric("linkSAGA", function(x, ...)
    standardGeneric("linkSAGA"))
}

#'@title Identifies SAGA GIS Installations and returns linking Informations 
#'@name linkSAGA
#'@description Finds the existing \href{https://saga-gis.sourceforge.io/}{SAGA GIS} installation(s), 
#'generates and sets the necessary path and system variables for a seamless use of the command 
#'line calls of the 'SAGA GIS' CLI API, setup valid system variables for calling a default 
#'\code{rsaga.env} and by this makes available the \code{RSAGA} wrapper functions.\cr
#'All existing installation(s) means that it looks for the \code{saga_cmd} or \code{saga_cmd.exe} 
#'executables. If the file is found it is assumed to be a valid 'SAGA GIS' installation. If it is called without any argument the most recent (i.e. highest) SAGA GIS version will be linked.
#'@note The excellent 'SAGA GIS' wrapper \href{https://CRAN.R-project.org/package=RSAGA}{RSAGA} 
#'package was updated several times however it covers currently (Dec 2019) only 'SAGA GIS' 
#'versions from 2.3.1 - 6.3.0 The fast evolution of 'SAGA GIS' makes it highly impracticable
#'to keep the wrapper adaptions in line (currently 7.5). \code{RSAGA} will meet all linking needs perfectly if 
#'you use 'SAGA GIS' versions from 2.0.4 - 7.5.0. \cr  However you must call \code{rsaga.env} using the \code{rsaga.env(modules = saga$sagaModPath)} assuming that \code{saga} contains the returnPaths of \code{linkSAGA} 
#'In addition most recently  the very promising  \href{https://github.com/stevenpawley/Rsagacmd}{Rsagacmd} wrapper package is providing a new list oriented wrapping tool.
#'@return A list containing the selected \code{RSAGA} path variables \code{$sagaPath},\code{$sagaModPath},\code{$sagaCmd} and potentially other installations \code{$installed}  
#'@param default_SAGA string contains path to \code{RSAGA} binaries
#'@param searchLocation drive letter to be searched, for Windows systems default
#' is \code{C:}, for Linux systems default is \code{/usr/bin}.
#'@param ver_select boolean default is FALSE. If there is more than one 'SAGA GIS' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'SAGA GIS' version 
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@param returnPaths boolean if set to FALSE the pathes of the selected version are written 
#' to the PATH variable only, otherwise all paths and versions of the installed SAGA versions ae returned.#'@details If called without any parameter \code{linkSAGA()} it performs a full search over \code{C:}. If it finds one or more 'SAGA GIS' binaries it will take the first hit. You have to set \code{ver_select = TRUE} for an interactive selection of the preferred version. Additionally the selected SAGA pathes are added to the environment and the global variables \code{sagaPath}, \code{sagaModPath} and \code{sagaCmd} will be created.

#'@export linkSAGA
#'  
#'@examples
#'\dontrun{
#'
#' # call if you do not have any idea if and where SAGA GIS is installed
#' # it will return a list with the selected and available SAGA installations
#' # it prepares the system for running the selected SAGA version via RSAGA or CLI
#' linkSAGA()
#'
#' # overriding the default environment of rsaga.env call 
#' 
#' saga<-linkSAGA()
#' if (saga$exist) {
#' require(RSAGA)
#' RSAGA::rsaga.env(path = saga$installed$binDir[1],modules = saga$installed$moduleDir[1])
#' }
#'}


linkSAGA <- function(default_SAGA = NULL, 
                     searchLocation = "default", 
                     ver_select=FALSE,
                     quiet = TRUE,
                     returnPaths = TRUE){
  # (R) set pathes  of SAGA modules and binaries depending on OS  
  exist <- FALSE
  if (ver_select =='T') ver_select <- TRUE
  if (ver_select == "F" && !is.numeric(ver_select)) ver_select <- FALSE
  scmd <- ifelse(Sys.info()["sysname"]=="Windows", "saga_cmd.exe", "saga_cmd")
  removePattern <- ifelse(Sys.info()["sysname"]=="Windows", "\\\\$", "/$")
  sep = ifelse(Sys.info()["sysname"]=="Windows", "\\", "/")
  
  # if no default_SAGA is given search for SAGA installations
  if (is.null(default_SAGA)) 
    default_SAGA <- findSAGA(searchLocation = searchLocation,
                             quiet = quiet) 
  if (default_SAGA[[1]][1] != FALSE) {
  # only one SAGA installation found/given
  if (nrow(default_SAGA) == 1) {  
    sagaCmd <- paste0(default_SAGA[[1]][1],sep,scmd )
    sagaPath <- gsub(removePattern, "", default_SAGA[[1]][1])
    if (!is.null(default_SAGA[[2]][1])) 
      if (getSagaVer(sagaPath) >= "3.0.0" && Sys.info()["sysname"]=="Windows")
        sagaModPath <- paste0(default_SAGA[[1]][1],sep,"tools" )
    else if (getSagaVer(sagaPath) < "3.0.0" && Sys.info()["sysname"]=="Windows") 
      sagaModPath <- paste0(default_SAGA[[1]][1],sep,"modules" )
    else sagaModPath <- paste0(default_SAGA[[2]][1])
  } 
  # more than one SAGA installation and ver_select = TRUE
  else if (nrow(default_SAGA) > 1  & ver_select) { 
    cat("You have installed more than one SAGA GIS version\n")
    print(default_SAGA)
    cat("\n")
    cat("Choose version: \n")
    sagaVersion<-readinteger()  
    default_saga <- gsub(removePattern, "", default_SAGA[[1]][sagaVersion])
    sagaCmd <- paste0(default_SAGA[[1]][sagaVersion],sep,scmd )
    sagaPath <- default_saga
    if (getSagaVer(sagaPath) >= "3.0.0" && Sys.info()["sysname"]=="Windows") {
      sagaModPath <- paste0(default_SAGA[[1]][sagaVersion],sep,"tools" )
      #system(paste0( "mklink /d /h  ",paste0(default_SAGA[[1]][sagaVersion],sep,"modules ", default_SAGA[[1]][sagaVersion],sep,"tools" )))
    }
    else if (getSagaVer(sagaPath) < "3.0.0" && Sys.info()["sysname"]=="Windows") 
      sagaModPath <- paste0(default_SAGA[[1]][sagaVersion],sep,"modules" )
    else sagaModPath <- paste0(default_SAGA[[2]][sagaVersion])
  }  # more than one SAGA installation and ver_select >0
  else if (nrow(default_SAGA) > 1  & is.numeric(ver_select) & ver_select > 0) { 
    cat("You have installed more than one SAGA GIS version.\n")
    print(default_SAGA)
    cat("Your have choosen version: ",ver_select,"\n")
    default_saga <- gsub(removePattern, "", default_SAGA[[1]][ver_select])
    sagaCmd <- paste0(default_SAGA[[1]][ver_select],sep,scmd )
    sagaPath <- default_saga
    if (getSagaVer(sagaPath) >= "3.0.0" && Sys.info()["sysname"]=="Windows") {
      sagaModPath <- paste0(default_SAGA[[1]][ver_select],sep,"tools" )
      #system(paste0( "mklink /d /h  ",paste0(default_SAGA[[1]][ver_select],sep,"modules ", default_SAGA[[1]][sagaVersion],sep,"tools" )))
    }
    else if (getSagaVer(sagaPath) < "3.0.0" && Sys.info()["sysname"]=="Windows") 
      sagaModPath <- paste0(default_SAGA[[1]][ver_select],sep,"modules" )
    else sagaModPath <- paste0(default_SAGA[[2]][ver_select])
  }
  
  # more than one installation and ver_select =FALSE 
  # => automatic selection of the newest SAGA
  else if (nrow(default_SAGA) > 1  & ver_select!="TRUE" ) { 
    recentSaga <- getrowSagaVer(default_SAGA)
    default_saga <- gsub(removePattern, "", default_SAGA[[1]][recentSaga])
    sagaCmd <- paste0(default_SAGA[[1]][recentSaga],sep,scmd )
    sagaPath <- default_saga
    if (getSagaVer(sagaPath) >= "3.0.0" && Sys.info()["sysname"]=="Windows") {
      #system(paste0( "mklink /d /h  ",paste0(default_SAGA[[1]][recentSaga],sep,"modules ", default_SAGA[[1]][recentSaga],sep,"tools" )))
      sagaModPath <- paste0(default_SAGA[[1]][recentSaga],sep,"tools")
    }
    else if (getSagaVer(sagaPath) < "3.0.0" && Sys.info()["sysname"]=="Windows") 
      sagaModPath <- paste0(default_SAGA[[1]][recentSaga],sep,"modules")
    else sagaModPath <- paste0(default_SAGA[[2]][recentSaga])
  }
  sagaModPath <-gsub(removePattern, "", sagaModPath )
  # SAGA_MLB is only used by Linux-RSAGA to identify the correct module path 
  Sys.setenv(SAGA_MLB = sagaModPath)
  # add saga bin folder to the systemwide search path
  add2Path(sagaPath)
  # create return list with all folders
  saga<-list()
  # if (grepl( pattern = " ",sagaPath)) sagaPath<-shQuote(R.utils::getAbsolutePath(sagaPath))
  # if (grepl( pattern = " ",sagaModPath)) sagaModPath<-shQuote(R.utils::getAbsolutePath(sagaModPath))
  # if (grepl( pattern = " ",sagaCmd)) sagaCmd<-shQuote(R.utils::getAbsolutePath(sagaCmd))
  saga$sagaPath<-sagaPath
  saga$sagaModPath <- sagaModPath
  saga$sagaCmd <- sagaCmd
  saga$installed <- default_SAGA
  saga$exist<-TRUE
} else saga$exist <- FALSE
  if (returnPaths) return(saga)
}

