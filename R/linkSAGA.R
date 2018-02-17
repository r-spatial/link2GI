if ( !isGeneric("linkSAGA") ) {
  setGeneric("linkSAGA", function(x, ...)
    standardGeneric("linkSAGA"))
}

#'@title Identifies SAGA GIS Installations and returns linking Informations 
#'@name linkSAGA
#'@description Finds the existing \href{http://www.saga-gis.org/}{SAGA GIS} installation(s), 
#'generates and sets the necessary path and system variables for a seamless use of the command 
#'line calls of the 'SAGA GIS' CLI API, setup valid system variables for calling a default 
#'\code{rsaga.env} and by this makes available the \code{RSAGA} wrapper functions.\cr
#'All existing installation(s) means that it looks for the \code{saga_cmd} or \code{saga_cmd.exe} 
#'executables. If the file is found it is assumed to be a valid 'SAGA GIS' installation.
#'@note The excellent 'SAGA GIS' wrapper \href{https://CRAN.R-project.org/package=RSAGA}{RSAGA} 
#'is in line for a major update however it covers currently (Feb 2018) only 'SAGA GIS' 
#'versions from 2.0.4 - 2.2.3. The fast evolution of 'SAGA GIS' makes it highly impracticable
#'to keep the wrapper adaptions in line. \code{RSAGA} will meet all linking needs perfectly if 
#'you use 'SAGA GIS' versions from 2.0.4 - 2.2.3. 
#'@return a list containing the selected \code{RSAGA} path variables \code{$sagaPath},\code{$sagaModPath},\code{$sagaCmd} and potentially other installations \code{$installed}  
#'@param default_SAGA string contains path to \code{RSAGA} binaries
#'@param searchLocation drive letter to be searched, for Windows systems default
#' is \code{C:}, for Linux systems default is \code{/usr}.
#'@param ver_select boolean default is FALSE. If there is more than one 'SAGA GIS' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'SAGA GIS' version 
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@param returnPaths boolean if set to FALSE the pathes of the selected version are written 
#' to the PATH variable only, otherwise all paths and versions of the installed SAGA versions ae returned.#'@details If called without any parameter \code{linkSAGA()} it performs a full search over \code{C:}. If it finds one or more 'SAGA GIS' binaries it will take the first hit. You have to set \code{ver_select = TRUE} for an interactive selection of the preferred version. Additionally the selected SAGA pathes are added to the environment and the global variables \code{sagaPath}, \code{sagaModPath} and \code{sagaCmd} will be created.
#'
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
#' # typical OSGeo4W64 installation 
#' saga <- linkSAGA(c("C:/OSGeo4W64/apps/saga","C:/OSGeo4W64/apps/saga/modules"))
#' # overriding the default environment using RSAGA and assuming you have 3 SAGA installations
#' saga<-linkSAGA()
#' RSAGA::rsaga.env(path = saga$installed$binDir[3],modules = saga$installed$moduleDir[3])
#'}


linkSAGA <- function(default_SAGA = NULL, 
                     searchLocation = "default", 
                     ver_select=FALSE,
                     quiet = TRUE,
                     returnPaths =FALSE){
  # (R) set pathes  of SAGA modules and binaries depending on OS  
  exist <- FALSE
  
  if (Sys.info()["sysname"] == "Windows") {
    if (is.null(default_SAGA)) default_SAGA <- findSAGA(searchLocation = searchLocation,
                                                        quiet = quiet) 
    # take the first return
    if (nrow(default_SAGA) == 1) {  
      sagaCmd <- paste0(default_SAGA[[1]][1],"\\saga_cmd.exe")
      sagaPath <- gsub("\\\\$", "", default_SAGA[[1]][1])
      if (!is.null(default_SAGA[[2]][1])) 
        sagaModPath <- paste0(default_SAGA[[2]][1],"\\modules")
    } else if (nrow(default_SAGA) > 1  & ver_select) { 
      
      cat("You have more than one valid SAGA GIS version\n")
      print(default_SAGA)
      cat("\n")
      cat("Choose a number: \n")
      ver<-readinteger()  
      default_saga <- gsub("\\\\$", "", default_SAGA[[1]][ver])
      sagaCmd <- paste0(default_SAGA[[1]][ver],"\\saga_cmd.exe")
      sagaPath <- default_saga
      sagaModPath <- paste0(default_SAGA[[1]][ver],"\\tools")
      
    }
    else if (nrow(default_SAGA) > 1  & !ver_select) { 
      
      if (!quiet) cat("You have more than one valid SAGA GIS version\n")
      if (!quiet) print(default_SAGA[[1]])
      if (!quiet) cat("\nI use the first one...\n")
      default_saga <- gsub("\\\\$", "", default_SAGA[[1]][1])
      sagaCmd <- paste0(default_SAGA[[1]][1],"\\saga_cmd.exe")
      sagaPath <- default_saga
      sagaModPath <- paste0(default_SAGA[[1]][1],"\\modules")
    }
  } 
  # if Linux
  else {
    
    if (is.null(default_SAGA)) {
      default_SAGA <- findSAGA(searchLocation = searchLocation,
                               quiet = quiet) 
      #print( default_SAGA[[2]][1])
      # take the first return
      if (nrow(default_SAGA) == 1) {  
        sagaCmd <- paste0(default_SAGA[[1]][1],"/saga_cmd")
        sagaPath <- gsub("//$", "", default_SAGA[[1]][1])
        sagaModPath <- default_SAGA[[2]][1]
      } else if (nrow(default_SAGA) > 1  & ver_select) { 
        
        cat("You have more than one valid SAGA GIS version\n")
        print(default_SAGA)
        cat("\n")
        cat("Choose a number: \n")
        ver<-readinteger()
        default_saga <- gsub("\\\\$", "", default_SAGA[[1]][ver])
        sagaCmd <- paste0(default_SAGA[[1]][ver],"/saga_cmd")
        sagaPath <- default_saga
        sagaModPath <- paste0(default_SAGA[[2]][ver])
      }
      else if (nrow(default_SAGA) > 1  & !ver_select) { 
        default_saga <- gsub("\\\\$", "", default_SAGA[[1]][1])
        sagaCmd <- paste0(default_SAGA[[1]][1],"\\saga_cmd")
        sagaPath <- default_SAGA[[1]][1]
        sagaModPath <- paste0(default_SAGA[[2]][1])
      }
    }
  }
  
  # workaround for RSAGA that is constructing a default module path only
  Sys.setenv(SAGA_MLB = sagaModPath)
  add2Path(sagaPath)
  saga<-list()
  saga$sagaPath<-sagaPath
  saga$sagaModPath <- sagaModPath
  saga$sagaCmd <- sagaCmd
  saga$installed <- default_SAGA
  if (returnPaths) return(saga)
}

