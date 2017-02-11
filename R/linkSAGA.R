if ( !isGeneric("linkSAGA") ) {
  setGeneric("linkSAGA", function(x, ...)
    standardGeneric("linkSAGA"))
}

#'@title Locate and bind valid SAGA installation(s)
#'@name linkSAGA
#'@description Locate and bind valid \href{http://www.saga-gis.org/}{SAGA GIlink2GI::linkSAGA()S}
#'  installation(s). It returns the pathes and correct environment settings. All
#'  valid means that it looks for the \code{saga_cmd} or \code{saga_cmd.exe}
#'  executables. If the file is found it is assumed to be a valid 'SAGA GIS' installation.
#'@note The excellent 'SAGA GIS' wrapper \href{https://CRAN.R-project.org/package=RSAGA}{RSAGA} is NOT used because the
#'  the developemt of 'SAGA GIS' breaks permanently the API call syntax. This fact makes it highly impracticable
#'  to keep with the wrapper adaptions in line. \code{RSAGA} will meet perfectly your needs if you use 'SAGA GIS' versions from 2.0.4 - 2.2.3.
#'  
#'@param defaultSAGA string contains path to SAGA binaries
#'@param DL drive letter
#'@param MP mount point
#'@param ver_select boolean default is FALSE. If there is more than one 'SAGA GIS' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'SAGA GIS' version 
#'@details If called without any parameter \code{linkSAGA()} it performs a full search over \code{C:}. If it finds one or more 'SAGA GIS' binaries it will take the first hit. You have to set \code{ver_select = TRUE} for an interactive selection of the preferred version. Additionally the selected SAGA pathes are added to the environment and the global variables \code{sagaPath}, \code{sagaModPath} and \code{sagaCmd} will be created.
#'
#'@export linkSAGA
#'  
#'@examples
#'\dontrun{
#'
#' # call if you do not have any idea if and where SAGA GIS is installed
#' linkSAGA()
#'
#' # typical OSGeo4W64 installation 
#' linkSAGA(c("C:/OSGeo4W64/apps/saga","C:/OSGeo4W64/apps/saga/modules"))
#'}


linkSAGA <- function(defaultSAGA = NULL, 
                     DL = "C:", 
                     MP="/usr",
                     ver_select=FALSE){
  # (R) set pathes  of SAGA modules and binaries depending on OS  
  exist <- FALSE
  if (Sys.info()["sysname"] == "Windows") {
    if (is.null(defaultSAGA)) defaultSAGA <- searchSAGAW(DL = DL, ver_select = ver_select) 
    # take the first return
    if (nrow(defaultSAGA) == 1) {  
    makGlobalVar("sagaCmd", paste0(defaultSAGA[[1]][1],"\\saga_cmd.exe"))
    makGlobalVar("sagaPath", defaultSAGA[[1]][1])
    if (!is.null(defaultSAGA[[2]][1])) makGlobalVar("sagaModPath",  defaultSAGA[[2]][1])
    add2Path(defaultSAGA[[1]][1])
    add2Path(defaultSAGA[[2]][1])
    } else if (nrow(defaultSAGA) > 1  & ver_select) { 
      
        cat("You have more than one valid SAGA GIS version\n")
        print(defaultSAGA)
        cat("\n")
        ver <- as.numeric(readline(prompt = "Please choose one:  "))
        makGlobalVar("sagaCmd", paste0(defaultSAGA[[1]][ver],"\\saga_cmd.exe"))
        makGlobalVar("sagaPath", defaultSAGA[[1]][ver])
        add2Path(defaultSAGA[[1]][ver])
    }
    else if (nrow(defaultSAGA) > 1  & !ver_select) { 
      
      cat("You have more than one valid SAGA GIS version\n")
      print(defaultSAGA[[1]])
      cat("\nTake the first one...\n")
      makGlobalVar("sagaCmd", paste0(defaultSAGA[[1]][1],"\\saga_cmd.exe"))
      makGlobalVar("sagaPath", defaultSAGA[[1]][1])
      add2Path(defaultSAGA[[1]][1])
    }
  } 
  # if Linux
  else {
    
    if (is.null(defaultSAGA)) {
      
      defaultSAGA[1] <- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'saga_cmd' -print"), stdout = TRUE)
      defaultSAGA[2] <- substr(defaultSAGA,1,nchar(defaultSAGA) - 9)
      rawSAGALib <-     system2("find", paste(MP," ! -readable -prune -o -type f  -iname 'libio_gdal.so' -print"), stdout = TRUE)
      defaultSAGA[3] <- substr(rawSAGALib,1,nchar(rawSAGALib) - 14)
    }
    makGlobalVar("sagaCmd", defaultSAGA[1])
    makGlobalVar("sagaPath", defaultSAGA[2])
    makGlobalVar("sagaModPath",  defaultSAGA[3])
    add2Path(defaultSAGA[2])
    add2Path(defaultSAGA[3])
  }
}

