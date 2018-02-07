if ( !isGeneric("linkSAGA") ) {
  setGeneric("linkSAGA", function(x, ...)
    standardGeneric("linkSAGA"))
}

#'@title Locate and bind valid SAGA installation(s)
#'@name linkSAGA
#'@description Locate and bind valid \href{http://www.saga-gis.org/}{SAGA}
#'  installation(s). It returns the pathes and correct environment settings. All
#'  valid means that it looks for the \code{saga_cmd} or \code{saga_cmd.exe}
#'  executables. If the file is found it is assumed to be a valid 'SAGA GIS' installation.
#'@note The excellent 'SAGA GIS' wrapper \href{https://CRAN.R-project.org/package=RSAGA}{RSAGA} is NOT used because the
#'  it currently covers only 'SAGA GIS' versions from 2.0.4 - 2.2.3. The fast SAGA GIS changes makes it highly impracticable
#'  to keep the wrapper adaptions in line. \code{RSAGA} will meet all linking needs perfectly if you use 'SAGA GIS' versions from 2.0.4 - 2.2.3.
#'  
#'@param default_SAGA string contains path to SAGA binaries
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
#' # setting the environment using RSAGA
#' RSAGA::rsaga.env(path = sagaPath, modules = sagaModPath)
#'}


linkSAGA <- function(default_SAGA = NULL, 
                     DL = "C:", 
                     MP="/usr",
                     ver_select=FALSE){
  # (R) set pathes  of SAGA modules and binaries depending on OS  
  exist <- FALSE
  if (Sys.info()["sysname"] == "Windows") {
    if (is.null(default_SAGA)) default_SAGA <- searchSAGAW(DL = DL, ver_select = ver_select) 
    # take the first return
    if (nrow(default_SAGA) == 1) {  
    makGlobalVar("sagaCmd", paste0(default_SAGA[[1]][1],"\\saga_cmd.exe"))
    spa<-gsub("\\\\$", "", default_SAGA[[1]][1])
    makGlobalVar("sagaPath", spa)
    if (!is.null(default_SAGA[[2]][1])) makGlobalVar("sagaModPath",  paste0(default_SAGA[[2]][1],"modules"))
    add2Path(spa)
    
    } else if (nrow(default_SAGA) > 1  & ver_select) { 
      
        cat("You have more than one valid SAGA GIS version\n")
        print(default_SAGA)
        cat("\n")
        ver <- as.numeric(readline(prompt = "Please choose one:  "))
        default_saga <- gsub("\\\\$", "", default_SAGA[[1]][ver])
        makGlobalVar("sagaCmd", paste0(default_SAGA[[1]][ver],"\\saga_cmd.exe"))
        makGlobalVar("sagaPath", default_saga)
        makGlobalVar("sagaModPath", paste0(default_SAGA[[1]][ver],"modules"))
        add2Path(default_saga)
        #env<-RSAGA::rsaga.env(path = sagaPath, modules = sagaModPath)
    }
    else if (nrow(default_SAGA) > 1  & !ver_select) { 
      
      cat("You have more than one valid SAGA GIS version\n")
      print(default_SAGA[[1]])
      cat("\nTake the first one...\n")
      default_saga <- gsub("\\\\$", "", default_SAGA[[1]][1])
      makGlobalVar("sagaCmd", paste0(default_SAGA[[1]][1],"\\saga_cmd.exe"))
      makGlobalVar("sagaPath", default_saga)
      makGlobalVar("sagaModPath", paste0(default_SAGA[[1]][1],"\\modules"))
      add2Path(default_SAGA[[1]][1])
      add2Path(default_SAGA[[1]][1])
      #env<-RSAGA::rsaga.env(path = sagaPath, modules = sagaModPath)
    }
  } 
  # if Linux
  else {
    
    if (is.null(default_SAGA)) {
      
      default_SAGA <- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'saga_cmd' -print"), stdout = TRUE)
      default_SAGA[2] <- substr(default_SAGA[1],1,nchar(default_SAGA[1]) - 9)
      rawSAGALib <-     system2("find", paste(MP," ! -readable -prune -o -type f  -iname 'libio_gdal.so' -print"), stdout = TRUE)
      default_SAGA[3] <- substr(rawSAGALib[1],1,nchar(rawSAGALib[1]) - 14)
    }
    makGlobalVar("sagaCmd", default_SAGA[1])
    makGlobalVar("sagaPath", default_SAGA[2])
    makGlobalVar("sagaModPath",  default_SAGA[3])
    add2Path(default_SAGA[2])
    add2Path(default_SAGA[3])
  }
}

