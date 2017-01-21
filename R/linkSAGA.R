if ( !isGeneric("linkSAGA") ) {
  setGeneric("linkSAGA", function(x, ...)
    standardGeneric("linkSAGA"))
}

#'@title Locate and bind valid SAGA installation(s)
#'@name linkSAGA
#'@description Locate and bind valid 'SAGA GIS' \url{http://www.saga-gis.org/}
#'  installation(s). It returns the pathes and correct environment settings. All
#'  valid means that it looks for the \code{saga_cmd} or \code{saga_cmd.exe}
#'  executable of the SAGA CLI. If the file is found it is assumed to be a valid
#'  SAGA binary installation.
#'@note The excellent SAGA wrapper \href{https://CRAN.R-project.org/package=RSAGA}{RSAGA} is NOT used because the
#'  permanent occucrence of breaking the API calls makes it highly impracticable
#'  to keep in line. RSAGA fits perfectly your needs if you use 'SAGA GIS'
#'  (2.0.4 - 2.2.3).
#'  
#'@param defaultSAGA string contains path to SAGA binaries
#'@param DL drive letter
#'@param MP mount point
#'  
#'@details Adds SAGA pathes to the environment and creates the global variables
#'sagaPath, sagaModPath and sagaCmd
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


linkSAGA <- function(defaultSAGA = NULL, DL = "C:", MP="/usr"){
  # (R) set pathes  of SAGA modules and binaries depending on OS  
  exist <- FALSE
  if (Sys.info()["sysname"] == "Windows") {
    if (is.null(defaultSAGA)) defaultSAGA <- searchSAGA4W(DL = DL) 
    # take the first return
    makGlobalVar("sagaCmd", paste0(defaultSAGA[[1]][1],"\\saga_cmd.exe"))
    makGlobalVar("sagaPath", defaultSAGA[[1]][1])
    if (!is.null(defaultSAGA[[2]][1])) makGlobalVar("sagaModPath",  defaultSAGA[[2]][1])
    
    add2Path(defaultSAGA[[1]][1])
    add2Path(defaultSAGA[[2]][1])
    
  } 
  # if Linux
  else {
    
    if (is.null(defaultSAGA)) {
      
      defaultSAGA[1] <- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'saga_cmd' -print"), stdout = TRUE)
      defaultSAGA[2] <- substr(defaultSAGA,1,nchar(defaultSAGA) - 9)
      rawSAGALib <- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'libio_gdal.so' -print"), stdout = TRUE)
      defaultSAGA[3] <- substr(rawSAGALib,1,nchar(rawSAGALib) - 14)
    }
    makGlobalVar("sagaCmd", defaultSAGA[1])
    makGlobalVar("sagaPath", defaultSAGA[2])
    makGlobalVar("sagaModPath",  defaultSAGA[3])
    add2Path(defaultSAGA[2])
    add2Path(defaultSAGA[3])
  }
}

