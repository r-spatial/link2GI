if ( !isGeneric("initSAGA") ) {
  setGeneric("initSAGA", function(x, ...)
    standardGeneric("initSAGA"))
}
#' initSAGA setup SAGA binaries 
#'@description The function initSAGA trys to locate all valid SAGA installations 
#'  and returns the pathes and correct environment settings. All valid means that it
#'  looks for the \code{saga_cmd} or \code{saga_cmd.exe} executable of the SAGA CLI. If the file is found it is assumed to be a valid SAGA binary installation. 
#'@note The SAGA wrapper for R RSAGA is NOT used. The reason is the weird concept of the SAGA developers which are always changing the System calls and so forth. Due to this developing design RSAGA is only running stable with specific versions of SAGA. 
#'@param defaultSAGA string contains path to SAGA binaries
#'@param DL drive letter
#'@param MP mount point
#'
#'@return 
#' add SAGA pathes to the enviroment and creates global variables sagaPath, sagaModPath and sagaCmd
#' 
#'@export initSAGA
#'
#'@examples
#'\dontrun{
#'
#' # full search
#'initSAGA()
#'
#' typical OSGeo4W64 installation 
#'initSAGA(c("C:\\OSGeo4W64\\apps\\saga","C:\\OSGeo4W64\\apps\\saga\\modules"))
#'}


initSAGA <- function(defaultSAGA = NULL, DL = "C:", MP="/usr"){
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

