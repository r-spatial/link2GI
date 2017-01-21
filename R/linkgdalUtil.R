if (!isGeneric('linkgdalUtils')) {
  setGeneric('linkgdalUtils', function(x, ...)
    standardGeneric('linkgdalUtils'))
}

#'@title Check and export the gdalUtils settings
#'@name linkgdalUtils
#'@description  Check and export the gdalUtils settings. 
#'You need to have installed the 'GDAL' \url{http://www.gdal.org/} binaries.
#'@author CR
#'@return 
#' a list of the complete capabilities of the current installed GDAL version
#'@export linkgdalUtils 
#'@examples
#' \dontrun{
#'
#' # get all available driver 
#' gdal<- linkgdalUtils()
#' 
#' gdal[[1]]$drivers$format_code
#' 
#' # GET BINARY PATH 
#' gdal[[1]]$path
#' 
#' # get additional and available python tools
#' gdal[[1]]$python_utilities
#' }

linkgdalUtils <- function(){
  if (substr(Sys.getenv("COMPUTERNAME"),1,5) == "PCRZP") {
    gdalUtils::gdal_setInstallation(search_path = shQuote("C:/Program Files/QGIS 2.14/bin/"))
  } else {
  ## (gdalUtils) check for a valid GDAL binary installation on your system
    cat("\nsearching for GDAL binaries - this may take a while\n")
  gdalUtils::gdal_setInstallation()
  }
  valid.install <- !is.null(getOption("gdalUtils_gdalPath"))
  if (!valid.install) {
    stop('no valid GDAL/OGR found')
  } else {
    cat("GDAL version: ",getOption("gdalUtils_gdalPath")[[1]]$version)
    gdal <- getOption("gdalUtils_gdalPath")
  }
  
  # make the path available for System calls
  makGlobalVar("gdalPath", gdal[[1]]$path)
  
  # add to the beginning of the sessions PATH
  add2Path(gdal[[1]]$path)
  
  # return all gdalUtilSettings
  return(gdal)
  
}

