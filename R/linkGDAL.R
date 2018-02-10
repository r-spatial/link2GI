if (!isGeneric('linkGDAL')) {
  setGeneric('linkGDAL', function(x, ...)
    standardGeneric('linkGDAL'))
}

#'Checks and sets up via \code{gdalUtils}  the 'GDAL' pathes and settings for the command line usage.
#'@name linkGDAL
#'@description For almost all GI related software tools you need to have installed the  \href{http://www.gdal.org/}{'GDAL'}  binaries. It is useful for checking and linking the 'GDAL' binary 
#'installation to the 'R'environment for command line calls of GDAL functions.  \code{linkGDAL} checks
#' via \code{gdalUtils} the status of the 'GDAL' binaries installation. 
#' 
#'@param quiet boolean if set to FALSE you will get most of the console messages
#'@param returnPathes boolean if set to FALSE the GDAL binary path is written 
#' to the PATH variable only, otherwise all paths and names of the installed "GDAL" ae returned.
#'@author Chris Reudenbach
#'
#'@export linkGDAL 
#'  
#'@examples 
#'\dontrun{
#'
#' # get all available GDAL informations 
#' gdal<-linkGDAL()
#' 
#' # get available GDAL driver
#' gdal[[1]]$drivers$format_code
#' 
#' # get the binary path (is also written to the PATH variable)
#' gdal[[1]]$path
#' 
#' # get the available python tools e.g. for gdal_sieve.py
#' # you may call it like following:
#' #  ret <- system(paste0("gdal_sieve.py -8 ",
#' #                         "input.sdat ",
#' #                         "output.sdat ",
#' #                         "-of SAGA"), intern = TRUE)

#' gdal[[1]]$python_utilities
#' 
#' }

linkGDAL <- function(quiet = TRUE,
                     returnPathes = TRUE){
  if (substr(Sys.getenv("COMPUTERNAME"),1,5) == "PCRZP") {
    gdalUtils::gdal_setInstallation(search_path = shQuote("C:/Program Files/QGIS 2.14/bin/"))
  } else {
  ## (gdalUtils) check for a valid GDAL binary installation on your system
    if (!quiet) gdalUtils::gdal_setInstallation(verbose = TRUE)
    else gdalUtils::gdal_setInstallation()
  }
  valid.install <- !is.null(getOption("gdalUtils_gdalPath"))
  if (!valid.install) {
    stop('no valid GDAL/OGR found')
  } else {
    
    gdal <- getOption("gdalUtils_gdalPath")
  }
  
  
  # add to the beginning of the sessions PATH
  add2Path(gdal[[1]]$path)
  
  # return all gdalUtilSettings
  if (returnPathes) return(gdal)
  
}

