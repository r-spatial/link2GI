
#'@title Checks if x is of type raster,sf or sp
#'@name getSpatialClass
#'@description  Checks if x is a raster or sp object
#'@param obj R raster* or sp object
#'@author Chris Reudenbach
#'@keywords internal
#'@examples
#' \dontrun{
#' # add path
#' getSpatialClass(x)
#' }

getSpatialClass <- function(obj) {
  if (class(obj)[1] %in% c("RasterLayer", "RasterStack",
                           "RasterBrick", "Satellite",
                           "SpatialGridDataFrame",
                           "SpatialPixelsDataFrame")) {"rst"} 
  else if (class(obj)[1] %in% c("SpatialPointsDataFrame", "SpatialPoints",
                                "SpatialPolygonsDataFrame",
                                "SpatialPolygons",
                                "SpatialLinesDataFrame",
                                "SpatialLines",
                                "sf")) {"vec"}
  else {"paramList"}
}

#'@title Checks if running on a specified computer domain
#'@name checkPCDomain
#'@description  Checks if the computer belongs to the Marburg Universitys computer pools
#'@param cliCode code of the sofware currently \code{saga} and \code{otb} are supported
#'@param prefixPC contains the an arbitrary part of the computer name. It always starts with the first letter.
#'@author CR
#'@keywords internal
#'@examples
#' \dontrun{
#' # add path
#' checkPCDomain("saga",prefixPC="PCRZP")
#' }
#'@export checkPCDomain
checkPCDomain <- function(cliCode=NULL, prefixPC="PCRZP") {
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
  if (substr(Sys.getenv("COMPUTERNAME"),1,nchar(prefixPC)) == substr(prefixPC,1,nchar(prefixPC))) {
    if (cliCode == "saga") { 
      defaultSAGA <- shQuote(c("C:\\Program Files\\QGIS 2.14\\apps\\saga","C:\\Program Files\\QGIS 2.14\\apps\\saga\\modules"))
      return(defaultSAGA)
    } else {
      return(defaultSAGA = NULL)  
    }
  } else if (cliCode == "otb") {
    defaultOtb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    root_OTB <- shQuote("C:\\Program Files\\QGIS 2.14")
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = GiEnv)
    otbInstallations <- data.frame(instDir = shQuote("C:\\Program Files\\QGIS 2.14\\bin"), installation_type = "osgeo4wOTB", stringsAsFactors = FALSE)
    return(otbInstallations)
  }
  
  
}

#'@title Adds a defined variable and value to the global search path
#'@name add2Path
#'@description  Adds a variable to the global search path of the current environment
#'@param newPath the path that is added
#'@author Chris Reudenbach
#'@keywords internal
#'@examples
#' \dontrun{
#' # add path
#' addPath("pathtosomewhere")
#' }
#'@export add2Path
#'
add2Path <- function(newPath) {
  exist <- FALSE
  if (Sys.info()["sysname"] == "Windows") {
    del <- ";"  
  } else {
    del <- ":"  
  } 
  p <- Sys.getenv("PATH")
  if (substr(p, 1,nchar(newPath)) == newPath) {
    exist <- TRUE
  }
  # if not exist append path to systempath
  if (!exist) {
    Sys.setenv(PATH = paste0(newPath,del,Sys.getenv("PATH")))
  }
}

#'@title Generates a variable with a certain value in the R environment
#'@name makGlobalVar
#' @description  Generates a variable with a certain value in the R environment
#' @param name character string name of the variable
#' @param value character string value of the variable
#' @keywords internal
#'@export makGlobalVar 
#'@examples
#' \dontrun{
#'
#' # creates the global var \code{pathToData} with the value \code{~/home/data}
#' makGlobalVar("pathToData","~/home/data") 
#' 
#' }
#' 
makGlobalVar <- function(name,value) {
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv())  
  if (exists(name, envir = GiEnv)) {
    #warning(paste0("The variable '", name,"' already exist in .GlobalEnv"))
    assign(name, value, envir = GiEnv, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global GiEnv\n")
  } else {
    assign(name, value, envir = GiEnv, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global GiEnv\n")
  } 
}


readinteger <- function()
{ 
  
  n <- readline()
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}
