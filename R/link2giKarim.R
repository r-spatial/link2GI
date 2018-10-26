
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

#' Build package manually
#' 
#' @description 
#' This function was specifically designed to build a package from local source 
#' files manually, i.e., without using the package building functionality 
#' offered e.g. by RStudio. 
#' @details NOTE the default setting are focussing HRZ environment at Marburg University
#' 
#' 
#' @param dsn 'character'. Target folder containing source files; defaults to 
#' the current working directory.
#' @param pkgDir 'character'. Target folder containing the result ing package of the invoked build process. According to Marburg University pools the default is set to pkgDir="H:/Dokumente". If you want to use it in a different setting you may set pkgDir to whatever you want.
#' @param document 'logical'. Determines whether or not to invoke 
#' \code{\link{roxygenize}} with default roclets for documentation purposes.  
#' @param ... Further arguments passed on to \code{\link[devtools]{build}}. 
#' 
#' @seealso 
#' \code{\link{roxygenize}}, \code{\link[devtools]{build}},\code{\link{install.packages}}.
#' 
#' @author 
#' Florian Detsch, Chris Reudenbach
#' 
#' @keywords internal
#' @examples
#' \dontrun{
#' ## when in a package directory, e.g. '~/link2GI' 
#' manuallyBuild()
#' }
#' 
#'

manuallyBuild <- function(dsn = getwd(), pkgDir="H:/Dokumente",document = TRUE, ...) {
  
  ## reset 'dsn' to 'H:/...'  
  if (length(grep("students_smb", dsn)) > 0) {
    lst_dsn <- strsplit(dsn, "/")
    chr_dsn <- unlist(lst_dsn)[3:5]
    dsn <- paste0("H:/", paste(chr_dsn, collapse = "/"))
  }
  
  ## if 'document = TRUE', create documentation 
  if (document) {
    cat("\nCreating package documentation...\n")
    roxygen2::roxygenize(package.dir = dsn, 
                         roclets = c('rd', 'collate', 'namespace'))
  }
  
  ## build package
  cat("\nBuilding package...\n")
  
  devtools::build(pkg = dsn, path = dirname(dsn), ...)
  
  
  ## install package
  cat("Installing package...\n")
  pkg <- list.files(dirname(pkgDir), full.names = TRUE,
                    pattern = paste0(basename(dsn), ".*.tar.gz$"))
  pkg <- pkg[length(pkg)]
  
  install.packages(pkg, repos = NULL)
  
  return(invisible(NULL))
}
