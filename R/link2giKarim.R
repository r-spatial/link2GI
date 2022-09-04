
#'@title Checks if x is of type raster,terra,sf or sp
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
                           "SpatialGridDataFrame","stars",
                           "SpatialPixelsDataFrame",
                           "SpatRaster")) {"rst"} 
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
#' add2Path("pathtosomewhere")
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
#' @import roxygen2
#' @import devtools
#' @importFrom utils install.packages
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
  
  utils::install.packages(pkg, repos = NULL)
  
  return(invisible(NULL))
}

if ( !isGeneric("sf2gvec") ) {
  setGeneric("sf2gvec", function(x, ...)
    standardGeneric("sf2gvec"))
}

#' Write sf object directly to GRASS 7/8 vector utilising an existing or creating a new GRASS environment
#' @param x  \code{sf} object corresponding to the settings of the corresponding GRASS container
#' @param obj_name name of GRASS layer
#' @param gisdbase  GRASS gisDbase folder
#' @param location  GRASS location name containing \code{obj_name)}
#' @param gisdbase_exist logical switch if the GRASS gisdbase folder exist default is TRUE
#' @author Chris Reudenbach
#' @note  have a look at the \code{sf} capabilities to write direct to sqlite
#' @export sf2gvec
#' @importFrom sf st_as_sf
#' @importFrom sf st_write
#' @importFrom sf st_read 

#' @examples 
#'\dontrun{
#' ## example 
#' # get meuse data as sf object
#' require(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#'     
#' # write data to GRASS and create gisdbase
#' sf2gvec(x = nc,
#'           obj_name = "nc_R-G",
#'           gisdbase = "~/temp3",
#'           location = "project1")
#'  
#' # read from existing GRASS          
#' gvec2sf(x = nc_R-G,
#'           obj_name = "nc_R-G",
#'           gisdbase = "~/temp3",
#'           location = "project1")
#' }

sf2gvec <- function(x, epsg, obj_name, gisdbase, location , gisdbase_exist=FALSE){
  
  if (gisdbase_exist)
    linkGRASS(gisdbase = gisdbase, location = location, gisdbase_exist = TRUE)  
  else 
    linkGRASS(x = x, gisdbase = gisdbase, location = location)  
  path <- Sys.getenv("GISDBASE")
  sq_name <- gsub(tolower(paste0(obj_name,".sqlite")),pattern = "\\-",replacement = "_")
  if (!inherits(x, "sf")) sf::st_as_sf(x,x_sf)
  else
    x_sf <- x 
  #if (!file.exists(file.path(path,sq_name)))
  sf::st_write(x_sf,file.path(path,sq_name),append = FALSE)
  
  epsg = sf::st_crs(x)$epsg
  
  ret <- try(rgrass::execGRASS('v.import',  
                                flags  = c("overwrite", "o"),
                                #extent = "region",
                                input  = file.path(path,sq_name),
                               epsg = as.numeric(epsg),
                                
                                
                                output = gsub(tolower(sq_name),
                                              pattern = "\\.",
                                              replacement = ""),
                               ignore.stderr = TRUE,
                                intern = TRUE),silent = FALSE)
  
  if (methods::is(ret, "try-error"))  return(cat("Data not found"))
}

if ( !isGeneric("gvec2sf") ) {
  setGeneric("gvec2sf", function(x, ...)
    standardGeneric("gvec2sf"))
}

#' Converts from an existing GRASS 7/8 environment an arbitrary vector dataset  into a  sf object
#' @param x  \code{\link{sf}} object corresponding to the settings of the corresponding GRASS container
#' @param obj_name name of GRASS layer
#' @param gisdbase  GRASS gisDbase folder
#' @param location  GRASS location name containing \code{obj_name)}
#' @param gisdbase_exist logical switch if the GRASS gisdbase folder exist default is TRUE
#' @author Chris Reudenbach
#' @note  have a look at the \code{\link{sf}} capabilities to read direct from sqlite
#' @export gvec2sf
#' @examples 
#'\dontrun{
#' ## example 
#' # get meuse data as sf object
#' require(sf)
#' meuse_sf = st_as_sf(meuse, 
#'                    coords = c("x", "y"), 
#'                    crs = 28992, 
#'                    agr = "constant")
#'     
#' 
#' # write data to GRASS and create gisdbase
#' sf2gvec(x = meuse_sf,
#'           obj_name = "meuse_R-G",
#'           gisdbase = "~/temp3",
#'           location = "project1")
#'  
#' # read from existing GRASS          
#' gvec2sf(x = meuse_sf,
#'           obj_name = "meuse_R-G",
#'           gisdbase = "~/temp3",
#'           location = "project1")
#' }

gvec2sf <- function(x, obj_name, gisdbase, location ,gisdbase_exist = TRUE){
  
  if (gisdbase_exist)
    linkGRASS(gisdbase = gisdbase, location = location, gisdbase_exist = TRUE)  
  else 
    linkGRASS(x, gisdbase = gisdbase, location = location)  
  path <- Sys.getenv("GISDBASE")
  sq_name <- gsub(tolower(paste0(obj_name,".sqlite")),pattern = "\\-",replacement = "_")
  
  
  ret <- try(rgrass::execGRASS('v.out.ogr',  
                                flags = c("overwrite","quiet"),
                                input = gsub(tolower(sq_name),pattern = "\\.",replacement = ""),
                                output = file.path(path,paste0(obj_name,"_new.sqlite")),
                                format = "SQLite",
                                ignore.stderr = TRUE,
                                intern = TRUE),silent = TRUE)
 
  if(!methods::is(ret, "try-error")) 
    return(sf::st_read(file.path(path,paste0(obj_name,"_new.sqlite")),quiet = TRUE))
  else 
    return(cat("Data not found"))
}

#'  convenient function to establish all link2GI links
#' @description brute force search, find and linkl of all link2GI link functions. This is helpfull if yor system is well setup and the standard linkage procedure will provide the correct linkages. 
#'
#' @note You may also use the full list of arguments that is made available from the \code{link2GI} package, but it is strongly recommended in this case to use directly the single linkage functions from  \code{link2GI}.
#' @param links character. links
#' @param linkItems character. list of c("saga","grass7","otb","gdal")
#' @param simple logical. true  make all
#' @param sagaArgs character. full string of sagaArgs
#' @param grassArgs character. grassArgs full string of grassArgs
#' @param otbArgs character. full string of otbArgs
#' @param gdalArgs character. full string of gdalArgs
#' @param quiet supress all messages default is FALSE
#'
#'@examples
#'\dontrun{
#' # required packages
#' require(uavRst)
#' require(link2GI)
#'
#' # search, find and create the links to all supported  GI software
#' giLinks<-uavRst::linkAll()
#' 
#' # makes the GDAL linkage verbose
#' giLinks<-uavRst::linkAll(gdalArgs= "quiet = TRUE") 
#'
#'}

#' @export
linkAll <- function(links=NULL,
                    simple = TRUE,
                    linkItems = c("saga","grass7","otb","gdal"),
                    sagaArgs = "default",
                    grassArgs = "default",
                    otbArgs =   "default",
                    gdalArgs =  "default",
                    quiet = FALSE)  {
  
  
  if (!quiet )    cat("\n--- linking SAGA - GRASS - OTB - GDAL ---\n")
  if (sagaArgs == "default") sagaArgs   <- "default_SAGA = NULL, searchLocation = 'default', ver_select=FALSE, quiet = TRUE, returnPaths = TRUE"
  if (grassArgs == "default") grassArgs <- "x = NULL, default_GRASS = NULL, search_path = NULL, ver_select = FALSE, gisdbase_exist =FALSE, gisdbase = NULL, use_home =FALSE, location = NULL, spatial_params=NULL, resolution=NULL, quiet =TRUE, returnPaths = TRUE"
  if (otbArgs == "default") otbArgs <- "bin_OTB=NULL, root_OTB= NULL, type_OTB=NULL, searchLocation=NULL, ver_select=FALSE, quiet = TRUE, returnPaths = TRUE"
  if (gdalArgs == "default") gdalArgs <- "bin_GDAL=NULL, searchLocation=NULL, ver_select=FALSE, quiet = TRUE, returnPaths = TRUE"
  if (is.null(links) && (simple)){
    link<-list()
    for (links in linkItems) {
      cat("linking ", links,"\n")
      if (links=="gdal") 
        link[[links]]<-assign(links,eval(parse(text=paste("link2GI::link",toupper(links),"(returnPaths = T)",sep = ""))))
      else
        link[[links]]<-assign(links,eval(parse(text=paste("link2GI::link",toupper(links),"(returnPaths = T)",sep = ""))))
      
    }
    
  } else if (is.null(links)) {
    link<-list()
    for (links in linkItems) {
      link[[links]]<-assign(links,eval(parse(text=paste("link2GI::link",toupper(links),"(",eval(parse(text=paste0(links,"Args"))),")",sep = ""))))
    }
    
  }
  return(link)
}