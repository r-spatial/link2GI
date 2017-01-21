if (!isGeneric('linkGRASS7')) {
  setGeneric('linkGRASS7', function(x, ...)
    standardGeneric('linkGRASS7'))
}

#'@title The function initializes environment variables and pathes for GRASS7
#'@name linkGRASS7
#'@description Initializes the environment and the pathes for 'GRASS GIS 7.x' 
#' \url{https://grass.osgeo.org/}  The correct linkage to 'GRASS GIS' is performed by using an existing and valid
#'  \code{\link{raster}} or \code{\link{sp}} object. \cr
#'@note 'GRASS GIS 7' is excellently supported by the
#'  \link{rgrass7} wrapper package. Nevertheless 'GRASS GIS' is well known for
#'  its high demands regarding the correct workspace and environment setup. This
#'  becomes even worse on Windows platforms or if alternative 'GRASS GIS'
#'  installations are available. While the setup function
#'  \code{initGRASS} that is  provided by the \code{\link{rgrass7}} package, works fine under Linux and for known pathes and environmental variables, one
#'  will find that the integration of a Windows based 'GRASS GIS' especially if
#'  provided by 'OSGeo4W' \url{http://trac.osgeo.org/osgeo4w/} and/or the parallel
#'  installations of different software versions will be cumbersome. The function
#'  \code{linkGRASS7} tries to find all valid 'GRASS GIS' binaries by analyzing
#'  the startup files of 'GRASS GIS'. After identifying 'GRASS GIS' binaries all
#'  necessary system variables and settings will be performed.
#'@details The concept is very straightforward but for an all days usage pretty 
#'  helpful. You need to provide a \code{\link{raster}} or \code{\link{sp}} spatial object
#'  which has to be correctly georeferenced. The resulting params will be used
#'  to initialize a temporary but static 
#'  \href{https://CRAN.R-project.org/package=rgrass7}{rgrass7} environment. 
#'  During the rsession you will have full access to GRASS7 via the 
#'  \link{rgrass7} wrapper package. #'  *NOTE* If you have more than one valid
#'  installation you will be ask to select.
#'@param SP searchpath
#'@param x raster or sp object
#'@param setDefaultGrass default = NULL will force a search for GRASS You may 
#'  provide a valid combination as c("C:/OSGeo4W64","grass-7.0.5","osgeo4w")
#'@param verSelect boolean if TRUE you may choose interactively the binary version (if found  more than one),  by default FALSE
#'@author Chris Reudenbach
#'@return linkGRASS7 initializes the usage of GRASS7.
#'@export linkGRASS7
#'  
#'@examples 
#'\dontrun{
#'# get meuse data
#' library(sp)
#' data(meuse) 
#' coordinates(meuse) <- ~x+y 
#' proj4string(meuse) <-CRS("+init=epsg:28992") 
#' 
#' # automatic search and find of GRASS binaries if 
#' # more than one you have to choose. 
#' linkGRASS7(meuse)
#'  
#' # call if you do not have any idea if and where GRASS is installed
#' linkGRASS7(meuse)
#' 
#' # assuming a typical standalone installation 
#' linkGRASS7(meuse,c("C:/Program Files/GRASS GIS7.0.5","GRASS GIS 7.0.5","NSIS")) 
#' 
#' # assuming a typical OSGeo4W installation
#' linkGRASS7(meuse,c("C:/OSGeo4W64","grass-7.0.5","osgeo4W"))
#' 
#' # string for Linux c("/usr/bin","grass72") '
#' }

linkGRASS7 <- function(x = NULL,
                      setDefaultGrass = NULL, 
                      SP = NULL,
                      verSelect = FALSE){
  if (is.null(x)) {
    stop("You MUST provide a raster* or sp* object, Did not found any of them so stopped.")
  } else {
    if (getSpatialClass(x) == "rst") {
      resolution <- raster::res(x)[1]
      proj4 <- as.character(x@crs)
      ymax <- x@extent@ymax
      ymin <- x@extent@ymin
      xmax <- x@extent@xmax
      xmin <- x@extent@xmin
    } else if (getSpatialClass(x) == "rst") {
      # i do not understand all this class stuff :-(
      s <- x@proj4string
      s <- s@projargs
      s2 <- (strsplit(s,split = " "))
      proj4 <- paste(s2[[1]][2:length(unlist(s2))], collapse = ' ')
      xmax <- x@bbox[3]
      xmin <- x@bbox[1]
      ymax <- x@bbox[4]
      ymin <- x@bbox[2]
      #resolution<-0.0008333333
    } else {
      stop("Currently only raster* or sp* objects are supported - have to stop.")
    }
  }
  if (Sys.info()["sysname"] == "Windows") {
    if (is.null(SP)) SP <- "C:"
    grass.gis.base <- getGrassParams4W(setDefaultGrass,SP,verSelect)
  } else {
    if (is.null(SP)) SP <- "/usr"
    grass.gis.base <- getGrassParams4X(setDefaultGrass,SP,verSelect)
  }
  
  
  #Sys.setenv(.GRASS_CACHE = paste(Sys.getenv("HOME"), "\\.grass_cache",sep = "")) 
  #################### start with GRASS setup ------------------------------------
  # create the TEMPORARY GRASS location
  rgrass7::initGRASS(gisBase  = grass.gis.base,
                     home = tempdir(),
                     mapset = 'PERMANENT',
                     override = TRUE
  )
  
  # assign GRASS projection according to data set
  rgrass7::execGRASS('g.proj',
                     flags 
                     = c('c','quiet'),
                     proj4 = proj4
  )
  
  # assign GRASS extent
  if (getSpatialClass(x) == "rst") {
    rgrass7::execGRASS('g.region',
                       flags = c('quiet'),
                       n = as.character(ymax),
                       s = as.character(ymin),
                       e = as.character(xmax),
                       w = as.character(xmin),
                       res = as.character(resolution)
    )
  } else if (getSpatialClass(x) == "vec") {
    rgrass7::execGRASS('g.region',
                       flags = c('quiet'),
                       n = as.character(ymax),
                       s = as.character(ymin),
                       e = as.character(xmax),
                       w = as.character(xmin)
                       #res=as.character(resolution)
    )
  } else {
    stop("Currently only raster* or sp* objects are supported - have to stop.")
  }
  return(rgrass7::gmeta())
}

