if (!isGeneric('initGRASS')) {
  setGeneric('initGRASS', function(x, ...)
    standardGeneric('initGRASS'))
}

#'The function initGRASS setup enviroment for using \link{rgrass7} with GRASS 
#'@description Function that initializes environment and pathes for GRASS7x. '
#'Despite the GRASS GIS seup is performed by the initGRASS() funtion of the
#'\link{rgrass7} package, there are some workarounds necessary. 
#'While initGRASS works fine for known pathes and environmental varibles, one 
#'will find that the integration of Windows based GRASS especially as provided 
#'by OSGeo4W or the usage of parallel installations could be cumbersome. 
#'initGRASS trys to find valid GRASS binaries by analyzing the initial GRASS script files.
#'If necessary it set the system variables and finally it initialize GRASS for R with user
#'provided  valid raster or sp object.\cr\cr 
#'*NOTE* If you have more than one valid installation you will be ask to select.
#'@details The concept is very straightforward but for an all days usage pretty helpful. 
#'You need to provide a \link{raster}/\link{sp} spatial object which is correct georeferenced
#'The resulting params will be used to initialize a temporary but static 
#'\href{https://CRAN.R-project.org/package=rgrass7}{rgrass7}
#'environment. During the rsession you will have full access to
#'GRASS7 via the \link{rgrass7} wrapper package. 
#'@param SP searchpath
#'@param x raster or sp object 
#'@param setDefaultGrass default = NULL will force a search for GRASS You
#'may provide a valid combination as c("C:\\OSGeo4W64","grass-7.0.5","osgeo4w") 
#'@author Chris Reudenbach 
#'@return initGRASS initializes the usage of GRASS7.
#'@export initGRASS 
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
#' initGRASS(meuse) 
#' 
#' # assuming a typical standalone installation 
#' initGRASS(meuse,c("C:\\Program Files\\GRASS GIS7.0.5","GRASS GIS 7.0.5","NSIS")) 
#' 
#' # assuming a typical OSGeo4W installation
#' initGRASS(meuse,c("C:\\OSGeo4W64","grass-7.0.5","osgeo4W"))
#' 
#' # string for Linux c("/usr/bin","grass72") '
#' }

initGRASS <- function(x = NULL,
                      setDefaultGrass = NULL, 
                      SP = NULL){
  if (is.null(x)) {
    stop("You MUST provide a raster* or sp* object, Did not found any of them so stopped.")
  } else {
    type <- getSimpleClass(x)
    if (type == "rst") {
      resolution <- raster::res(x)[1]
      proj4 <- as.character(x@crs)
      ymax <- x@extent@ymax
      ymin <- x@extent@ymin
      xmax <- x@extent@xmax
      xmin <- x@extent@xmin
    } else {
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
    }
  }
  if (Sys.info()["sysname"] == "Windows") {
    if (is.null(SP)) SP <- "C:"
    grass.gis.base <- getGrassParams4W(setDefaultGrass,SP)
  } else {
    if (is.null(SP)) SP <- "/usr"
    grass.gis.base <- getGrassParams4X(setDefaultGrass,SP)
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
  if (type == "rst") {
    rgrass7::execGRASS('g.region',
                       flags = c('quiet'),
                       n = as.character(ymax),
                       s = as.character(ymin),
                       e = as.character(xmax),
                       w = as.character(xmin),
                       res = as.character(resolution)
    )
  } else {
    rgrass7::execGRASS('g.region',
                       flags = c('quiet'),
                       n = as.character(ymax),
                       s = as.character(ymin),
                       e = as.character(xmax),
                       w = as.character(xmin)
                       #res=as.character(resolution)
    )
  }
  return(rgrass7::gmeta())
}

