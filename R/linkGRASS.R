if (!isGeneric('linkGRASS7')) {
  setGeneric('linkGRASS7', function(x, ...)
    standardGeneric('linkGRASS7'))
}

#'@title Initializes environment variables and pathes for GRASS7
#'@name linkGRASS7
#'@description Initializes the environment and the pathes for 
#' \href{https://grass.osgeo.org/}{'GRASS GIS 7.x'}  The correct linkage to 'GRASS GIS' is performed by using an existing and valid
#'  \code{\link{raster}} or \code{\link{sp}} object. \cr
#'@note 'GRASS GIS 7' is excellently supported by the
#'  \link{rgrass7} wrapper package. Nevertheless 'GRASS GIS' is well known for
#'  its high demands regarding the correct workspace and environment setup. This
#'  becomes even worse on 'Windows' platforms or if alternative 'GRASS GIS'
#'  installations are available. While the setup function
#'  \code{initGRASS} that is  provided by the \code{\link{rgrass7}} package, works fine under Linux and for known pathes and environmental variables, one
#'  will find that the integration of a 'Windows' based 'GRASS GIS' especially if
#'  provided by 'OSGeo4W' \url{http://trac.osgeo.org/osgeo4w/} and/or the parallel
#'  installations of different software versions will be cumbersome. The function
#'  \code{linkGRASS7} tries to find all valid 'GRASS GIS' binaries by analyzing
#'  the startup files of 'GRASS GIS'. After identifying 'GRASS GIS' binaries all
#'  necessary system variables and settings will be performed.
#'@details The concept is very straightforward but for an all days usage pretty 
#'  helpful. You need to provide a \code{\link{raster}} or \code{\link{sp}} spatial object
#'  which has to be correctly georeferenced. The resulting params will be used
#'  to initialize a temporary but static 
#'  \href{https://CRAN.R-project.org/package=rgrass7}{rgrass7} environment.\cr\cr
#'  If you want speed up the init process (mainly the search over your hard disk) you can provide a correct parameter set. Best way to do so is to call search 
#'@note If you have more than one valid installation you will be ask to select one.
#'@param searchPath path or mounting point that will be searched
#'@param x raster or sp object
#'@param defaultGrass if NULL an automatic search will be performed. You may 
#'  also provide a valid combination as c("C:/OSGeo4W64","grass-7.0.5","osgeo4w")
#'@param gisdbase 'GRASS' database directory de4fault is \code{tempdir()}
#'@param location the 'GRASS' location
#'@param mapset the 'GRASS' mapset
#'@param verSelect boolean if TRUE you may choose interactively the binary version (if found  more than one),  by default FALSE
#'@author Chris Reudenbach
#'@return linkGRASS7 initializes the usage of GRASS7.
#'@export linkGRASS7
#'  
#'@examples 
#'\dontrun{
#'# get meuse data as sp object
#' library(link2GI)
#' require(sp)
#' data(meuse) 
#' coordinates(meuse) <- ~x+y 
#' proj4string(meuse) <-CRS("+init=epsg:28992") 
#' 
#'# get meuse data as sf object
#' require(sf)
#' meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
#' 
#' # automatic search and find of GRASS binaries if 
#' # more than one you have to choose. 
#' linkGRASS7(meuse)
#'  
#' # if you do not have any idea if and where GRASS is installed
#' # Actually this type of linking procedure is highly recommended
#' linkGRASS7(meuse)
#' 
#' # assuming a typical standalone non-OSGeo4W installation
#' linkGRASS7(meuse,c("C:/Program Files/GRASS GIS7.0.5","GRASS GIS 7.0.5","NSIS")) 
#' 
#' # assuming a typical OSGeo4W installation
#' linkGRASS7(meuse,c("C:/OSGeo4W64","grass-7.0.5","osgeo4W"))
#'
#' # assuming a permanent GRASS folder at "~/temp3" location named "project1" 
#' # and using an sf object for initialisation
#' linkGRASS7(meuse_sf,gisdbase = "~/temp3",location = "project1")   
#' 
#' # choose manually the GRASS installation 
#' linkGRASS7(meuse_sf,verSelect = TRUE)
#' 
#' # choose manually the GRASS installation and change the search location
#' linkGRASS7(meuse_sf,verSelect = TRUE, searchPath = "D:/")
#' 
#' }

linkGRASS7 <- function(x = NULL,
                      defaultGrass = NULL, 
                      searchPath = NULL,
                      verSelect = FALSE,
                      gisdbase = NULL,
                      mapset="PERMANENT",
                      location = NULL) {
  
  if (is.null(location)) location <-  basename(tempfile())
  if (is.null(gisdbase)) {
    gisdbase <-  tempdir()
  } else { 
    gisdbase <- path.expand(gisdbase)
  }
  
  if (!file.exists(file.path(gisdbase))) {
    dir.create(file.path(gisdbase),recursive = TRUE)
    #cat("the path ",gisdbase," is not found. Please provide an existing and valid path to your gisdbase folder\n ")
  }
  if (!file.exists(file.path(gisdbase,location))) {
    dir.create(file.path(gisdbase,location),recursive = TRUE)
  }

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
    } else if (getSpatialClass(x) == "vec") {
      # i do not understand all this class stuff :-(
      if (class(x)[1] == "sf" ) {
        corner <- sf::st_bbox(x) 
        xmax <- corner[3]
        xmin <- corner[1]
        ymax <- corner[4]
        ymin <- corner[2]
        proj4 <-  unlist(sf::st_crs(x)[2])
      } else {
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
    } else {
      stop("Currently only raster* or sp* objects are supported - have to stop.")
    }
  }
  if (Sys.info()["sysname"] == "Windows") {
      home <- Sys.getenv("USERPROFILE")
    if (is.null(searchPath)) searchPath <- "C:"
    grass.gis.base <- getGrassParams4W(defaultGrass,searchPath,verSelect)
  } else {
    home <- Sys.getenv("HOME")
    if (is.null(searchPath)) searchPath <- "/usr"
    grass.gis.base <- getGrassParams4X(defaultGrass,searchPath,verSelect)
  }
  
  
  #Sys.setenv(.GRASS_CACHE = paste(Sys.getenv("HOME"), "\\.grass_cache",sep = "")) 
  #################### start with GRASS setup ------------------------------------
  # create the TEMPORARY GRASS location
  rgrass7::initGRASS(gisBase  = grass.gis.base,
                     home = tmpDir(),
                     gisDbase = gisdbase,
                     mapset = mapset,
                     location = location,
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
                       flags = c('quiet','d'),
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

