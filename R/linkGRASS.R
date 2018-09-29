if (!isGeneric('linkGRASS7')) {
  setGeneric('linkGRASS7', function(x, ...)
    standardGeneric('linkGRASS7'))
}

#'@title 	Locate and set up 'GRASS 7' API bindings 
#'@name linkGRASS7
#'@description Initializes the session environment and the system pathes for an easy acces to 
#' \href{https://grass.osgeo.org/}{'GRASS GIS 7.x'}.  The correct setup of the spatial and projection parameters is
#'  automatically performed by using either an existing and valid \code{\link{raster}}, \code{\link{sp}} or \code{\link{sf}} object, 
#'  or manually by providing a list containing the minimum parameters needed.\cr
#'@note 'GRASS GIS 7' is excellently supported by the
#'  \link{rgrass7} wrapper package. Nevertheless 'GRASS GIS' is well known for
#'  its high demands regarding the correct spatial and reference setup an a bunch 
#'  of workspace and environment requirements. This becomes even worse on 'Windows' 
#'  platforms or if several alternative 'GRASS GIS' installations are available.
#'  If one knows what to do the \code{\link{rgrass7}} package setup function \code{initGRASS} works fine under Linux. 
#'  This is also valid for well known configurations under the 'Windows' operation system. 
#'  Nevertheless on university lab or on company computers with restriced privileges and/or using different releases
#'  like the  \href{http://trac.osgeo.org/osgeo4w/}{'OSGeo4W'} distribution and the  
#'  \href{https://grass.osgeo.org/download/software/ms-windows/#stand-alone}{'GRASS 7' stand-alone} installation, 
#'  or different software releases (e.g. 'GRASS 7.0.5 and GRASS 7.2.0), it becomes often cumbersome or even impossible to get the correct linkages. \cr 
#'  The function \code{linkGRASS7} tries to find all valid 'GRASS GIS' binaries by analyzing
#'  the startup script files of 'GRASS GIS'. After identifying the 'GRASS GIS' binaries all
#'  necessary system variables and settings will be generated and passed to a temporary R enviroment.
#'@details The concept is straightforward but for an all days usage helpful. Either you need to 
#' provide a \code{\link{raster}} or \code{\link{sp}} \code{\link{sf}} spatial object
#'  which has correct spatial and projection properties or you may link directlxy to an existing 'GRASS' gisdbase and mapset. 
#'  If you choose an spatial object to initialize a correct 'GRASS' mapset it is used to create either a temporary or a permanent 
#'  \href{https://CRAN.R-project.org/package=rgrass7}{rgrass7} environment including the correct 'GRASS 7' structure.\cr\cr
#'  The most time consuming part on 'Windows' Systems is the search process. This can easily take 10 or more minutes. 
#'  To speed up this process you can also provide a correct parameter set. Best way to do so is to call \code{searchGRASSW} or for 'Linux' \code{searchGRASSX} manually. 
#'  and call \code{linkGRASS7} with the version arguments of your choice. linkGRASS7 initializes the usage of GRASS7.
#'@note If you have more than one valid installation and run \code{linkGRASS7()} without arguments, you will be ask to select one.
#'@param search_path path or mounting point that will be searched
#'@param x raster or sp object
#'@param default_GRASS7 default is \code{NULL} If is \code{NULL} an automatic search for all installed versions is performed. 
#'                    If you provide a valid list the corresponding version is initialized. An example for OSGeo4W64 is: \code{c("C:/OSGeo4W64","grass-7.0.5","osgeo4w")}
#'@param gisdbase default is \code{NULL}, invoke \code{tempdir()} to the 'GRASS' database. Alternativeley you can provide a individual path.
#'@param location default is \code{NULL}, invoke \code{basename(tempfile())} for defining the 'GRASS' location. Alternativeley you can provide a individual path.
#'@param gisdbase_exist default is FALSE if set to TRUE the arguments gisdbase and location are expected to be an existing GRASS gisdbase
#'@param spatial_params default is \code{NULL}. Instead of a spatial object you may provide the geometry as a list. E.g. c(xmin,ymin,xmax,ymax,proj4_string)
#'@param resolution resolution in map units for the GRASS raster cells
#'@param ver_select boolean if TRUE you may choose interactively the binary version (if found  more than one),  by default FALSE
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@param returnPaths boolean if set to FALSE the pathes of the selected version are written 
#' to the PATH variable only, otherwise all paths and versions of the installed GRASS versions ae returned.


#'@author Chris Reudenbach

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
#' meuse_sf = st_as_sf(meuse, 
#'                     coords = 
#'                     c("x", "y"), 
#'                     crs = 28992, 
#'                     agr = "constant")
#' 
#'  
#' # Automatic search and find of GRASS binaries 
#' # using the meuse sp data object for spatial referencing
#' # This is the highly recommended linking procedure for on the fly jobs
#' # NOTE: if more than one GRASS installation is found you have to choose. 
#' linkGRASS7(meuse)
#' 
#' # find GRASS installations 
#' findGRASS()
#' 
#' # Typical osge4W installation (QGIS), using the meuse sp data object for spatial referencing
#' linkGRASS7(meuse,c("C:/Program Files/QGIS 2.18","grass-7.2.1","osgeo4W")) 
#' 
#' # Typical osgeo4W installation (rootdir), using the meuse sp data object for spatial referencing 
#' linkGRASS7(meuse,c("C:/OSGeo4W64/","grass-7.2.2","osgeo4W"))
#'
#' # CREATE and link to a permanent GRASS folder at "~/temp3", location named "project1" 
#' linkGRASS7(meuse_sf, gisdbase = "~/temp3",location = "project1")   
#' 
#' # ONLY LINK to a permanent GRASS folder at "~/temp3", location named "project1" 
#' linkGRASS7(gisdbase = "~/temp3",location = "project1", gisdbase_exist = TRUE )   
#' 
#' # SELECT the GRASS installation 
#' linkGRASS7(meuse_sf,ver_select = TRUE)
#' 
#' # SELECT the GRASS installation and define the search location
#' linkGRASS7(meuse_sf,ver_select = TRUE, search_path = "D:")
#'
#' # setting up GRASS manually with spatial parameters of the meuse data
#' proj4_string <- as.character(sp::CRS("+init=epsg:28992"))
#' linkGRASS7(spatial_params = c(178605,329714,181390,333611,proj4_string)) 
#' 
#' # creating a GRASS gisdbase manually with spatial parameters of the meuse data 
#' # additionally using a peramanent directory "~/examples" and the location "meuse_spatial_params "
#' proj4_string <- as.character(sp::CRS("+init=epsg:28992"))
#' linkGRASS7(gisdbase = paste0(tempdir(),"~/examples"),
#'            location = "meuse_spatial_params",
#'            spatial_params = c(178605,329714,181390,333611,proj4_string))
#'}

linkGRASS7 <- function(x = NULL,
                      default_GRASS7 = NULL, 
                      search_path = NULL,
                      ver_select = FALSE,
                      gisdbase_exist =FALSE,
                      gisdbase = NULL,
                      location = NULL,
                      spatial_params=NULL,
                      resolution=NULL,
                      quiet =TRUE,
                      returnPaths = FALSE) {
  # if no spatial object AND no extent AND no existing GRASS dbase is provided stop

  if (class(x)[1]=="character")   x <- raster::raster(x)
  # search for GRASS on your system
  if (Sys.info()["sysname"] == "Windows") {
    home <- Sys.getenv("USERPROFILE")
    if (is.null(search_path)) search_path <- "C:"
    grass <- paramGRASSw(default_GRASS7,search_path,ver_select)
  } else {
    home <- Sys.getenv("HOME")
    if (is.null(search_path)) search_path <- "/usr"
    grass <- paramGRASSx(default_GRASS7,search_path,ver_select)
  }
  # if an existing gdbase is provided link it  
  if (!is.null(location) & !is.null(gisdbase) & gisdbase_exist ) {
    rgrass7::initGRASS(gisBase  = grass$gisbase_GRASS,
                       home = tmpDir(),
                       gisDbase = path.expand(gisdbase),
                       mapset = "PERMANENT",
                       location = location,
                       override = TRUE
    ) 
   if(!quiet) return(rgrass7::gmeta())
  }
  
  ### if not do the normal linking procedure
  
  # create temporary location if not provided
  if (is.null(location)) {
    location <-  basename(tempfile())
  } else {
    location <- location  
  }
  # create temporary gsdbase if not provided
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
  
   if (!is.null(x) & is.null(spatial_params)) {
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
        if (!is.null(resolution)) resolution<- resolution
        else resolution <- "1"
      } else {
        s <- x@proj4string
        s <- s@projargs
        s2 <- (strsplit(s,split = " "))
        proj4 <- paste(s2[[1]][2:length(unlist(s2))], collapse = ' ')
        xmax <- x@bbox[3]
        xmin <- x@bbox[1]
        ymax <- x@bbox[4]
        ymin <- x@bbox[2]
        if (!is.null(resolution)) resolution<- resolution
        else resolution <- "1"
      }
    } 
  } else if  (!is.null(spatial_params)) {
    if (getSpatialClass(x) == "paramList") {
      proj4 <- spatial_params[5]
      xmax <- spatial_params[3]
      xmin <- spatial_params[1]
      ymax <- spatial_params[4]
      ymin <- spatial_params[2]
      if (!is.null(resolution)) resolution<- resolution
      else resolution <- "1"
    } 
  } else if  (is.null(x) & is.null(spatial_params)) {
    if (!quiet) cat("WARNING\n It is strongly recommended that you provide a raster*, sp* object or manually add the extent, resolution and projection information.\n These informations are obligatory to setup  the GRASS loccation...\n. Did not found any of them so lat WGS84 EPSG 4326 is assumed.\n")
    
    proj4 <- "+proj=longlat +datum=WGS84 +no_defs"
    xmax <- 180
    xmin <- -180
    ymax <- 90
    ymin <- -90
    if (!is.null(resolution)) resolution<- resolution
    else resolution <- "1"
    
  }
  
  
  
  
  #Sys.setenv(.GRASS_CACHE = paste(Sys.getenv("HOME"), "\\.grass_cache",sep = "")) 
  #################### start with GRASS setup ------------------------------------
  # create the TEMPORARY GRASS location
  rgrass7::initGRASS(gisBase  = grass$gisbase_GRASS,
                     home = tempdir(),
                     gisDbase = gisdbase,
                     mapset = "PERMANENT",
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
                       w = as.character(xmin),
                       res=as.character(resolution)
    )
  } else if (getSpatialClass(x) == "paramList") {
    rgrass7::execGRASS('g.region',
                       flags = c('quiet'),
                       n = as.character(ymax),
                       s = as.character(ymin),
                       e = as.character(xmax),
                       w = as.character(xmin),
                       res = as.character(resolution)
    )
  }
   else {
    stop("Currently only raster* or sp* objects are supported - have to stop.")
  }
  if(!quiet) print(rgrass7::gmeta())
  if (returnPaths) return(grass)
}

