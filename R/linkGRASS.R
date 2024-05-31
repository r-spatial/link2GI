if (!isGeneric('linkGRASS')) {
  setGeneric('linkGRASS', function(x, ...)
    standardGeneric('linkGRASS'))
}

#'@title 	Locate and set up 'GRASS' API bindings 
#'@name linkGRASS
#'@description Initializes the session environment and the system paths for an easy access to
#' \href{https://grass.osgeo.org/}{'GRASS GIS 7.x/8.x'}.  The correct setup of the spatial and
#'  projection parameters is automatically performed by using either an existing and valid 
#'  \code{raster}, \code{terra}, \code{sp} or \code{sf} object, 
#'  or manually by providing a list containing the minimum parameters needed.\cr
#'@note GRASS GIS is excellently supported by the \code{rgrass} wrapper package. Nevertheless, 'GRASS GIS' is known for its
#' its high demands on the correct spatial and reference setup  and environment requirements. This becomes even worse on 'Windows 
#' platforms or when there are several alternative 'GRASS GIS' installations available.
#' If you know how to use the \code{rgrass} package setup function \code{rgrass::initGRASS} works fine on Linux. 
#' This is also true for known configurations under the 'Windows' operating system. 
#' However, on university labs or corporate machines with limited privileges and/or different releases
#' such as the \href{https://trac.osgeo.org/osgeo4w/}{'OSGeo4W'} distribution and the  \href{https://grass.osgeo.org/download/windows/}{'GRASS' stand-alone} installation, 
#' or different software releases (e.g. 'GRASS 7.0.5 and GRASS 8.1.0), it often becomes inconvenient or even 
#' to get the correct links. \cr The function \code{linkGRASS} tries to find all valid 'GRASS GIS' binaries by #' analyzing the startup script files.
#' GRASS GIS' startup script files. After identifying the 'GRASS GIS' binaries, all #' necessary system variables and settings are
#' system variables and settings are generated and passed to a temporary R environment.
#' The concept is simple, but helpful for everyday use. You need to either 
#' provide a \code{raster} or \code{sp} \code{sf} spatial object
#' that has the correct spatial and projection properties, or you can link directly to an existing 'GRASS' gisdbase and mapset. 
#' If you choose a spatial object to initialize a correct 'GRASS' mapset, it will be used to create either a temporary or permanent mapset. 
#' \href{https://CRAN.R-project.org/package=rgrass}{rgrass} environment with the correct 'GRASS' structure.\cr\cr
#' The most time consuming part on Windows systems is the search process. This can easily take 10 minutes or more. 
#' To speed up this process, you can also provide a correct parameter set. The best way to do this is to manually call \code{searchGRASSW} or for 'Linux' \code{searchGRASSX}. 
#' and call \code{linkGRASS} with the version arguments of your choice. linkGRASS will initialize the use of GRASS.
#' If you have more than one valid installation and call \code{linkGRASS()} without arguments, you will be asked to select one.
#'@param search_path Path or mount point to search for.
#'@param x raster/terra or sf/sp object
#'@param default_GRASS default is \code{NULL} If is \code{NULL} an automatic search for all installed versions is performed. 
#'                    If you provide a valid list the corresponding version is initialized. An example for OSGeo4W64 is:
#'                    \code{c("C:/OSGeo4W64","grass-7.0.5","osgeo4w")}
#'@param gisdbase default is \code{NULL}, invoke \code{tempdir()} to the 'GRASS' database. Alternatively you can provide a individual path.
#'@param location default is \code{NULL}, invoke \code{basename(tempfile())} for defining the 'GRASS' location. Alternatively you can provide a individual path.
#'@param use_home default is \code{FALSE}, set the GISRC path to tempdir(), if TRUE the HOME or USERPROFILE setting is used for writing the GISRC file
#'@param gisdbase_exist default is FALSE if set to TRUE the arguments gisdbase and location are expected to be an existing GRASS gisdbase
#'@param spatial_params default is \code{NULL}. Instead of a spatial object you may provide the geometry as a list. E.g. c(xmin,ymin,xmax,ymax,proj4_string)
#'@param resolution resolution in map units for the GRASS raster cells
#'@param epsg manual epsg override
#'@param ver_select Boolean if TRUE you may choose interactively the binary version (if found  more than one),  by default FALSE
#'@param quiet Boolean  switch for suppressing console messages default is TRUE
#'@param returnPaths Boolean if set to FALSE the pathes of the selected version are written 
#' to the PATH variable only, otherwise all paths and versions of the installed GRASS versions ae returned.

#'@author Chris Reudenbach

#'@export linkGRASS
#'@aliases linkGRASS7 
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs 
#' @importFrom sf st_bbox
#' @import methods
#' 
#' @examples 
#' 
#' run = FALSE
#' if (run) {
#' library(link2GI)
#' require(sf)
#' 
#' # get  data                         
#' nc = st_read(system.file("shape/nc.shp", package="sf"))

#' # Automatic linking of GRASS binaries using the nc data object for spatial referencing
#' # This is the best practice linking procedure for on-the-fly jobs.
#' # NOTE: If more than one GRASS installation is found, you will have to select one. 
#' grass = linkGRASS(nc)
#' 
#' # Select the GRASS installation (if more than one)
#' linkGRASS(nc, ver_select = TRUE)
#' 
#' # Select the GRASS installation and define the search location
#' linkGRASS(nc, ver_select = TRUE, search_path = "~/")
#' 
#' # Set up GRASS manually with spatial parameters of the nc data
#' epsg = 4267
#' proj4_string = sp::CRS(paste0("+init=epsg:",epsg))
#' linkGRASS(spatial_params = c(178605,329714,181390,333611,proj4_string)) 
#' 
#' # create some temporary project folders for a permanent gisdbase
#' root_folder = tempdir()
#' grass_path = link2GI::createFolder(root_folder = root_folder,
#'                          folders = c("project1/"))
#' if (grass$exist){
#' # CREATE and link to a permanent GRASS folder at "root_folder", location named "project1" 
#' linkGRASS(nc, gisdbase = root_folder, location = "project1")   
#' 
#' # ONLY LINK to a permanent GRASS folder in "root_folder", location named "project1" 
#' linkGRASS(gisdbase = root_folder, location = "project1", gisdbase_exist = TRUE )   
#' 
#' # Manual creation of a GRASS gisdbase with the spatial parameters of the NC data. 
#' # additional use of a permanent directory "root_folder" and the location "nc_spatial_params".
#' epsg = 4267
#' proj4_string = sp::CRS(paste0("+init=epsg:",epsg))
#' linkGRASS(gisdbase = root_folder,
#'            location = "nc_spatial_params",
#'            spatial_params = c(-84.32385, 33.88199,-75.45698,36.58965,proj4_string),epsg = epsg)
#' }
#' }
#' 

linkGRASS = function(x = NULL,
                       epsg = NULL,
                       default_GRASS=NULL, 
                       search_path=NULL,
                       ver_select=FALSE,
                       gisdbase_exist=FALSE,
                       gisdbase=NULL,
                       use_home=FALSE,
                       location=NULL,
                       spatial_params=NULL,
                       resolution=NULL,
                       quiet=TRUE,
                       returnPaths=TRUE) {
  # if no spatial object AND no extent AND no existing GRASS dbase is provided stop
  if (!use_home) home = tempdir()
  if  (is.null(epsg)) {
    crs_info = sf::st_crs(x)
    # Extract the EPSG code
    epsg = crs_info$epsg
  }
  #epsg = try( st_crs(x) )
  if (is.na(epsg)) epsg = 4326
  if (class(x)[1]=="character")   {
    x = terra::rast(x)
    terra::crs(x)  = sf::st_crs(as.numeric(epsg))$wkt

    }
  else if (class(x)[1]=="SpatRaster") {
    x = terra::rast(x)
    terra::crs(x)  = sf::st_crs(as.numeric(epsg))$wkt
    
  }
  else if (class(x)[1] %in% c("RasterLayer", "RasterStack",
                                "RasterBrick", "Satellite",
                                "SpatialGridDataFrame",
                                "SpatialPixelsDataFrame")){
    x = terra::rast(x)
    terra::crs(x)  = sf::st_crs(as.numeric(epsg))$wkt

  }
  else if (class(x)[1]=="stars") {
    epsg = attributes(x)$dimensions[[1]]$refsys$epsg
    #stars::write_stars(x,"o.tif",overwrite=TRUE)
    
  }
  
  # search for GRASS on your system
  if (Sys.info()["sysname"] == "Windows") {
   if (use_home) home = Sys.getenv("USERPROFILE")
    if (is.null(search_path)) search_path = "C:/"
    grass = paramGRASSw(default_GRASS,search_path,ver_select)
  } else {
    if (use_home) home = Sys.getenv("HOME")
    if (is.null(search_path)) search_path = "/usr/bin"
    grass = paramGRASSx(default_GRASS,search_path,ver_select)
  }
  if (grass[[1]][1] != FALSE) {
    # if an existing gdbase is provided link it  
    if (!is.null(location) & !is.null(gisdbase) & gisdbase_exist ) {
      rgrass::initGRASS(gisBase = grass$gisbase_GRASS,
                         home = home,
                         gisDbase = path.expand(gisdbase),
                         mapset = "PERMANENT",
                         location = location,
                         override = TRUE
      ) 
      grass$exist = TRUE
    }
    
    ### if not do the temp linking procedure
    
    # create temporary location if not provided
    else {
      if (is.null(location)) {
      location =  basename(tempfile())
    } else {
      location = location  
    }
    # create temporary gsdbase if not provided
    if (is.null(gisdbase)) {
      gisdbase =  tempdir()
    } else { 
      gisdbase = path.expand(gisdbase)
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
        x=terra::rast(x)
        resolution = terra::res(x)[1]
        proj4 = as.character(terra::crs(x))
        ymax = terra::ext(x)[4]
        ymin = terra::ext(x)[3]
        xmax = terra::ext(x)[2]
        xmin = terra::ext(x)[1]
      } else if (getSpatialClass(x) == "vec") {
        # i do not understand all this class stuff :-(
        if (class(x)[1] == "sf" ) {
          corner = sf::st_bbox(x) 
          xmax = corner[3]
          xmin = corner[1]
          ymax = corner[4]
          ymin = corner[2]
          proj4 =  sf::st_crs(x)$proj4string
          if (!is.null(resolution)) resolution = resolution
          else resolution = "1"
        } else {
          s = x@proj4string
          s = s@projargs
          s2 = (strsplit(s,split = " "))
          proj4 = paste(s2[[1]][2:length(unlist(s2))], collapse = ' ')
          xmax = x@bbox[3]
          xmin = x@bbox[1]
          ymax = x@bbox[4]
          ymin = x@bbox[2]
          if (!is.null(resolution)) resolution = resolution
          else resolution = "1"
        }
      } 
    } else if  (!is.null(spatial_params)) {
      if (getSpatialClass(x) == "paramList") {
        proj4 = spatial_params[5]
        xmax = spatial_params[3]
        xmin = spatial_params[1]
        ymax = spatial_params[4]
        ymin = spatial_params[2]
        if (!is.null(resolution)) resolution = resolution
        else resolution = "1"
      } 
    } else if  (is.null(x) & is.null(spatial_params) &   !gisdbase_exist ) {
      if (!quiet) cat("WARNING\n It is strongly recommended that you provide a raster/terra*, sf/sp* object or manually add the extent, resolution and projection information.\n These informations are obligatory to setup  the GRASS loccation...\n. Did not found any of them so lat WGS84 EPSG 4326 is assumed.\n")
      
      proj4 = "+proj=longlat +datum=WGS84 +no_defs"
      xmax = 180
      xmin = -180
      ymax = 90
      ymin = -90
      epsg=4326
      if (!is.null(resolution)) resolution = resolution
      else resolution = "1"
    }
    
    
    
    
    
    #Sys.setenv(.GRASS_CACHE = paste(Sys.getenv("HOME"), "\\.grass_cache",sep = "")) 
    #################### start with GRASS setup ------------------------------------
    # create the TEMPORARY GRASS location
    returnPaths = TRUE
    rgrass::initGRASS(gisBase = grass$gisbase_GRASS,
                       home = home,
                       gisDbase = gisdbase,
                       mapset = "PERMANENT",
                       location = location,
                       override = TRUE
    )
    
    # assign GRASS projection according to data set
    rgrass::execGRASS('g.proj',
                       flags 
                      = c('c','f','quiet'),
                      epsg = as.numeric(epsg)

    )
    #file.remove("o.tif",showWarnings = FALSE)
    # assign GRASS extent
    if (getSpatialClass(x) == "rst") {
      rgrass::execGRASS('g.region',
                         flags = c('quiet','d'),
                         n = as.character(ymax),
                         s = as.character(ymin),
                         e = as.character(xmax),
                         w = as.character(xmin),
                         res = as.character(resolution)
      )
    } else if (getSpatialClass(x) == "vec") {
      rgrass::execGRASS('g.region',
                         flags = c('quiet'),
                         n = as.character(ymax),
                         s = as.character(ymin),
                         e = as.character(xmax),
                         w = as.character(xmin),
                         res=as.character(resolution)
      )
    } else if (getSpatialClass(x) == "paramList") {
      rgrass::execGRASS('g.region',
                         flags = c('quiet'),
                         n = as.character(ymax),
                         s = as.character(ymin),
                         e = as.character(xmax),
                         w = as.character(xmin),
                         res = as.character(resolution)
      )
    }
    else {
      stop("Currently only raster*/terra or sf* objects are supported - have to stop.")
    }
    
    
    }
    } else {
    grass$exist  = FALSE
    returnPaths = TRUE
    }
  if(!quiet) return(rgrass::gmeta())
  if (returnPaths) return(grass)
}
