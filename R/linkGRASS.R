#' Locate and set up GRASS GIS API bindings
#'
#' Initializes a GRASS GIS 7.x/8.x runtime environment and prepares a valid
#' temporary or permanent GRASS location and mapset for use from R.
#'
#' The function detects installed GRASS versions, initializes required
#' environment variables, and derives spatial reference information either
#' from an existing spatial object or from manually provided parameters.
#'
#' @param x A spatial object used to initialize the GRASS location.
#'   Supported classes are `terra::SpatRaster`, `sf`, `sp`, `stars`,
#'   or a file path to a raster dataset.
#' @param epsg Integer EPSG code used to define the GRASS projection.
#'   If `NULL`, the EPSG code is inferred from `x` when possible.
#' @param default_GRASS Optional character vector defining a GRASS installation
#'   (e.g. `c("/usr/lib/grass83", "8.3.2", "grass")`).
#' @param search_path Character path used to search for GRASS installations.
#' @param ver_select Logical or numeric value controlling interactive or indexed
#'   GRASS version selection.
#' @param gisdbase_exist Logical; if `TRUE`, `gisdbase` and `location` must
#'   already exist and will only be linked.
#' @param gisdbase Path to the GRASS database directory.
#' @param use_home Logical; if `TRUE`, the user home directory is used for GISRC.
#' @param location Name of the GRASS location to create or link.
#' @param spatial_params Optional numeric vector defining extent manually
#'   (`xmin, ymin, xmax, ymax[, proj]`).
#' @param resolution Numeric raster resolution used for `g.region`.
#' @param quiet Logical; suppress console output if `TRUE`.
#' @param returnPaths Logical; return detected GRASS installation paths.
#'
#' @details
#' GRASS requires a fully initialized runtime environment (PATH, GISBASE,
#' PROJ, GDAL). On some platforms, R must be started from a shell where GRASS
#' is already available.
#'
#' The function ensures that `PROJ_INFO` and `PROJ_UNITS` are written by
#' explicitly calling `g.proj` before region initialization.
#'
#' @return
#' A list describing the selected GRASS installation and status, or `NULL`
#' if no valid installation was found.
#'
#' @author Chris Reudenbach
#'
#' @seealso
#' \code{\link[rgrass]{initGRASS}},
#' \code{\link[rgrass]{execGRASS}}
#' @examples
#' \dontrun{
#' library(link2GI)
#' library(sf)
#'
#' # Example 1: initialize a temporary GRASS location from an sf object
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#' grass <- linkGRASS(nc)
#'
#' # Example 2: select GRASS version interactively if multiple installations exist
#' linkGRASS(nc, ver_select = TRUE)
#'
#' # Example 3: create a permanent GRASS location
#' root <- tempdir()
#' linkGRASS(
#'   x        = nc,
#'   gisdbase = root,
#'   location = "project1"
#' )
#'
#' # Example 4: link to an existing GRASS location without recreating it
#' linkGRASS(
#'   gisdbase        = root,
#'   location        = "project1",
#'   gisdbase_exist  = TRUE
#' )
#'
#' # Example 5: manual setup using spatial parameters only
#' epsg <- 28992
#' linkGRASS(
#'   spatial_params = c(178605, 329714, 181390, 333611),
#'   epsg = epsg
#' )
#' }
#' @export

linkGRASS <- function(x = NULL, epsg = NULL, default_GRASS = NULL, search_path = NULL,
                      ver_select = FALSE, gisdbase_exist = FALSE, gisdbase = NULL,
                      use_home = FALSE, location = NULL, spatial_params = NULL,
                      resolution = NULL, quiet = TRUE, returnPaths = TRUE) {
  
  # HOME/GISRC base
  home <- if (isTRUE(use_home)) {
    if (Sys.info()["sysname"] == "Windows") Sys.getenv("USERPROFILE") else Sys.getenv("HOME")
  } else {
    tempdir()
  }
  
  # EPSG inference (only if x is provided)
  if (is.null(epsg) && !is.null(x)) {
    crs_info <- try(sf::st_crs(x), silent = TRUE)
    if (!methods::is(crs_info, "try-error")) {
      epsg <- crs_info$epsg
    }
  }
  if (is.na(epsg) || is.null(epsg)) epsg <- 4326
  
  # normalize/convert x types
  if (!is.null(x)) {
    
    if (inherits(x, "character")) {
      xr <- terra::rast(x)
      if (is.na(terra::crs(xr)) || terra::crs(xr) == "") {
        terra::crs(xr) <- sf::st_crs(as.numeric(epsg))$wkt
      }
      x <- xr
      
    } else if (inherits(x, "SpatRaster")) {
      xr <- terra::rast(x)
      if (is.na(terra::crs(xr)) || terra::crs(xr) == "") {
        terra::crs(xr) <- sf::st_crs(as.numeric(epsg))$wkt
      }
      x <- xr
      
    } else if (inherits(x, c("RasterLayer", "RasterStack", "RasterBrick", "Satellite",
                             "SpatialGridDataFrame", "SpatialPixelsDataFrame"))) {
      xr <- terra::rast(x)
      if (is.na(terra::crs(xr)) || terra::crs(xr) == "") {
        terra::crs(xr) <- sf::st_crs(as.numeric(epsg))$wkt
      }
      x <- xr
      
    } else if (inherits(x, "stars")) {
      epsg <- attributes(x)$dimensions[[1]]$refsys$epsg
      if (is.na(epsg) || is.null(epsg)) epsg <- 4326
    }
  }
  
  # search for GRASS on your system
  if (Sys.info()["sysname"] == "Windows") {
    if (is.null(search_path)) search_path <- "C:/"
    grass <- paramGRASSw(default_GRASS, search_path, ver_select, quiet = quiet)
  } else {
    if (is.null(search_path)) search_path <- "/usr/bin"
    grass <- paramGRASSx(default_GRASS, search_path, ver_select, quiet = quiet)
  }
  
  # robust found check
  grass_found <- FALSE
  if (is.list(grass) && !is.null(grass$exist)) {
    grass_found <- isTRUE(grass$exist)
  } else {
    grass_found <- is.list(grass) && length(grass) > 0 && !isFALSE(grass[[1]][1])
  }
  
  if (!grass_found) {
    if (is.list(grass)) grass$exist <- FALSE
    if (!quiet) return(rgrass::gmeta())
    if (returnPaths) return(grass)
    return(invisible(NULL))
  }
  
  # if an existing gdbase is provided link it
  if (!is.null(location) && !is.null(gisdbase) && isTRUE(gisdbase_exist)) {
    
    rgrass::initGRASS(
      gisBase  = grass$gisbase_GRASS,
      home     = home,
      gisDbase = path.expand(gisdbase),
      mapset   = "PERMANENT",
      location = location,
      override = TRUE
    )
    
    grass$exist <- TRUE
    
  } else {
    
    # create temporary location if not provided
    if (is.null(location)) location <- basename(tempfile())
    
    # create gsdbase if not provided
    if (is.null(gisdbase)) {
      gisdbase <- tempdir()
    } else {
      gisdbase <- path.expand(gisdbase)
    }
    
    if (!file.exists(gisdbase)) dir.create(gisdbase, recursive = TRUE)
    if (!file.exists(file.path(gisdbase, location))) dir.create(file.path(gisdbase, location), recursive = TRUE)
    
    # defaults
    if (is.null(resolution)) resolution <- 1
    
    # derive extent/resolution from x or spatial_params
    if (!is.null(x) && is.null(spatial_params)) {
      
      if (getSpatialClass(x) == "rst") {
        xr <- terra::rast(x)
        resolution <- terra::res(xr)[1]
        ex <- terra::ext(xr)
        xmin <- ex[1]; xmax <- ex[2]; ymin <- ex[3]; ymax <- ex[4]
        
      } else if (getSpatialClass(x) == "vec") {
        
        if (inherits(x, "sf")) {
          bb <- sf::st_bbox(x)
          xmin <- as.numeric(bb["xmin"]); xmax <- as.numeric(bb["xmax"])
          ymin <- as.numeric(bb["ymin"]); ymax <- as.numeric(bb["ymax"])
        } else {
          xmin <- x@bbox["x","min"]; xmax <- x@bbox["x","max"]
          ymin <- x@bbox["y","min"]; ymax <- x@bbox["y","max"]
        }
        
      } else {
        stop("Currently only raster*/terra or sf/sp* objects are supported - have to stop.")
      }
      
    } else if (!is.null(spatial_params)) {
      
      if (length(spatial_params) < 4) stop("spatial_params must contain at least xmin,ymin,xmax,ymax")
      xmin <- as.numeric(spatial_params[1])
      ymin <- as.numeric(spatial_params[2])
      xmax <- as.numeric(spatial_params[3])
      ymax <- as.numeric(spatial_params[4])
      
    } else if (is.null(x) && is.null(spatial_params) && !isTRUE(gisdbase_exist)) {
      
      if (!quiet) {
        cat("WARNING\n It is strongly recommended that you provide a raster/terra*, sf/sp* object or manually add the extent, resolution and projection information.\n",
            "These informations are obligatory to setup the GRASS location...\n",
            "Did not find any of them so WGS84 EPSG 4326 world extent is assumed.\n")
      }
      
      xmin <- -180; xmax <- 180; ymin <- -90; ymax <- 90
      epsg <- 4326
      if (is.null(resolution)) resolution <- 1
    }
    
    # start GRASS setup: create TEMPORARY GRASS location
    suppressWarnings(
      rgrass::initGRASS(
        gisBase  = grass$gisbase_GRASS,
        home     = home,
        gisDbase = gisdbase,
        mapset   = "PERMANENT",
        location = location,
        override = TRUE
      )
    )
    
    
    # ---- FIX 1: do NOT pass "quiet" as a flag ----
    # ensure projection files are written to PERMANENT
    rgrass::execGRASS("g.proj", flags = c("c", "f"), epsg = as.numeric(epsg))
    
    # ---- FIX 2: region setup (strings already) ----
    # do NOT pass "quiet" as a flag
    rgrass::execGRASS(
      "g.region",
      flags = "d",
      n = as.character(ymax),
      s = as.character(ymin),
      e = as.character(xmax),
      w = as.character(xmin),
      res = as.character(resolution)
    )
  }
  
  if (!quiet) return(rgrass::gmeta())
  if (returnPaths) return(grass)
  invisible(NULL)
}
