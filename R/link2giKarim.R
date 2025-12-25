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
  if (class(obj)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick", "Satellite", "SpatialGridDataFrame", "stars", "SpatialPixelsDataFrame",
                           "SpatRaster")) {
    "rst"
  } else if (class(obj)[1] %in% c("SpatialPointsDataFrame", "SpatialPoints", "SpatialPolygonsDataFrame", "SpatialPolygons",
                                  "SpatialLinesDataFrame", "SpatialLines", "sf")) {
    "vec"
  } else {
    "paramList"
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
#' add2Path('pathtosomewhere')
#' }
#'@export 
#'
add2Path <- function(newPath) {
  exist <- FALSE
  if (Sys.info()["sysname"] == "Windows") {
    del <- ";"
  } else {
    del <- ":"
  }
  p <- Sys.getenv("PATH")
  if (substr(p, 1, nchar(newPath)) == newPath) {
    exist <- TRUE
  }
  # if not exist append path to systempath
  if (!exist) {
    Sys.setenv(PATH = paste0(newPath, del, Sys.getenv("PATH")))
  }
}

readinteger <- function() {
  n <- readline()
  n <- as.integer(n)
  if (is.na(n)) {
    n <- readinteger()
  }
  return(n)
}

if (!isGeneric("sf2gvec")) {
  setGeneric("sf2gvec", function(x, ...) standardGeneric("sf2gvec"))
}
#' Write sf object directly to `GRASS` vector utilising an existing or creating a new GRASS environment
#' @param x  \code{sf} object corresponding to the settings of the corresponding GRASS container
#' @param obj_name name of GRASS layer
#' @param epsg numeric epsg code
#' @param gisdbase  GRASS gisDbase folder
#' @param location  GRASS location name containing \code{obj_name)}
#' @param gisdbase_exist logical switch if the GRASS gisdbase folder exist default is TRUE
#' @author Chris Reudenbach
#' @note  have a look at the \code{sf} capabilities to write direct to sqlite
#' @export
#' @importFrom sf st_as_sf
#' @importFrom sf st_write
#' @importFrom sf st_read 
#' @examples 
#' 
#' run = FALSE
#' if (run) {
#' ## example 
#' require(sf)
#' require(sp)
#' require(link2GI)
#' data(meuse)
#' meuse_sf = st_as_sf(meuse, 
#'                     coords = c('x', 'y'), 
#'                     crs = 28992, 
#'                     agr = 'constant')
#' 
#' 
#' # write data to GRASS and create gisdbase
#' sf2gvec(x = meuse_sf,
#'         obj_name = 'meuse_R-G',
#'         gisdbase = '~/temp3/',
#'         location = 'project1')
#' 
#' # read from existing GRASS          
#' gvec2sf(x = meuse_sf,
#'         obj_name = 'meuse_r_g',
#'         gisdbase = '~/temp3',       
#'         location = 'project1')
#' 
#' }
sf2gvec <- function(x, epsg, obj_name, gisdbase, location, gisdbase_exist = FALSE) {
  if (gisdbase_exist) {
    linkGRASS(gisdbase = gisdbase, location = location, gisdbase_exist = TRUE)
  } else {
    linkGRASS(x = x, gisdbase = gisdbase, location = location)
  }
  path <- Sys.getenv("GISDBASE")
  sq_name <- gsub(tolower(paste0(obj_name, ".sqlite")), pattern = "\\-", replacement = "_")
  if (!inherits(x, "sf")) {
    sf::st_as_sf(x, x_sf)
  } else {
    x_sf <- x
  }
  # if (!file.exists(file.path(path,sq_name)))
  sf::st_write(x_sf, file.path(path, sq_name), append = FALSE)
  epsg <- sf::st_crs(x)$epsg
  ret <- try(rgrass::execGRASS("v.import", flags = c("overwrite", "o"), input = file.path(path, sq_name), epsg = as.numeric(epsg),
                               output = gsub(tolower(sq_name), pattern = "\\.", replacement = ""), ignore.stderr = TRUE, intern = TRUE), silent = FALSE)
  if (methods::is(ret, "try-error"))
    return(cat("Data not found"))
}
if (!isGeneric("gvec2sf")) {
  setGeneric("gvec2sf", function(x, ...) standardGeneric("gvec2sf"))
}
#' Converts from an existing `GRASS` environment an arbitrary vector dataset  into a  sf object
#' @param x  sf object corresponding to the settings of the corresponding GRASS container
#' @param obj_name name of GRASS layer
#' @param gisdbase  GRASS gisDbase folder
#' @param location  GRASS location name containing \code{obj_name}
#' @param gisdbase_exist logical switch if the GRASS gisdbase folder exist default is TRUE
#' @author Chris Reudenbach
#' @note  have a look at the sf capabilities to read direct from sqlite
#' @export
#' @examples 
#' 
#' run = FALSE
#' if (run) {
#' ## example 
#' require(sf)
#' require(sp)
#' require(link2GI)
#' data(meuse)
#' meuse_sf = st_as_sf(meuse, 
#'                     coords = c('x', 'y'), 
#'                     crs = 28992, 
#'                     agr = 'constant')
#' 
#' 
#' # write data to GRASS and create gisdbase
#' sf2gvec(x = meuse_sf,
#'         obj_name = 'meuse_R-G',
#'         gisdbase = '~/temp3/',
#'         location = 'project1')
#' 
#' # read from existing GRASS          
#' gvec2sf(x = meuse_sf,
#'         obj_name = 'meuse_r_g',
#'         gisdbase = '~/temp3',       
#'         location = 'project1')
#' 
#' }
gvec2sf <- function(x, obj_name, gisdbase, location, gisdbase_exist = TRUE) {
  if (gisdbase_exist)
    linkGRASS(gisdbase = gisdbase, location = location, gisdbase_exist = TRUE) else linkGRASS(x, gisdbase = gisdbase, location = location)
  path <- Sys.getenv("GISDBASE")
  sq_name <- gsub(tolower(paste0(obj_name, ".sqlite")), pattern = "\\-", replacement = "_")
  ret <- try(rgrass::execGRASS("v.out.ogr", flags = c("overwrite", "quiet"), input = gsub(tolower(sq_name), pattern = "\\.",
                                                                                          replacement = ""), output = file.path(path, paste0(obj_name, "_new.sqlite")), format = "SQLite", ignore.stderr = TRUE,
                               intern = TRUE), silent = TRUE)
  if (!methods::is(ret, "try-error"))
    return(sf::st_read(file.path(path, paste0(obj_name, "_new.sqlite")), quiet = TRUE)) else return(cat("Data not found"))
}


.bf_wpath <- function(path) {
  if (identical(path, "default")) path <- "C:/"
  
  # normalize separators
  path <- gsub("\\\\", "/", path)
  
  # only Windows has shortPathName()
  if (identical(Sys.info()[["sysname"]], "Windows")) {
    path <- gsub("/", "\\\\", path)
    path <- utils::shortPathName(path)
  }
  
  path
}

.reverse_bf_wpath <- function(path) {
  path <- gsub("\\\\", "/", path)
  
  if (identical(Sys.info()[["sysname"]], "Windows")) {
    # keep forward slashes in return, but resolve 8.3 if given
    path <- utils::shortPathName(gsub("/", "\\\\", path))
    path <- gsub("\\\\", "/", path)
  }
  
  path
}

.readkey <- function() {
  cat("[press [ESC] to continue]")
  invisible(readline())
}


.reverse_bf_wpath <- function(path) {
  path <- gsub("\\\\", "/", path)
  # path = gsub('/', '\\\\', path)
  path <- utils::shortPathName(path)
  return(path)
}
.readkey <- function() {
  cat("[press [ESC] to continue]")
  line <- readline()
}

# ---- Minimal wrapper layer for testability (mock-friendly) ----

.link2gi_sys2 <- function(command, args = character(), stdout = TRUE, stderr = FALSE, ...) {
  base::system2(command = command, args = args, stdout = stdout, stderr = stderr, ...)
}

.link2gi_readLines <- function(con, warn = FALSE, ...) {
  base::readLines(con = con, warn = warn, ...)
}

.link2gi_which <- function(x) {
  # Sys.which returns named character vector
  Sys.which(x)
}

.link2gi_dir_exists <- function(path) {
  base::dir.exists(path)
}

.link2gi_file_exists <- function(path) {
  base::file.exists(path)
}

.link2gi_glob <- function(path) {
  Sys.glob(path)
}

