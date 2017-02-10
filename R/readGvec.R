if ( !isGeneric("readGvec") ) {
  setGeneric("readGvec", function(x, ...)
    standardGeneric("readGvec"))
}

#' Read GRASS 7 vector into  sf object
#' @param x sf* object corresponding to the settings of the corresponding GRASS container
#' @param objName name of GRASS layer
#' @param gisdbase  GRASS gisDbase folder
#' @param location  GRASS location name containing \code{objName)}
#' @author Chris Reudenbach
#' 
#' @export readGvec
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
#' readGvec(x = meuse_sf,
#'           objName = "meuse_R-G",
#'           gisdbase = "~/temp3",
#'           location = "project1")
#' }

readGvec <- function(x,objName,gisdbase,location ){

  linkGRASS7(x,gisdbase = gisdbase,location = location)  
  path <- Sys.getenv("GISDBASE")
  sqName <- gsub(tolower(paste0(objName,".sqlite")),pattern = "\\-",replacement = "_")

   rgrass7::execGRASS('v.out.ogr',  
                      flags = c("overwrite","quiet"),
                      input = gsub(tolower(sqName),pattern = "\\.",replacement = "_"),
                      output = file.path(path,paste0(objName,"_new.sqlite")),
                      format = "SQLite")
   
  return(sf::st_read(file.path(path,paste0(objName,"_new.sqlite"))))
}