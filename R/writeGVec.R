if ( !isGeneric("writeGvec") ) {
  setGeneric("writeGvec", function(x, ...)
    standardGeneric("writeGvec"))
}

#' Write sf object to GRASS 7
#' @param x sp* or sf* object
#' @param nrasters vector of channels to use from x. Default =nlayers(x)
#' @author Chris Reudenbach
#' 
#' @export writeGvec
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
#' writeGvec(x = meuse_sf,
#'           objName = "meuse_R-G",
#'           gisdbase = "~/temp3",
#'           location = "project1")
#' }

writeGvec <- function(x,objName,gisdbase,location ){

  linkGRASS7(x,gisdbase = gisdbase,location = location)  
  path <- Sys.getenv("GISDBASE")
  sqName <- gsub(tolower(paste0(objName,".sqlite")),pattern = "\\-",replacement = "_")
  st_write(x, file.path(path,sqName))
  
  ret<-rgrass7::execGRASS('v.import',  
                     flags = c("overwrite","quiet","o"),
                     extent="region",
                     input = file.path(path,sqName),
                     layer = sqName,
                     output = gsub(tolower(sqName),pattern = "\\.",replacement = "_"))
  
  # rgrass7::execGRASS('v.out.ogr',  
  #                    flags = c("overwrite","quiet"),
  #                    input = gsub(tolower(paste0(objName,".sqlite")),pattern = "\\.",replacement = "_"),
  #                    output = file.path(path,paste0(objName,"_new.sqlite")),
  #                    format = "SQLite")
  
  #return(sf::st_read(file.path(path,paste0(objName,"_new.sqlite"))))
}