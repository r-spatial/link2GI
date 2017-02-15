if ( !isGeneric("r_gvec") ) {
  setGeneric("r_gvec", function(x, ...)
    standardGeneric("r_gvec"))
}

#' Read GRASS 7 vector into  sf object
#' @param x  \code{\link{sf}} object corresponding to the settings of the corresponding GRASS container
#' @param obj_name name of GRASS layer
#' @param gisdbase  GRASS gisDbase folder
#' @param location  GRASS location name containing \code{obj_name)}
#' @author Chris Reudenbach
#' 
#' @export r_gvec
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
#' r_gvec(x = meuse_sf,
#'           obj_name = "meuse_R-G",
#'           gisdbase = "~/temp3",
#'           location = "project1")
#' }

r_gvec <- function(x, obj_name, gisdbase, location ,gisdbase_exist = FALSE){

  if (gisdbase_exist)
    linkGRASS7(gisdbase = gisdbase, location = location, gisdbase_exist = TRUE)  
  else 
    linkGRASS7(x, gisdbase = gisdbase, location = location)  
  path <- Sys.getenv("GISDBASE")
  sq_name <- gsub(tolower(paste0(obj_name,".sqlite")),pattern = "\\-",replacement = "_")

   rgrass7::execGRASS('v.out.ogr',  
                      flags = c("overwrite","quiet"),
                      input = gsub(tolower(sq_name),pattern = "\\.",replacement = "_"),
                      output = file.path(path,paste0(obj_name,"_new.sqlite")),
                      format = "SQLite",
                      ignore.stderr = TRUE,
                      intern = TRUE)
   
  return(sf::st_read(file.path(path,paste0(obj_name,"_new.sqlite")),quiet = TRUE))
}