if (!isGeneric('linkGDAL')) {
  setGeneric('linkGDAL', function(x, ...)
    standardGeneric('linkGDAL'))
}

#'@title Locate and set up 'GDAL' API bindings
#'@name linkGDAL
#'@description  Locate and set up  \href{https://www.gdal.org}{'GDAL - Geospatial Data Abstraction Librar'} API bindings
#'@details It looks for the \code{gdalinfo(.exe)} file. If the file is found in a \code{bin} folder it is assumed to be a valid 'GDAL' binary installation.
#'@param bin_GDAL string contains path to where the gdal binaries are located
#'@param root_GDAL string provides the root folder of the \code{bin_GDAL}
#'@param ver_select boolean default is FALSE. If there is more than one 'GDAL' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'GDAL' version 
#'@param searchLocation string hard drive letter default is \code{C:}
#'@param type_GDAL string 
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@param returnPaths boolean if set to FALSE the pathes of the selected version are written 
#' to the PATH variable only, otherwise all paths and versions of the installed GRASS versions ae returned.

#'
#'@note You may also set the path manually. Using a 'OSGeo4W64' \url{http://trac.osgeo.org/osgeo4w/} installation it is typically \code{C:/OSGeo4W64/bin/}
#'@author Chris Reudenbach
#'@return add gdal pathes to the enviroment and creates global variables path_GDAL
#'@details if called without any parameter \code{linkGDAL()} it performs a full search over the hardrive \code{C:}. If it finds one or more 'GDAL' binaries it will take the first hit. You have to set \code{ver_select = TRUE} for an interactive selection of the preferred version.
#'@export linkGDAL
#'  
#'@examples
#' \dontrun{
#' # call if you do not have any idea if and where GDAL is installed
#' gdal<-linkGDAL()
#' if (gdal$exist) {
#' # call it for a default OSGeo4W installation of the GDAL
#' print(gdal)
#' }
#'}

linkGDAL <- function(bin_GDAL=NULL,
                     root_GDAL= NULL, 
                     type_GDAL=NULL,
                     searchLocation=NULL,
                     ver_select=FALSE,
                     quiet = TRUE,
                     returnPaths = TRUE) {
  
  if (is.null(searchLocation)){
    if (Sys.info()["sysname"] == "Windows") {
      searchLocation<-"C:"
    } else 
    {searchLocation<-"/usr"}
  }
  params_GDAL <- findGDAL(searchLocation = searchLocation,quiet = quiet)
  # if no path is provided  we have to search
  #cat(nrow(params_GDAL))
  #params_GDAL <- system2("find", paste("/usr"," ! -readable -prune -o -type f -executable -iname 'gdalinfo' -print"),stdout = TRUE)
  #bin_GDAL <- substr(params_GDAL,1,nchar(params_GDAL) - 6)  
  #pathGDAL <- bin_GDAL
  #params_GDAL <- searchGDALW()
  # if just one valid installation was found take it
  if (nrow(params_GDAL$gdalInstallations) != FALSE){
    if (Sys.info()["sysname"] != "Windows"){   
      if (nrow(params_GDAL$gdalInstallations) == 1) {  
        pathGDAL <-params_GDAL$gdalInstallations[[1]][1]
        
        # if more than one valid installation was found you have to choose 
      } else if (nrow(params_GDAL$gdalInstallations) > 1 & is.numeric(ver_select) & ver_select > 0 ) {
        #print("installation folder: ",params_GDAL$baseDir,"\ninstallation type: ",params_GDAL$installationType,"\n")
        pathGDAL <- params_GDAL$gdalInstallations[[1]][ver_select]
        if(!quiet){
          cat("You have more than one valid GDAL version\n")
          print(params_GDAL$gdalInstallations,right = FALSE,row.names = TRUE) 
          cat("You have selected: ",ver_select,"\n")}
      } else if (nrow(params_GDAL$gdalInstallations) > 1 &  (!ver_select)) {
        
        recentGDAL <- getrowGDALVer(params_GDAL$gdalInstallations)
        pathGDAL <- params_GDAL$gdalInstallations[[1]][recentGDAL]
        if (!quiet){   cat("You have choosen version: ",ver_select,"\n")}
      } 
      else if (nrow(params_GDAL$gdalInstallations) > 1 & ver_select ) {
        cat("You have more than one valid GDAL version\n")
        #print("installation folder: ",params_GDAL$baseDir,"\ninstallation type: ",params_GDAL$installationType,"\n")
        print(params_GDAL,right = FALSE,row.names = TRUE) 
        if (is.null(type_GDAL)) {
          ver <- as.numeric(readline(prompt = "Please choose one:  "))
          pathGDAL <- params_GDAL$gdalInstallations[[1]][recentGDAL]
        } 
      } 
      
      # (R) set pathes  of GDAL  binaries depending on OS WINDOWS 
    }  else {    
      # if (is.null(searchLocation)) searchLocation<-"C:"
      # params_GDAL <- findGDAL(searchLocation = searchLocation,quiet = quiet)
      #if ( params_GDAL != FALSE)
      if (nrow(params_GDAL[[1]][1]) == 1) {  
        
        pathGDAL <- setenvGDAL(bin_GDAL =params_GDAL$gdalInstallations["binDir"][[1]][1],root_GDAL = params_GDAL$gdalInstallations["binDir"][[1]][1])
        
        # if more than one valid installation was found you have to choose 
      } else if (nrow(params_GDAL$gdalInstallations) > 1 & ver_select ) {
        cat("You have more than one valid GDAL version\n")
        #print("installation folder: ",params_GDAL$baseDir,"\ninstallation type: ",params_GDAL$installationType,"\n")
        print(nrow(params_GDAL$gdalInstallations),right = FALSE,row.names = TRUE) 
        if (is.null(type_GDAL)) {
          ver <- as.numeric(readline(prompt = "Please choose one:  "))
          pathGDAL <- setenvGDAL(bin_GDAL = params_GDAL$gdalInstallations["binDir"][[1]][ver], root_GDAL = params_GDAL$gdalInstallations["binDir"][[1]][ver])
          #gdalCmd<- paste0(pathGDAL,"gdalcli.bat")
        } else {
          pathGDAL <- setenvGDAL(bin_GDAL = params_GDAL[params_GDAL["installationType"] == type_GDAL][1],root_GDAL = params_GDAL[params_GDAL["installationType"] == type_GDAL][2])
        }
      }
    }
    #else {
    #    pathGDAL <- setenvGDAL(bin_GDAL = params_GDAL$binDir[[1]],root_GDAL = params_GDAL$baseDir[[1]])
    #  }
    
    # if a setDefaultGDAL was provided take this 
    
    gdal<-list()
    gdal$pathGDAL<-pathGDAL
    #gdal$gdalCmd<-gdalCmd
    gdal$version<-params_GDAL
    gdal$exist<-TRUE
  }
  else { 
    gdal<-list()
    gdal$exist<-FALSE
    returnPaths <-TRUE
  }
  if (returnPaths) return(gdal)
}


