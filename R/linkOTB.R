if (!isGeneric('linkOTB')) {
  setGeneric('linkOTB', function(x, ...)
    standardGeneric('linkOTB'))
}

#'@title Locate and set up 'Orfeo ToolBox' API bindings
#'@name linkOTB
#'@description  Locate and set up  \href{https://www.orfeo-toolbox.org/}{'Orfeo ToolBox'} API bindings
#'@details It looks for the \code{otb_cli.bat} file. If the file is found in a \code{bin} folder it is assumed to be a valid 'OTB' binary installation.
#'@param bin_OTB string contains path to where the otb binaries are located
#'@param root_OTB string provides the root folder of the \code{bin_OTB}
#'@param ver_select boolean default is FALSE. If there is more than one 'OTB' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'OTB' version 
#'@param searchLocation string hard drive letter default is \code{C:}
#'@param type_OTB string 
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@param returnPathes boolean if set to FALSE the pathes of the selected version are written 
#' to the PATH variable only, otherwise all paths and versions of the installed GRASS versions ae returned.

#'
#'@note You may also set the path manually. Using a 'OSGeo4W64' \url{http://trac.osgeo.org/osgeo4w/} installation it is typically \code{C:/OSGeo4W64/bin/}
#'@author Chris Reudenbach
#'@return add otb pathes to the enviroment and creates global variables path_OTB
#'@details if called without any parameter \code{linkOTB()} it performs a full search over the hardrive \code{C:}. If it finds one or more 'OTB' binaries it will take the first hit. You have to set \code{ver_select = TRUE} for an interactive selection of the preferred version.
#'@export linkOTB
#'  
#'@examples
#' \dontrun{
#' # call if you do not have any idea if and where OTB is installed
#' linkOTB()
#' 
#' # call it for a default OSGeo4W installation of the OTB
#' linkOTB("C:/OSGeo4W64/bin/")
#' 
#' # call it for a default Linux installation of the OTB
#' linkOTB("/usr/bin/")
#'}

linkOTB <- function(bin_OTB=NULL,
                    root_OTB= NULL, 
                    type_OTB=NULL,
                    searchLocation=NULL,
                    ver_select=FALSE,
                    quiet = TRUE,
                    returnPathes = TRUE) {
  
  
  if (Sys.info()["sysname"] == "Linux") {
    if (is.null(searchLocation)) searchLocation<-"/usr"
    params_OTB <- findOTB(searchLocation = searchLocation,quiet = quiet)
    # if no path is provided  we have to search
    cat(nrow(params_OTB))
    #params_OTB <- system2("find", paste("/usr"," ! -readable -prune -o -type f -executable -iname 'otbcli' -print"),stdout = TRUE)
    #bin_OTB <- substr(params_OTB,1,nchar(params_OTB) - 6)  
    #pathOTB <- bin_OTB
    #params_OTB <- searchOTBW()
    # if just one valid installation was found take it
    if (nrow(params_OTB) == 1) {  
      pathOTB <- params_OTB[1]
      otbCmd <- params_OTB[2]
      # if more than one valid installation was found you have to choose 
    } else if (nrow(params_OTB) > 1 & ver_select ) {
      cat("You have more than one valid OTB version\n")
      #print("installation folder: ",params_OTB$baseDir,"\ninstallation type: ",params_OTB$installationType,"\n")
      print(params_OTB,right = FALSE,row.names = TRUE) 
      if (is.null(type_OTB)) {
        ver <- as.numeric(readline(prompt = "Please choose one:  "))
        pathOTB <- params_OTB$binDir[[ver]] 
        otbCmd <- params_OTB$otbCmd[[ver]]
      } 
    } 
    
    # (R) set pathes  of OTB  binaries depending on OS WINDOWS 
  }  else {    
    if (is.null(searchLocation)) searchLocation<-"C:"
    params_OTB <- findOTB(searchLocation = searchLocation,quiet = quiet)
    if (nrow(params_OTB) == 1) {  

      pathOTB <- setenvOTB(bin_OTB = params_OTB$binDir[1],root_OTB = params_OTB$baseDir[2])
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(params_OTB) > 1 & ver_select ) {
      cat("You have more than one valid OTB version\n")
      #print("installation folder: ",params_OTB$baseDir,"\ninstallation type: ",params_OTB$installationType,"\n")
      print(params_OTB[1],right = FALSE,row.names = TRUE) 
      if (is.null(type_OTB)) {
        ver <- as.numeric(readline(prompt = "Please choose one:  "))
        pathOTB <- setenvOTB(bin_OTB = params_OTB$binDir[[ver]], root_OTB = params_OTB$baseDir[[ver]])
        #otbCmd<- paste0(pathOTB,"otbcli.bat")
      } else {
        pathOTB <- setenvOTB(bin_OTB = params_OTB[params_OTB["installationType"] == type_OTB][1],root_OTB = params_OTB[params_OTB["installationType"] == type_OTB][2])
      }
    }
  }
  #else {
  #    pathOTB <- setenvOTB(bin_OTB = params_OTB$binDir[[1]],root_OTB = params_OTB$baseDir[[1]])
  #  }
    
    # if a setDefaultOTB was provided take this 
  
  otb<-list()
  otb$pathOTB<-pathOTB
  #otb$otbCmd<-otbCmd
  otb$version<-params_OTB
  if (returnPathes) return(otb)
}


