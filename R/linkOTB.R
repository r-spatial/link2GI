if (!isGeneric('linkOTB')) {
  setGeneric('linkOTB', function(x, ...)
    standardGeneric('linkOTB'))
}

#'@title Locate and set up 'Orfeo ToolBox' API bindings
#'@name linkOTB
#'@description  Locate and set up  \href{https://www.orfeo-toolbox.org/}{'Orfeo ToolBox'} API bindings
#'@details It looks for the \code{otb_cli.bat} file. If the file is found in a \code{bin} folder it is assumed to be a valid 'OTB' binary installation.
#'@param binPathOtb string contains path to where the otb binaries are located
#'@param rootPathOtb string provides the root folder of the \code{binPathOtb}
#'@param verSelect boolean default is FALSE. If there is more than one 'OTB' installation and \code{verSelect} = TRUE the user can select interactively the preferred 'OTB' version 
#'@param DL string hard drive letter default is \code{C:}
#'@param otbType string 
#'
#'@note You may also set the path manually. Using a 'OSGeo4W64' \url{http://trac.osgeo.org/osgeo4w/} installation it is typically \code{C:/OSGeo4W64/bin/}
#'@author Chris Reudenbach
#'@return add otb pathes to the enviroment and creates global variables otbPath
#'@details if called without any parameter \code{linkOTB()} it performs a full search over the hardrive \code{C:}. If it finds one or more 'OTB' binaries it will take the first hit. You have to set \code{verSelect = TRUE} for an interactive selection of the preferred version.
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

linkOTB <- function(binPathOtb=NULL,
                    rootPathOtb= NULL, 
                    otbType=NULL,
                    DL="C:",
                    verSelect=FALSE) {
  
  if (Sys.info()["sysname"] == "Linux") {
    # if no path is provided  we have to search
    
    otbParams <- system2("find", paste("/usr"," ! -readable -prune -o -type f -executable -iname 'otbcli' -print"),stdout = TRUE)
    binPathOtb <- substr(otbParams,1,nchar(otbParams) - 6)  
  makGlobalVar("otbPath", binPathOtb)
  }
    
  
  # (R) set pathes  of OTB  binaries depending on OS WINDOWS
  else if (is.null(binPathOtb)) {
    otbParams <- searchOTBW()
    # if just one valid installation was found take it
    if (nrow(otbParams) == 1) {  
      otbPath <- setOTBEnv(binPathOtb = otbParams$binDir[1],rootPathOtb = otbParams$baseDir[2])
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(otbParams) > 1 & verSelect ) {
      cat("You have more than one valid OTB version\n")
      #print("installation folder: ",otbParams$baseDir,"\ninstallation type: ",otbParams$installationType,"\n")
      print(otbParams[1],right = FALSE,row.names = TRUE) 
      if (is.null(otbType)) {
        ver <- as.numeric(readline(prompt = "Please choose one:  "))
        otbPath <- setOTBEnv(binPathOtb = otbParams$binDir[[ver]], rootPathOtb = otbParams$baseDir[[ver]])
      } else {
        otbPath <- setOTBEnv(binPathOtb = otbParams[otbParams["installationType"] == otbType][1],rootPathOtb = otbParams[otbParams["installationType"] == otbType][2])
      }
    } else {
      otbPath <- setOTBEnv(binPathOtb = otbParams$binDir[[1]],rootPathOtb = otbParams$baseDir[[1]])
    }
    
    # if a setDefaultOTB was provided take this 
  } 
  return(otbPath)
}


