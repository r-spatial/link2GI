
#'@title Searches recursively for existing 'Windows' 'SAGA GIS' installation(s)
#'@name searchSAGAX
#'@description  Search for valid 'GRASS GIS' installations at a given 'Linux' mount point
#'@param MP default mount point is \code{/usr}
#'@param ver_select boolean default is FALSE. If there is more than one 'SAGA GIS' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'SAGA GIS' version 
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@return A dataframe contasining the 'SAGA GIS' root folder(s), the version name(s) and the installation type(s)
#'@author Chris Reudenbach
#'@export searchSAGAX
#'
#'@examples
#' \dontrun{
#'#### Examples how to use searchSAGAX
#'
#' # get all valid SAGA installation folders and params
#' sagaParams<- searchSAGAX()
#' }

searchSAGAX <- function(MP = "/usr",
                        ver_select=FALSE,
                        quiet = TRUE) {
  
  if (Sys.info()["sysname"] == "Linux") {  
    
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for saga_cmd.exe returns all version of otb bat files
    if (!quiet) cat("\nsearching for SAGA GIS installations - this may take a while\n")
    if (!quiet) cat("For providing the path manually see ?searchSAGAX \n")
    
    # find saga and SAGA lib path(es)
    rawSAGA    <- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'saga_cmd' -print"), stdout = TRUE)
    rawSAGALib <- system2("find", paste(MP," ! -readable -prune -o -type f  -iname 'libio_gdal.so' -print"), stdout = TRUE)
    
    # split the search returns of existing SAGA GIS installation(s
    sagaPath <- lapply(seq(length(rawSAGA)), function(i){
      root_dir <- substr(rawSAGA[i],1, gregexpr(pattern = "saga_cmd", rawSAGA[i])[[1]][1] - 1)
      moduleDir <- substr(rawSAGALib[i],1, gregexpr(pattern = "libio_gdal.so", rawSAGALib[i])[[1]][1] - 1)
      
      # put the result in a data frame
      data.frame(binDir = gsub("\\\\$", "", root_dir), 
                 moduleDir = gsub("\\\\$", "", moduleDir),
                 stringsAsFactors = FALSE)
    }) # end lapply
    # bind df 
    sagaPath <- do.call("rbind", sagaPath)
    
    
  } # end of sysname = Windows
  else {sagaPath <- "Sorry no Linux system..." }
  return(sagaPath)
}

#'@title Searches recursively for existing 'Windows' 'SAGA GIS' installation(s)
#'@name searchSAGAW
#'@description  Searches recursivley for existing 'SAGA GIS' installation(s) on a given 'Windows' drive 
#'@param DL drive letter default is "C:"
#'@param ver_select boolean default is FALSE. If there is more than one 'SAGA GIS' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'SAGA GIS' version 
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@return A dataframe contasining the 'SAGA GIS' root folder(s), the version name(s) and the installation type(s)
#'@author Chris Reudenbach
#'@export searchSAGAW
#'
#'@examples
#' \dontrun{
#'#### Examples how to use searchSAGAW 
#'
#' # get all valid SAGA installation folders and params
#' sagaParams<- searchSAGAW()
#' }

searchSAGAW <- function(DL = "C:",
                        ver_select=FALSE,
                        quiet = TRUE) {
  
  if (Sys.info()["sysname"] == "Windows") {  
    sagaPath <- checkPCDomain("saga")  
    if (is.null(sagaPath)) {
      
      # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
      # recursive dir for saga_cmd.exe returns all version of otb bat files
      if (!quiet) cat("\nsearching for SAGA GIS installations - this may take a while\n")
      if (!quiet) cat("For providing the path manually see ?searchSAGAW \n")
      
      # for a straightforward use of a correct codetable using the cmd command "dir" is used
      rawSAGA <- system(paste0("cmd.exe /c dir /B /S ",DL,"\\","saga_cmd.exe"),intern = TRUE)
      
      # trys to identify valid SAGA GIS installation(s) & version number(s)
      sagaPath <- lapply(seq(length(rawSAGA)), function(i){
        cmdfileLines <- rawSAGA[i]
        installerType <- ""
        
        # if "OSGeo4W64" 
        if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), rawSAGA[i], value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("OSGeo4W64", collapse = "|"), rawSAGA[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "saga_cmd.exe", root_dir)[[1]][1] - 1)
          installDir <- root_dir
          installerType <- "osgeo4w64SAGA"
        }    
        
        # if  "OSGeo4W" 
        else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), rawSAGA[i], value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("OSGeo4W", collapse = "|"), rawSAGA[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "saga_cmd.exe", root_dir)[[1]][1] - 1)
          installerType <- "osgeo4wSAGA"
        }
        # if  "QGIS" 
        else if (length(unique(grep(paste("QGIS", collapse = "|"), rawSAGA[i], value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("QGIS", collapse = "|"), rawSAGA[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "saga_cmd.exe", root_dir)[[1]][1] - 1)
          installerType <- "qgisSAGA"
        }
        else{
          root_dir <- substr(rawSAGA[i],1, gregexpr(pattern = "saga_cmd.exe", rawSAGA[i])[[1]][1] - 1)
          installerType <- "soloSAGA"
        }
        if (file.exists(paste0(root_dir,"\\tools"))) moduleDir <- paste0(root_dir,"tools")
        else moduleDir <- paste0(root_dir,"modules")
        # put the result in a data frame
        data.frame(binDir = gsub("\\\\$", "", root_dir), moduleDir = gsub("\\\\$", "", moduleDir), installation_type = installerType,stringsAsFactors = FALSE)
      }) # end lapply
      # bind df 
      sagaPath <- do.call("rbind", sagaPath)
      
    }  #  end of is.null(sagaPath)
  } # end of sysname = Windows
  else {sagaPath <- "Sorry no Windows system..." }
  return(sagaPath)
}
