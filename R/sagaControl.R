
#'@title Searches recursively for existing 'Windows' 'SAGA GIS' installation(s)
#'@name searchSAGAX
#'@description  Search for valid 'GRASS GIS' installations at a given 'Linux' mount point
#'@param MP default mount point is \code{/usr}
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@return A dataframe contasining the 'SAGA GIS' root folder(s), the version name(s) and the installation type(s)
#'@author Chris Reudenbach
#'@keywords internal
#'@export searchSAGAX
#'
#'@examples
#' \dontrun{
#'#### Examples how to use searchSAGAX
#'
#' # get all valid SAGA installation folders and params
#' searchSAGAX()
#' }

searchSAGAX <- function(MP = "/usr",
                        quiet = TRUE) {
  if (MP=="default") MP <- "/usr"
  
  if (Sys.info()["sysname"] == "Linux") {  
    
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for saga_cmd.exe returns all version of otb bat files
    if (!quiet) cat("\nsearching for SAGA GIS installations - this may take a while\n")
    if (!quiet) cat("For providing the path manually see ?searchSAGAX \n")
    
    # find saga and SAGA lib path(es)
    options(show.error.messages = FALSE)
    options(warn=-1)
    rawSAGA    <- try(system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'saga_cmd' -print"), stdout = TRUE))
    rawSAGALib <- try(system2("find", paste(MP," ! -readable -prune -o -type f  -iname 'libio_gdal.so' -print"), stdout = TRUE))
    
   if (grepl(rawSAGA,pattern = "File not found") | grepl(rawSAGA,pattern = "Datei nicht gefunden")) {

      class(rawSAGA) <- c("try-error", class(rawSAGA))
    }
    options(show.error.messages = TRUE)
    options(warn=0)
    if(!class(rawSAGA) == "try-error") { 
    
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
   }
  else {
      if (!quiet) cat(paste("Did not find any valid SAGA installation at mount point",MP))
      return(sagaPath <- FALSE)}
    
  } # end of sysname = Windows
  else {
    sagaPath <- NULL 
    cat("No SAGA GIS found.\n")
  }
  return(sagaPath)
}

#'@title Searches recursively for existing 'Windows' 'SAGA GIS' installation(s)
#'@name searchSAGAW
#'@description  Searches recursivley for existing 'SAGA GIS' installation(s) on a given 'Windows' drive 
#'@param DL drive letter default is "C:"
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@return A dataframe contasining the 'SAGA GIS' root folder(s), the version name(s) and the installation type(s)
#'@author Chris Reudenbach
#'@keywords internal
#'@export searchSAGAW
#'
#'@examples
#' \dontrun{
#'#### Examples how to use searchSAGAW 
#'
#' # get all valid SAGA installation folders and params
#' searchSAGAW()
#' }

searchSAGAW <- function(DL = "C:",
                        quiet = TRUE) {
  if (DL=="default") DL <- "C:"
  if (Sys.info()["sysname"] == "Windows") {  
    sagaPath <- checkPCDomain("saga")  
    if (is.null(sagaPath)) {
      
      # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
      # recursive dir for saga_cmd.exe returns all version of otb bat files
      if (!quiet) cat("\nsearching for SAGA GIS installations - this may take a while\n")
      if (!quiet) cat("For providing the path manually see ?searchSAGAW \n")
      
      # for a straightforward use of a correct codetable using the cmd command "dir" is used
      
      options(show.error.messages = FALSE)
      options(warn=-1)
      rawSAGA <- try(system(paste0("cmd.exe /c dir /B /S ",DL,"\\","saga_cmd.exe"),intern = TRUE))
      
      if (grepl(rawSAGA,pattern = "File not found") | grepl(rawSAGA,pattern = "Datei nicht gefunden")) {
        rawSAGA<- "message"
        class(rawSAGA) <- c("try-error", class(rawSAGA))
      }
      options(show.error.messages = TRUE)
      options(warn=0)
      
      if(!class(rawSAGA)[1] == "try-error") {
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
      
      }else {
        if (!quiet) cat(paste("Did not find any valid SAGA installation at mount point",DL))
        sagaPath <- FALSE}
    }  #  end of is.null(sagaPath)
  } # end of sysname = Windows
  else {
    sagaPath <-NULL
    cat("Sorry no Windows system...")
  }
  return(sagaPath)
}

#'@title Search recursivly existing 'SAGA GIS' installation(s) at a given drive/mountpoint 
#'@name findSAGA
#'@description  Provides an  list of valid 'SAGA GIS' installation(s) 
#'on your 'Windows' system. There is a major difference between osgeo4W and 
#'stand_alone installations. The functions trys to find all valid 
#'installations by analysing the calling batch scripts.
#'@param searchLocation drive letter to be searched, for Windows systems default
#' is \code{C:}, for Linux systems default is \code{/usr}.
#'@param quiet boolean  switch for supressing console messages default is TRUE

#'@return A dataframe with the 'SAGA GIS' root folder(s), version name(s) and 
#'installation type code(s)
#'@author Chris Reudenbach
#'@export findSAGA
#'
#'@examples
#' \dontrun{
#' # find recursively all existing 'SAGA GIS' installation folders starting 
#' # at the default search location
#' findSAGA()
#' }
findSAGA <- function(searchLocation = "default", 
                     quiet = TRUE) {
  
  if (Sys.info()["sysname"] == "Windows") {
    if (searchLocation=="default") searchLocation <- "C:"
    if (searchLocation %in% paste0(LETTERS,":"))
      link = link2GI::searchSAGAW(DL = searchLocation,quiet = quiet)  
    else stop("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon" )
  } else {
    if (searchLocation=="default") searchLocation <- "/usr"
    if (grepl(searchLocation,pattern = ":"))  stop("You are running Linux - please choose a suitable searchLocation argument" )
    else link = link2GI::searchSAGAX(MP = searchLocation,quiet = quiet)
  }
  return(link)
}

getrowSagaVer<- function (paths){
  #tmp<-c()
  scmd = ifelse(Sys.info()["sysname"]=="Windows", "saga_cmd.exe", "saga_cmd")
  sep = ifelse(Sys.info()["sysname"]=="Windows", "\\", "/")
  highestVer<-"2.0.8"
  for (i in 1:nrow(paths)){
  tmp<-  strsplit(x = system(paste0(paste0(shQuote(paths$binDir[i]),sep,scmd)," --version"),intern = TRUE),split = "SAGA Version: ")[[1]][2]
  highestVer <- max(tmp,highestVer)
  pathI <- i
  }
  return (pathI)
}


getSagaVer<- function (paths){
  sep = ifelse(Sys.info()["sysname"]=="Windows", "\\", "/")
  scmd = ifelse(Sys.info()["sysname"]=="Windows", "saga_cmd.exe", "saga_cmd")
  sagaVersion<-  strsplit(x = system(paste0(paste0(shQuote(paths),sep,scmd)," --version"),intern = TRUE),split = "SAGA Version: ")[[1]][2]
  return (sagaVersion)
}