
#'@title  Usually for internally usage, initializes and set up  access to the 'GDAL' command line interface
#'@name setenvGDAL
#'@description  Initializes and set up  access to the 'GDAL' command line interface
#'  
#'@param bin_GDAL  string contains the path to the 'GDAL' binaries
#'@return Adds 'GDAL' pathes to the enviroment and creates the variable global string variable \code{gdalCmd}, that contains the path to the 'GDAL' binaries.
#'@export setenvGDAL
#'  
#'@examples
#' \dontrun{
#'## example for the most common default OSGeo4W64 installation of GDAL
#'setenvGDAL(bin_GDAL = "C:/OSGeo4W64/bin/",
#'           root_GDAL = "C:/OSGeo4W64")
#'}

setenvGDAL <- function(bin_GDAL = NULL){
  
  
  # (R) set pathes  of gdal modules and binaries depending on OS  
  if (Sys.info()["sysname"] == "Windows") {
    if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
    #makGlobalVar("gdalPath", bin_GDAL)
    add2Path(bin_GDAL)
    Sys.setenv(GDAL_DATA =  bin_GDAL)
  }
  else {
    if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
    #makGlobalVar("gdalPath", bin_GDAL)
    add2Path((bin_GDAL))
    p<-system(paste0((bin_GDAL),"gdal-config --datadir"),intern = TRUE,ignore.stdout = FALSE,ignore.stderr = FALSE)
    #Sys.setenv(GDAL_DRIVER_PATH = root_GDAL)
    Sys.setenv(GDAL_DATA = p)
  }
  
  return(bin_GDAL)
}

#'@title Search recursively for valid 'GDAL' installation(s) on a 'Windows' OS
#'@name searchGDALW
#'@description  Search for valid 'GDAL' installations on a 'Windows' OS
#'@param DL drive letter default is "C:"
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@return A dataframe with the 'GDAL' root folder(s) the version name(s) and the installation type(s).
#'@author Chris Reudenbach
#'@export searchGDALW
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' # get all valid GDAL installation folders and params
#' searchGDALW()
#' }

searchGDALW <- function(DL = "C:",
                        quiet=TRUE) {
  if (DL=="default") DL <- "C:"
  if (Sys.info()["sysname"] == "Windows") {
    if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
    
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for gdal*.bat returns all version of gdal bat files
    if (!quiet) cat("\nsearching for GDAL installations - this may take a while\n")
    if (!quiet) cat("For providing the path manually see ?searchGDALW \n")
    
    
    options(show.error.messages = FALSE)
    options(warn=-1)
    raw_GDAL  <- try(system(paste0("cmd.exe"," /c dir /B /S ",DL,"\\","gdalinfo.exe"),intern=TRUE))
    if (identical(raw_GDAL, character(0))) raw_GDAL <- "File not found"
    if (grepl(raw_GDAL,pattern = "File not found") | grepl(raw_GDAL,pattern = "Datei nicht gefunden")) {
      
      class(raw_GDAL) <- c("try-error", class(raw_GDAL))
    }
    options(show.error.messages = TRUE)
    options(warn=0)
    gdal1<-gdal_py<-gdal_bin<-list()
    if(!class(raw_GDAL)[1] == "try-error")  {
      #if (!grepl(DL,raw_GDAL)) stop("\n At ",DL," no GDAL installation found")
      
      # trys to identify valid gdal installations and their version numbers
      gdalInstallations <- lapply(seq(length(raw_GDAL)), function(i){
        # convert codetable according to cmd.exe using type
        batchfile_lines <- raw_GDAL[i]
        installerType <- ""
        installDir = ""
        root_dir=""

        # if the the tag "OSGEO4W64" exists set installation_type
        if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), raw_GDAL[i], value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("OSGeo4W64", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          if (file.exists(file.path(root_dir,"gdalinfo.exe"))) installerType <- "osgeo4w64"
          else installerType <- "unknown"
          installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        }    
        
        # if the the tag "OSGEO4W" exists set installation_type
        else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), raw_GDAL[i], value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("OSGeo4W", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          if (file.exists(file.path(root_dir,"gdalinfo.exe"))) installerType <- "osgeo4w"
          else installerType <- "unknown"
          installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        }
        # if the the tag "QGIS" exists set installation_type
        else if (length(unique(grep(paste("QGIS", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("QGIS", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          if (file.exists(file.path(root_dir,"gdalinfo.exe"))) installerType <- "qgis"
          else installerType <- "unknown"
          installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          
        }
        # if the the tag "GDAL-" exists set installation_type
        else if (length(unique(grep(paste("GDAL", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("GDAL", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          if (file.exists(file.path(root_dir,"gdalinfo.exe"))) installerType <- "GDAL"
          else installerType <- "unknown"
          
          installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          
        }        # if the the tag "GRASS-" exists set installation_type
        else if (length(unique(grep(paste("GRASS", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("GRASS", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          bundle <- substr(root_dir,gregexpr(pattern = "GRASS", root_dir)[[1]][1],gregexpr(pattern = "bin", root_dir)[[1]][1]-7)
          if (file.exists(file.path(root_dir,"gdalinfo.exe"))) installerType <- bundle
          else installerType <- "unknown"
          
        }        # if the the tag "OTB-" exists set installation_type
        else if (length(unique(grep(paste("OTB", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("OTB", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir,1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          if (file.exists(file.path(root_dir,"gdalinfo.exe"))) installerType <- "OTB"
          else installerType <- "unknown"
          
        }
        
        # put the existing GISBASE directory, version number  and installation type in a data frame
        data.frame(binDir = root_dir, baseDir = installDir, installation_type = installerType, stringsAsFactors = FALSE)
      }) # end lapply
      # bind the df lines
      gdalInstallations <- do.call("rbind", gdalInstallations)
      for (i in 1:nrow(gdalInstallations)){
        gdal_bin[[i]]<- as.data.frame(grep(Sys.glob(file.path(R.utils::getAbsolutePath(gdalInstallations[[1]][i]),"gdal*")),pattern = "\\.(py)$", value=TRUE, invert=TRUE))
        names(gdal_bin[[i]])<-"gdal_bin"
        gdal_py[[i]]<-as.data.frame(grep(Sys.glob(file.path(R.utils::getAbsolutePath(gdalInstallations[[1]][i]),"gdal*")),pattern = "\\.(py)$", value=TRUE))
        names(gdal_py[[i]])<-"gdal_py"}
      
    } else {
      if(!quiet) cat("Did not find any valid GDAL installation at mount point",DL)
      return(gdalInstallations <- FALSE)}
    
  } else {
    gdalInstallations <- NULL
    cat("Sorry no Windows system..." )
    
  }
  gdalInstallations$binDir<- utils::shortPathName(gdalInstallations$binDir)
  gdalInstallations$baseDir<- utils::shortPathName(gdalInstallations$baseDir)
  df <-gdalInstallations[gdalInstallations$installation_type != "unknown", ]
  rownames(df) <- 1:nrow(df)
  gdal1$gdalInstallations<-df
  gdal1$bin<- gdal_bin[gdalInstallations$installation_type != "unknown" ]
  gdal1$py<- gdal_py[gdalInstallations$installation_type != "unknown" ]
  
  return(gdal1)
}

#'@title Search recursively for valid 'GDAL' installation(s) on a 'Windows' OS
#'@name searchGDALX
#'@description  Search for valid 'GDAL' installations on a 'Windows' OS
#'@param MP drive letter default is "C:"
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@return A dataframe with the 'GDAL' root folder(s) the version name(s) and the installation type(s).
#'@author Chris Reudenbach
#'@export searchGDALX
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' # get all valid GDAL installation folders and params
#' searchGDALX()
#' }

searchGDALX <- function(MP = "/usr",
                        quiet=TRUE) {
  if (MP=="default") MP <- "/usr"
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
  # trys to find a osgeo4w installation at the mounting point  disk returns root directory and version name
  # recursive dir for gdal*.bat returns all version of gdal bat files
  if (!quiet) cat("\nsearching for GDAL Toolbox installations - this may take a while\n")
  if (!quiet) cat("For providing the path manually see ?searchGDALX \n")
  
  raw_GDAL <- 
    options(show.error.messages = FALSE)
  options(warn=-1)
  raw_GDAL  <- try(system2("find", paste( MP," ! -readable -prune -o -type f -executable -iname 'gdalinfo' -print"),stdout = TRUE))
  if (identical(raw_GDAL, character(0))) raw_GDAL <- "File not found"
  if (grepl(raw_GDAL,pattern = "File not found") | grepl(raw_GDAL,pattern = "Datei nicht gefunden")) {
    
    class(raw_GDAL) <- c("try-error", class(raw_GDAL))
  }
  options(show.error.messages = TRUE)
  options(warn=0)
  gdal1<-gdal_py<-gdal_bin<-list()
  if(!class(raw_GDAL)[1] == "try-error") {
    #if (!grepl(MP,raw_GDAL)) stop("\n At ",MP," no GDAL installation found")
    # trys to identify valid gdal installations and their version numbers
    gdalInstallations <- lapply(seq(length(raw_GDAL)), function(i){
      
      # TODO strip version from GDAL /usr/bin/gdalcli_BandMath -version
      # "This is the BandMath application, version 6.0.0"
      
      # if the the tag "OSGEO4W64" exists set installation_type
      root_dir <- substr(raw_GDAL[i],1, gregexpr(pattern = "gdalinfo", raw_GDAL[i])[[1]][1] - 1)
      # put the existing GISBASE directory, version number  and installation type in a data frame
      data.frame(binDir = root_dir,stringsAsFactors = FALSE)
    }) # end lapply
    # bind the df lines
    gdalInstallations <- do.call("rbind", gdalInstallations)
    for (i in 1:nrow(gdalInstallations)){
      gdal_bin[[i]]<- as.data.frame(grep(Sys.glob(file.path(R.utils::getAbsolutePath(gdalInstallations[[1]][i]),"gdal*")),pattern = "\\.(py)$", value=TRUE, invert=TRUE))
      names(gdal_bin[[i]])<-"gdal_bin"
      gdal_py[[i]]<-as.data.frame(grep(Sys.glob(file.path(R.utils::getAbsolutePath(gdalInstallations[[1]][i]),"gdal*")),pattern = "\\.(py)$", value=TRUE))
      names(gdal_py[[i]])<-"gdal_py"
    }
  } else {
    if(!quiet) cat("Did not find any valid GDAL installation at mount point",MP)
    return(gdalInstallations <- FALSE)}
  gdal1$gdalInstallations<-gdalInstallations
  gdal1$bin<- gdal_bin
  gdal1$py<- gdal_py
  return(gdal1)
}

#'@title Search recursivly existing 'GDAL binaries' installation(s) at a given drive/mountpoint 
#'@name findGDAL
#'@description  Provides an  list of valid 'GDAL' installation(s) 
#'on your 'Windows' system. There is a major difference between osgeo4W and 
#'stand_alone installations. The functions trys to find all valid 
#'installations by analysing the calling batch scripts.
#'@param searchLocation drive letter to be searched, for Windows systems default
#' is \code{C:}, for Linux systems default is \code{/usr}.
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@return A dataframe with the 'GDAL' root folder(s),  and command line executable(s)
#'@author Chris Reudenbach
#'@export findGDAL
#'
#'@examples
#' \dontrun{
#' # find recursively all existing 'GDAL' installations folders starting 
#' # at the default search location
#' findGDAL()
#' }
findGDAL <- function(searchLocation = "default",
                     quiet=TRUE) {
  
  if (Sys.info()["sysname"] == "Windows") {
    if (searchLocation=="default") searchLocation <- "C:"
    if (grepl(paste0(LETTERS, ":", collapse="|"), searchLocation))
      link = link2GI::searchGDALW(DL = searchLocation,                     
                                  quiet=TRUE)  
    else stop("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon" )
  } else {
    if (searchLocation=="default") searchLocation <- "/usr"
    if (grepl(searchLocation,pattern = ":"))  stop("You are running Linux - please choose a suitable searchLocation argument" )
    else link = link2GI::searchGDALX(MP = searchLocation,
                                     quiet=TRUE)
  } 
  return(link)
}

getrowGDALVer<- function (paths){
  #tmp<-c()
  scmd = ifelse(Sys.info()["sysname"]=="Windows", "gdalinfo.exe", "gdalinfo")
  sep = ifelse(Sys.info()["sysname"]=="Windows", "\\", "/")
  highestVer<-"1.4.0"
  for (i in 1:nrow(paths)){
    
    ret<- system(paste0(paste0(shQuote(paths$binDir[i]),sep,scmd)," --version"),intern = TRUE)
    
    tmp<-  strsplit(x = ret ,split = "GDAL ")[[1]][2]
    tmp2<- strsplit(x = tmp,split = ", released ")[[1]][1]
    highestVer <- max(tmp2,highestVer)
    pathI <- i
  }
  return (pathI)
}


# getGDALVer<- function (paths){
#   sep = ifelse(Sys.info()["sysname"]=="Windows", "\\", "/")
#   scmd = ifelse(Sys.info()["sysname"]=="Windows", "gdalinfo.exe", "gdalinfo")
#   tmp<-  strsplit(x = system(paste0(paste0(shQuote(paths$binDir[i]),sep,scmd)," --version"),intern = TRUE),split = "GDAL ")[[1]][2]
#   gdalVersion<- strsplit(x = tmp,split = ", released ")[[1]][1]
#   return (gdalVersion)
# }
