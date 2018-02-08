
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

#'@title Usually for internally usage get 'GRASS GIS' and \code{rgrass7} parameters on 'Windows' OS
#'@name WGparam
#'@description Initialize the enviroment variables on a 'Windows' OS for using 
#'  'GRASS GIS' via \link{rgrass7}
#'@details The concept is very straightforward but for an all days usage pretty
#'  helpful. You need to provide a \link{raster} or a \link{sp} object. The derived properties are used to initialize a temporary but static
#'  \href{https://CRAN.R-project.org/package=rgrass7}{rgrass7} environment. During the rsession you will have full access to
#'  GRASS7 both via the wrapper package as well as the command line. WGparam initializes the usage of GRASS7.
#'@param DL raster or sp object
#'@param set_default_GRASS7 default = NULL forces a full search for 'GRASS GIS' binaries. You may
#'  alternatively provide a vector containing pathes and keywords. c("C:/OSGeo4W64","grass-7.0.5","osgeo4W") is valid for a typical osgeo4w installation.
#'@param ver_select if TRUE you must interactivley selcect between alternative installations 
#'@export WGparam
#'  
#'@examples
#' \dontrun{
#' # automatic retrieval of valid 'GRASS GIS' environment settings 
#' # if more than one is found the user has to choose.
#' WGparam()
#' 
#' # typical stand_alone installation
#' WGparam(c("C:/Program Files/GRASS GIS 7.0.5","GRASS GIS 7.0.5","NSIS"))
#' 
#' # typical OSGeo4W64 installation
#' WGparam(c("C:/OSGeo4W64","grass-7.0.5","osgeo4W"))
#' }

WGparam <- function(set_default_GRASS7=NULL, DL="C:", ver_select = FALSE){
  
  # (R) set pathes  of 'GRASS' binaries depending on 'WINDOWS'
  if (is.null(set_default_GRASS7)) {
    
    # if no path is provided  we have to search
    params_GRASS <- searchGRASSW(DL = DL)
    
    # if just one valid installation was found take it
    if (nrow(params_GRASS) == 1) {  
      gisbase_GRASS <- setenv_GRASS4W(root_GRASS = params_GRASS$instDir[[1]],
                                      grass_version = params_GRASS$version[[1]], 
                                      installation_type = params_GRASS$installation_type[[1]] )
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(params_GRASS) > 1 & ver_select) {
      cat("You have more than one valid GRASS GIS version\n")
      print(params_GRASS)
      cat("\n")
      ver <- as.numeric(readline(prompt = "Please choose one:  "))
      gisbase_GRASS <- normalizePath(setenv_GRASS4W(root_GRASS = params_GRASS$instDir[[ver]],
                                    grass_version = params_GRASS$version[[ver]], 
                                    installation_type = params_GRASS$installation_type[[ver]] ),winslash = "/")
    } else if (nrow(params_GRASS) > 1 & !ver_select) {  
      gisbase_GRASS <- setenv_GRASS4W(root_GRASS = params_GRASS$instDir[[1]],
                                      grass_version = params_GRASS$version[[1]], 
                                      installation_type = params_GRASS$installation_type[[1]] )
      
      # if more than one valid installation was found you have to choose 
    }
    
    # if a set_default_GRASS7 was provided take this 
  } else {
    gisbase_GRASS <- setenv_GRASS4W(root_GRASS = set_default_GRASS7[1],
                                    grass_version = set_default_GRASS7[2], 
                                    installation_type = set_default_GRASS7[3])  
  }
  return(gisbase_GRASS)
}



#'@title Search recursivly valid 'GRASS GIS' installation(s) on a given 'Windows' drive 
#'@name searchGRASSW
#'@title Search for valid OSGeo4W 'GRASS GIS' installation(s) on a given 'Windows' drive 
#'@description  Provides an  list of valid 'GRASS GIS' installation(s) on your 'Windows' system. There is a major difference between osgeo4W and stand_alone installations. The functions trys to find all valid installations by analysing the calling batch scripts.
#'@param DL drive letter to be searched, default is "C:"
#'@return A dataframe with the 'GRASS GIS' root folder(s), version name(s) and installation type code(s)
#'@author Chris Reudenbach
#'@export searchGRASSW
#'@keywords internal
#'@examples
#' \dontrun{
#' # get all valid 'GRASS GIS' installation folders and params at "C:"
#' searchGRASSW()
#' }

searchGRASSW <- function(DL = "C:"){
  if (DL=="default") DL <- "C:"
  
  # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
  # recursive dir for grass*.bat returns all version of grass bat files
  cat("\nsearching for GRASS installations - this may take a while\n")
  cat("For providing the path manually see ?searchGRASSW \n")
  raw_GRASS <- system(paste0("cmd.exe /c dir /B /S ", DL, "\\grass*.bat"), intern = T)
  
  # trys to identify valid grass installation(s) & version number(s)
  installations_GRASS <- lapply(seq(length(raw_GRASS)), function(i){
    # convert codetable according to cmd.exe using type
    batchfile_lines <- system(paste0("cmd.exe /c TYPE \"", raw_GRASS[i], "\""), 
                             ignore.stdout = TRUE, intern = T)
    osgeo4w <- FALSE
    stand_alone <- FALSE
    root_dir <- ''
    
    # if the the tag "OSGEO4W" exists set installation_type
    if (length(unique(grep(paste("OSGEO4W", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
      osgeo4w <- TRUE
      stand_alone <- FALSE
    }
    # if the the tag "NSIS installer" exists set installation_type
    if (length(unique(grep(paste("NSIS installer", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
      osgeo4w <- FALSE
      stand_alone <- TRUE
    }
    
    ### if installation_type is osgeo4w
    if (osgeo4w) {
      # grep line with root directory and extract the substring defining GISBASE
      root_dir <- unique(grep(paste("SET OSGEO4W_ROOT=", collapse = "|"), batchfile_lines, value = TRUE))
      if (length(root_dir) > 0) root_dir <- substr(root_dir, gregexpr(pattern = "=", root_dir)[[1]][1] + 1, nchar(root_dir))
      
      # grep line with the version name and extract it
      ver_char <- unique(grep(paste("\\benv.bat\\b", collapse = "|"), batchfile_lines,value = TRUE))
      if (length(root_dir) > 0) {
        ver_char <- substr(ver_char, gregexpr(pattern = "\\grass-", ver_char)[[1]][1], nchar(ver_char))
        ver_char <- substr(ver_char, 1, gregexpr(pattern = "\\\\", ver_char)[[1]][1] - 1)
      }
      installerType <- "osgeo4W"
    }
    
    ### if installatationtype is stand_alone
    if (stand_alone) {
      # grep line containing GISBASE and extract the substring 
      root_dir <- unique(grep(paste("set GISBASE=", collapse = "|"), batchfile_lines, value = TRUE))
      if (length(root_dir) > 0) root_dir <- substr(root_dir, gregexpr(pattern = "=", root_dir)[[1]][1] + 1, nchar(root_dir))
      ver_char <- root_dir
      if (length(root_dir) > 0) {
        ver_char <- substr(ver_char, gregexpr(pattern = "GRASS", ver_char)[[1]][1], nchar(ver_char))
      }
      installerType <- "NSIS"
    }
    
    # check if the the folder really exists
    if (length(root_dir) > 0) {
      if (!file.exists(file.path(root_dir))) {
        exist <- FALSE
      } else {
        exist <- TRUE
      } 
    } else {
      exist <- FALSE
    }
    
    # put the existing GISBASE directory, version number  and installation type in a data frame
    if (length(root_dir) > 0 & exist) {
      data.frame(instDir = root_dir, version = ver_char, installation_type = installerType,stringsAsFactors = FALSE)
    }
  }) # end lapply
  
  # bind the df lines
  installations_GRASS <- do.call("rbind", installations_GRASS)
  
  return(installations_GRASS)
}

#'@title Usually for internally usage, get 'GRASS GIS' and \code{rgrass7} parameters on 'Linux' OS
#'@name XGparam
#'@description Initialize and set up \link{rgrass7}  for 'Linux'
#'@details During the rsession you will have full access to GRASS7 GIS via the \link{rgrass7} wrappe. Additionally you may use also use the API calls of GRASS7 via the command line.
#'@param set_default_GRASS7 default = NULL will force a search for 'GRASS GIS' You may provide a valid combination as c("C:/OSGeo4W64","grass-7.0.5","osgeo4w")
#'@param MP mount point to be searched. default is "usr"
#'@param ver_select if TRUE you must interactivley selcect between alternative installations
#'@export XGparam
#'
#'@examples
#' \dontrun{
#' # automatic retrieval of the GRASS7 enviroment settings
#' getparams_GRASS7X()
#' 
#' # typical stand_alone installation
#' XGparam("/usr/bin/grass72")
#' 
#' # typical user defined installation (compiled sources)
#' XGparam("/usr/local/bin/grass72")
#' }

XGparam <- function(set_default_GRASS7=NULL, MP = "/usr",ver_select = FALSE){
  
  # (R) set pathes  of 'GRASS' binaries depending on 'Windows' OS
  if (is.null(set_default_GRASS7)) {
    
    # if no path is provided  we have to search
    params_GRASS <- searchGRASSX(MP = MP)
    
    # if just one valid installation was found take it
    if (nrow(params_GRASS) == 1) {  
      gisbase_GRASS <- params_GRASS$instDir
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(params_GRASS) > 1 & ver_select ) {
      cat("You have more than one valid GRASS version\n")
      print(params_GRASS)
      cat("\n")
      ver <- as.numeric(readline(prompt = "Please choose one:  "))
      gisbase_GRASS <- params_GRASS$instDir[[ver]]
    }
    
    # if a set_default_GRASS7 was provided take this 
  } else {
    gisbase_GRASS <- set_default_GRASS7
  }
  return(gisbase_GRASS)
}

#'@title Search recursivly valid 'GRASS GIS' installation(s) at a given 'Linux' mount point
#'@name searchGRASSX
#'@description Search for valid 'GRASS GIS' installations at a given 'Linux' mount point
#'@param MP default is /usr
#'@return A dataframe containing 'GRASS GIS' binary folder(s), version name(s) and installation type code(s)
#'@author Chris Reudenbach
#'@export searchGRASSX
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' # get all valid 'GRASS GIS' installation folders in the /usr directory (typical location)
#' searchGRASSX("/usr")
#' 
#' # get all valid 'GRASS GIS' installation folders in the home directory
#' searchGRASSX("~/")
#' }

searchGRASSX <- function(MP = "/usr"){
  if (MP=="default") MP <- "/usr"
  raw_GRASS <- system2("find", paste(MP," ! -readable -prune -o -type f -executable -iname 'grass??' -print"),stdout = TRUE)
  if (length(raw_GRASS) > 0) {
    installations_GRASS <- lapply(seq(length(raw_GRASS)), function(i){
      # grep line containing GISBASE and extract the substring 
      root_dir <- try(grep(readLines(raw_GRASS),pattern = 'gisbase = "',value = TRUE),silent = TRUE)
      if(!class(root_dir) == "try-error") {
      root_dir <- substr(root_dir, gregexpr(pattern = '"', root_dir)[[1]][1] + 1, nchar(root_dir) - 1)
      ver_char <- grep(readLines(raw_GRASS),pattern = 'grass_version = "',value = TRUE)
      ver_char <- substr(ver_char, gregexpr(pattern = '"', ver_char)[[1]][1] + 1, nchar(ver_char) - 1)
      cmd <- grep(readLines(raw_GRASS),pattern = 'cmd_name = "',value = TRUE)
      cmd <- substr(cmd, gregexpr(pattern = '"', cmd)[[1]][1] + 1, nchar(cmd) - 1)
      
      # put it in data frame
      data.frame(instDir = root_dir, version = ver_char, cmd = cmd , stringsAsFactors = FALSE)
      }
    }) # end lapply
    
    # bind the df lines
    installations_GRASS <- do.call("rbind", installations_GRASS)
    return(installations_GRASS)
  } else {
    warning(paste("Did not find any valid GRASS installation at mount point",MP))
    return(installations_GRASS <- NULL)
  }
}



#'@title Create valid 'GRASS GIS 7.xx' rsession environment settings
#'@name setenv_GRASS4W
#'@description  Initializes and set up  access to 'GRASS GIS 7.xx' via the \link{rgrass7} wrapper or command line packages
#'@param root_GRASS  grass root directory i.e. "C:\\OSGEO4~1",
#'@param grass_version grass version name i.e. "grass-7.0.5"
#'@param installation_type two options "osgeo4w" as installed by the 'OSGeo4W'-installer and "NSIS" that is typical for a stand_alone installtion of 'GRASS GIS'.
#'@param jpgmem jpeg2000 memory allocation size. Default is 1000000
#'@return Set all necessary environment variables and additionally returns the GISBASE directory as string.
#'@author Chris Reudenbach
#'@export setenv_GRASS4W
#'
#'@examples
#' \dontrun{
#' # get all valid 'GRASS GIS' installation folders and params
#' grassParam<- setenv_GRASS4W()
#' }

setenv_GRASS4W <- function(root_GRASS="C:\\OSGEO4~1",
                          grass_version = "grass-7.0.5",
                          installation_type = "osgeo4W",
                          jpgmem = 1000000) {
  if (Sys.info()["sysname"] == "Windows") {
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv())  
  #.GRASS_CACHE <- new.env(FALSE parent=globalenv())
  if (installation_type == "osgeo4W" || installation_type == "OSGeo4W64") {
    Sys.setenv(OSGEO4W_ROOT = root_GRASS)
    # define GISBASE
    gisbase_GRASS <- paste0(root_GRASS,"\\apps\\grass\\",grass_version)
    Sys.setenv(GISBASE = gisbase_GRASS,envir = GiEnv)
    assign("SYS", "WinNat", envir = GiEnv)
    assign("addEXE", ".exe", envir = GiEnv)
    assign("WN_bat", "", envir = GiEnv)
    assign("legacyExec", "windows", envir = GiEnv)
    
    
    Sys.setenv(GRASS_PYTHON = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\bin\\python.exe"),envir = GiEnv)
    Sys.setenv(PYTHONHOME = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\apps\\Python27"),envir = GiEnv)
    Sys.setenv(PYTHONPATH = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\apps\\grass\\",grass_version,"\\etc\\python"),envir = GiEnv)
    Sys.setenv(GRASS_PROJSHARE = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\proj"),envir = GiEnv)
    Sys.setenv(PROJ_LIB = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\proj"),envir = GiEnv)
    Sys.setenv(GDAL_DATA = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\gdal"),envir = GiEnv)
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = GiEnv)
    Sys.setenv(FONTCONFIG_FILE = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\etc\\fonts.conf"),envir = GiEnv)
    Sys.setenv(JPEGMEM = jpgmem,envir = GiEnv)
    Sys.setenv(FONTCONFIG_FILE = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\bin\\gdalplugins"),envir = GiEnv)
    Sys.setenv(GISRC = paste(Sys.getenv("HOME"), "\\.grassrc7",  sep = ""),envir = GiEnv)
    
    # set path variable
    Sys.setenv(PATH = paste0(gisbase_GRASS,";",
                           root_GRASS,"\\apps\\Python27\\lib\\site-packages\\numpy\\core",";",
                           root_GRASS,"\\apps\\grass\\",grass_version,"\\bin",";",
                           root_GRASS,"\\apps\\grass\\",grass_version,"\\lib",";",
                           root_GRASS,"\\apps\\grass\\",grass_version,"\\etc",";",
                           root_GRASS,"\\apps\\grass\\",grass_version,"\\etc\\python",";",
                           root_GRASS,"\\apps\\Python27\\Scripts",";",
                           root_GRASS,"\\bin",";",
                           root_GRASS,"\\apps",";",
                           paste0(Sys.getenv("WINDIR"),"/WBem"),";",
                           Sys.getenv("PATH")),envir = GiEnv)
    
    # get list of all tools
    system(paste0(root_GRASS,"/bin/o-help.bat"))
    
  } 
  # for the NSIS windows installer versions
  else {
    
    Sys.setenv(GRASS_ROOT = root_GRASS)
    # define GISBASE
    gisbase_GRASS <- normalizePath(root_GRASS)
    Sys.setenv(GISBASE = gisbase_GRASS,envir = GiEnv)
    assign("SYS", "WinNat", envir = GiEnv)
    assign("addEXE", ".exe", envir = GiEnv)
    assign("WN_bat", "", envir = GiEnv)
    assign("legacyExec", "windows", envir = GiEnv)
    
    
    Sys.setenv(GRASS_PYTHON = paste0(Sys.getenv("GRASS_ROOT"),"\\bin\\python.exe"),envir = GiEnv)
    Sys.setenv(PYTHONHOME = paste0(Sys.getenv("GRASS_ROOT"),"\\Python27"),envir = GiEnv)
    Sys.setenv(PYTHONPATH = paste0(Sys.getenv("GRASS_ROOT"),"\\etc\\python"),envir = GiEnv)
    Sys.setenv(GRASS_PROJSHARE = paste0(Sys.getenv("GRASS_ROOT"),"\\share\\proj"),envir = GiEnv)
    Sys.setenv(PROJ_LIB = paste0(Sys.getenv("GRASS_ROOT"),"\\share\\proj"),envir = GiEnv)
    Sys.setenv(GDAL_DATA = paste0(Sys.getenv("GRASS_ROOT"),"\\share\\gdal"),envir = GiEnv)
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("GRASS_ROOT"),"\\share\\epsg_csv"),envir = GiEnv)
    Sys.setenv(FONTCONFIG_FILE = paste0(Sys.getenv("GRASS_ROOT"),"\\etc\\fonts.conf"),envir = GiEnv)
    Sys.setenv(JPEGMEM = jpgmem,envir = GiEnv)
    Sys.setenv(FONTCONFIG_FILE = paste0(Sys.getenv("GRASS_ROOT"),"\\bin\\gdalplugins"),envir = GiEnv)
    Sys.setenv(GISRC = paste(Sys.getenv("HOME"), "\\.grassrc7",  sep = ""),envir = GiEnv)
    
    # set path variable OSGeo4W64/apps/grass/grass-7.2.2/bin
    Sys.setenv(PATH = paste0(gisbase_GRASS,";",
                           root_GRASS,"\\Python27\\lib\\site-packages\\numpy\\core",";",
                           root_GRASS,"\\bin",";",
                           root_GRASS,"\\extrabin",";",
                           root_GRASS,"\\lib",";",
                           root_GRASS,"\\etc",";",
                           root_GRASS,"\\etc\\python",";",
                           root_GRASS,"\\Scripts",";",
                           root_GRASS,";",
                           paste0(Sys.getenv("WINDIR"),"/WBem"),";",
                           Sys.getenv("PATH")),envir = GiEnv)
    
  }
  } else {gisbase_GRASS <- "Sorry no Windows System..." }
  return(gisbase_GRASS)
}


#'@title  Initializes and set up  access to the 'OTB' command line interface
#'@name setenv_OTB
#'@description  Initializes and set up  access to the 'OTB' command line interface
#'  
#'@param bin_OTB  string contains the path to the 'OTB' binaries
#'@param root_OTB string contains the full string to the root folder
#'  containing the 'OTB' installation'
#'@return Adds 'OTB' pathes to the enviroment and creates the variable global string variable \code{otbCmd}, that contains the path to the 'OTB' binaries.
#'@export setenv_OTB
#'  
#'@examples
#' \dontrun{
#'## example for the most common default OSGeo4W64 installation of OTB
#'setenv_OTB(bin_OTB="C:\\OSGeo4W64\\bin\\",root_OTB="C:\\OSGeo4W64")
#'}

setenv_OTB <- function(bin_OTB = NULL, root_OTB = NULL){
  # check if running on a HRZMR Pool PC
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
  if (substr(Sys.getenv("COMPUTERNAME"),1,5) == "PCRZP") {
    bin_OTB <- checkPCDomain("otb")   
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = GiEnv)
  } else {
    # (R) set pathes  of otb modules and binaries depending on OS  
    if (Sys.info()["sysname"] == "Windows") {
      #makGlobalVar("otbPath", bin_OTB)
      add2Path(bin_OTB)
      Sys.setenv(OSGEO4W_ROOT = root_OTB)
      Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = GiEnv)
    }
    #else {
    #  makGlobalVar("otbPath", "(usr/bin/")
    #}
  }
  return(bin_OTB)
}

#'@title Search recursively for valid 'OTB' installation(s) on a 'Windows' OS
#'@name searchOTBW
#'@description  Search for valid 'OTB' installations on a 'Windows' OS
#'@param DL drive letter default is "C:"
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@return A dataframe with the 'OTB' root folder(s) the version name(s) and the installation type(s).
#'@author Chris Reudenbach
#'@export searchOTBW
#'@keywords internal
#'
#'@examples
#' \dontrun{
#'#### Examples how to use RSAGA and OTB bindings from R
#'
#' # get all valid OTB installation folders and params
#' searchOTBW()
#' }

searchOTBW <- function(DL = "C:",
                       quiet = TRUE) {
  if (Sys.info()["sysname"] == "Windows") {
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
  if (substr(Sys.getenv("COMPUTERNAME"),1,5) == "PCRZP") {
    defaultOtb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    otbInstallations <- data.frame(instDir = shQuote("C:\\Program Files\\QGIS 2.14\\bin"), installation_type = "osgeo4wOTB",stringsAsFactors = FALSE)
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = GiEnv)
  } else {
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for otb*.bat returns all version of otb bat files
    if (!quiet) cat("\nsearching for Orfeo Toolbox installations - this may take a while\n")
    if (!quiet) cat("For providing the path manually see ?searchOTBW \n")
    raw_OTB <- system(paste0("cmd.exe"," /c dir /B /S ",DL,"\\","otbcli.bat"),intern=TRUE)
    if (!grepl(DL,raw_OTB)) stop("\n At ",DL," no OTB installation found")
    # trys to identify valid otb installations and their version numbers
    otbInstallations <- lapply(seq(length(raw_OTB)), function(i){
      # convert codetable according to cmd.exe using type
      batchfile_lines <- raw_OTB[i]
      installerType <- ""
      # if the the tag "OSGEO4W64" exists set installation_type
      if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), raw_OTB[i], value = TRUE))) > 0) {
        root_dir <- unique(grep(paste("OSGeo4W64", collapse = "|"), raw_OTB[i], value = TRUE))
        root_dir <- substr(root_dir,1, gregexpr(pattern = "otbcli.bat", root_dir)[[1]][1] - 1)
        installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        installerType <- "osgeo4w64OTB"
      }    
      
      # if the the tag "OSGEO4W" exists set installation_type
      else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), raw_OTB[i], value = TRUE))) > 0) {
        root_dir <- unique(grep(paste("OSGeo4W", collapse = "|"), raw_OTB[i], value = TRUE))
        root_dir <- substr(root_dir,1, gregexpr(pattern = "otbcli.bat", root_dir)[[1]][1] - 1)
        installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        installerType <- "osgeo4wOTB"
      }
      # if the the tag "QGIS" exists set installation_type
      else if (length(unique(grep(paste("QGIS", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
        root_dir <- unique(grep(paste("QGIS", collapse = "|"), raw_OTB[i], value = TRUE))
        root_dir <- substr(root_dir,1, gregexpr(pattern = "otbcli.bat", root_dir)[[1]][1] - 1)
        installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        installerType <- "qgisOTB"
      }
      # if the the tag "OTB-" exists set installation_type
      else if (length(unique(grep(paste("OTB-", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
        root_dir <- unique(grep(paste("OTB-", collapse = "|"), raw_OTB[i], value = TRUE))
        root_dir <- substr(root_dir,1, gregexpr(pattern = "otbcli.bat", root_dir)[[1]][1] - 1)
        installDir <- substr(root_dir,1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        installerType <- "OTB"
      }
      # put the existing GISBASE directory, version number  and installation type in a data frame
      data.frame(binDir = root_dir, baseDir = installDir, installation_type = installerType, stringsAsFactors = FALSE)
    }) # end lapply
    # bind the df lines
    otbInstallations <- do.call("rbind", otbInstallations)
  }
  } else {otbInstallations <- "Sorry no Windows system..." }
  return(otbInstallations)
}



#'@title Checks if x is of type raster,sf or sp
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
  if (class(obj)[1] %in% c("RasterLayer", "RasterStack",
                        "RasterBrick", "Satellite",
                        "SpatialGridDataFrame",
                        "SpatialPixelsDataFrame")) {"rst"} 
  else if (class(obj)[1] %in% c("SpatialPointsDataFrame", "SpatialPoints",
                             "SpatialPolygonsDataFrame",
                             "SpatialPolygons",
                             "SpatialLinesDataFrame",
                             "SpatialLines",
                             "sf")) {"vec"}
  else {"paramList"}
}

#'@title Checks if running on a specified computer domain
#'@name checkPCDomain
#'@description  Checks if the computer belongs to the Marburg Universitys computer pools
#'@param cliCode code of the sofware currently \code{saga} and \code{otb} are supported
#'@param prefixPC contains the an arbitrary part of the computer name. It always starts with the first letter.
#'@author CR
#'@keywords internal
#'@examples
#' \dontrun{
#' # add path
#' checkPCDomain("saga",prefixPC="PCRZP")
#' }
#'@export checkPCDomain
checkPCDomain <- function(cliCode=NULL, prefixPC="PCRZP") {
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
  if (substr(Sys.getenv("COMPUTERNAME"),1,nchar(prefixPC)) == substr(prefixPC,1,nchar(prefixPC))) {
    if (cliCode == "saga") { 
      defaultSAGA <- shQuote(c("C:\\Program Files\\QGIS 2.14\\apps\\saga","C:\\Program Files\\QGIS 2.14\\apps\\saga\\modules"))
      return(defaultSAGA)
    } else {
      return(defaultSAGA = NULL)  
    }
  } else if (cliCode == "otb") {
    defaultOtb <- shQuote("C:\\Program Files\\QGIS 2.14\\bin")
    root_OTB <- shQuote("C:\\Program Files\\QGIS 2.14")
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"),"\\share\\epsg_csv"),envir = GiEnv)
    otbInstallations <- data.frame(instDir = shQuote("C:\\Program Files\\QGIS 2.14\\bin"), installation_type = "osgeo4wOTB", stringsAsFactors = FALSE)
    return(otbInstallations)
  }
  
  
}

#'@title Adds a defined variable and value to the global search path
#'@name add2Path
#'@description  Adds a variable to the global search path of the current environment
#'@param newPath the path that is added
#'@author Chris Reudenbach
#'@examples
#' \dontrun{
#' # add path
#' addPath("pathtosomewhere")
#' }
#'@export add2Path
#'
add2Path <- function(newPath) {
  exist <- FALSE
  if (Sys.info()["sysname"] == "Windows") {
    del <- ";"  
  } else {
    del <- ":"  
  } 
  p <- Sys.getenv("PATH")
  if (substr(p, 1,nchar(newPath)) == newPath) {
    exist <- TRUE
  }
  # if not exist append path to systempath
  if (!exist) {
    Sys.setenv(PATH = paste0(newPath,del,Sys.getenv("PATH")))
  }
}

#'@title Generates a variable with a certain value in the R environment
#'@name makGlobalVar
#' @description  Generates a variable with a certain value in the R environment
#' @param name character string name of the variable
#' @param value character string value of the variable
#' @keywords internal
#'@export makGlobalVar 
#'@examples
#' \dontrun{
#'
#' # creates the global var \code{pathToData} with the value \code{~/home/data}
#' makGlobalVar("pathToData","~/home/data") 
#' 
#' }
#' 
makGlobalVar <- function(name,value) {
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv())  
  if (exists(name, envir = GiEnv)) {
    #warning(paste0("The variable '", name,"' already exist in .GlobalEnv"))
    assign(name, value, envir = GiEnv, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global GiEnv\n")
  } else {
    assign(name, value, envir = GiEnv, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global GiEnv\n")
  } 
}

checkGisdbase <- function(x = NULL , gisdbase = NULL, location = NULL, gisdbase_exist = FALSE, obj_name = NULL ) {
if (gisdbase_exist)
  linkGRASS7(gisdbase = gisdbase, location = location, gisdbase_exist = TRUE)  
else 
  linkGRASS7(x, gisdbase = gisdbase, location = location)  
path <- Sys.getenv("GISDBASE")
sq_name <- gsub(tolower(paste0(obj_name,".sqlite")),pattern = "\\-",replacement = "_")
return(list(gisbase_path = path, sqlite = sq_name))
}


#'@title Search recursivly existing 'GRASS GIS' installation(s) at a given drive/mountpoint 
#'@name findGRASS
#'@description  Provides an  list of valid 'GRASS GIS' installation(s) 
#'on your 'Windows' system. There is a major difference between osgeo4W and 
#'stand_alone installations. The functions trys to find all valid 
#'installations by analysing the calling batch scripts.
#'@param searchLocation drive letter to be searched, for Windows systems default
#' is \code{C:}, for Linux systems default is \code{/usr}.
#'@return A dataframe with the 'GRASS GIS' root folder(s), version name(s) and 
#'installation type code(s)
#'@author Chris Reudenbach
#'@export findGRASS
#'
#'@examples
#' \dontrun{
#' # find recursively all existing 'GRASS GIS' installation folders starting 
#' # at the default search location
#' findGRASS()
#' }
findGRASS <- function(searchLocation = "default") {

if (Sys.info()["sysname"] == "Windows") {
  link = link2GI::searchGRASSW(DL = searchLocation)  
} else {
  link = link2GI::searchGRASSX(MP = searchLocation)
}
  return(link)
}