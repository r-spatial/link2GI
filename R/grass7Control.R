
#'@title Usually for internally usage get 'GRASS GIS' and \code{rgrass7} parameters on 'Windows' OS
#'@name paramGRASSw
#'@description Initialize the enviroment variables on a 'Windows' OS for using 
#'  'GRASS GIS' via \link{rgrass7}
#'@details The concept is very straightforward but for an all days usage pretty
#'  helpful. You need to provide a \link{raster} or a \link{sp} object. The derived properties are used to initialize a temporary but static
#'  \href{https://CRAN.R-project.org/package=rgrass7}{rgrass7} environment. During the rsession you will have full access to
#'  GRASS7 both via the wrapper package as well as the command line. paramGRASSw initializes the usage of GRASS7.
#'@param DL raster or sp object
#'@param ver_select boolean default is FALSE. If there is more than one 'SAGA GIS' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'SAGA GIS' version 
#'@param set_default_GRASS7 default = NULL forces a full search for 'GRASS GIS' binaries. You may
#'  alternatively provide a vector containing pathes and keywords. c("C:/OSGeo4W64","grass-7.0.5","osgeo4W") is valid for a typical osgeo4w installation.
#'  
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@export paramGRASSw
#'  
#'@examples
#' \dontrun{
#' # automatic retrieval of valid 'GRASS GIS' environment settings 
#' # if more than one is found the user has to choose.
#' paramGRASSw()
#' 
#' # typical OSGeo4W64 installation
#' paramGRASSw(c("C:/OSGeo4W64","grass-7.0.5","osgeo4W"))
#' }

paramGRASSw <- function(set_default_GRASS7=NULL, 
                        DL="C:", 
                        ver_select =FALSE,
                        quiet = TRUE){
  if (ver_select =='T') ver_select <- TRUE
  if (ver_select == "F" && !is.numeric(ver_select)) ver_select <- FALSE
  # (R) set pathes  of 'GRASS' binaries depending on 'WINDOWS'
  if (is.null(set_default_GRASS7)) {
    if (DL=="default" || is.null(DL)) DL <- "C:"
    # if no path is provided  we have to search
    params_GRASS <- findGRASS(searchLocation = DL,
                              quiet = quiet)
    
    # if just one valid installation was found take it
    if (nrow(params_GRASS) == 1) {  
      gisbase_GRASS <- setenvGRASSw(root_GRASS = params_GRASS$instDir[[1]],
                                    grass_version = params_GRASS$version[[1]], 
                                    installation_type = params_GRASS$installation_type[[1]],
                                    quiet = quiet )
      grass_version = params_GRASS$version[[1]]
      installation_type = params_GRASS$installation_type[[1]]
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(params_GRASS) > 1 & ver_select) {
      cat("You have more than one valid GRASS GIS version\n")
      print(params_GRASS)
      cat("\n")
      ver <- as.numeric(readline(prompt = "Please select one:  "))
      gisbase_GRASS <- normalizePath(setenvGRASSw(root_GRASS = params_GRASS$instDir[[ver]],
                                                  grass_version = params_GRASS$version[[ver]], 
                                                  installation_type = params_GRASS$installation_type[[ver]],
                                                  quiet = quiet  ),
                                     winslash = "/")
      grass_version = params_GRASS$version[[ver]]
      installation_type = params_GRASS$installation_type[[ver]]
    }   else if (nrow(params_GRASS) > 1 & is.numeric(ver_select) & ver_select > 0 ) {
      cat("You have more than one valid GRASS GIS version\n")
      print(params_GRASS)
      cat("You have selected version: ",ver_select,"\n")
      
      gisbase_GRASS <- normalizePath(setenvGRASSw(root_GRASS = params_GRASS$instDir[[ver_select]],
                                                  grass_version = params_GRASS$version[[ver_select]], 
                                                  installation_type = params_GRASS$installation_type[[ver_select]],
                                                  quiet = quiet  ),
                                     winslash = "/")
      grass_version = params_GRASS$version[[ver_select]]
      installation_type = params_GRASS$installation_type[[ver_select]]
    } else if (nrow(params_GRASS) > 1 & !ver_select) {  
      cat("You have more than one valid GRASS version installed!\n")
      cat("The latest installed version (",which(params_GRASS$version == max(params_GRASS$version)),")has been selected \n")
      
      gisbase_GRASS <- setenvGRASSw(root_GRASS = params_GRASS$instDir[[which(params_GRASS$version == max(params_GRASS$version))]],
                                    grass_version = params_GRASS$version[[which(params_GRASS$version == max(params_GRASS$version))]], 
                                    installation_type = params_GRASS$installation_type[[which(params_GRASS$version == max(params_GRASS$version))]] ,
                                    quiet=quiet)
      grass_version = params_GRASS$version[[which(params_GRASS$version == max(params_GRASS$version))]]
      installation_type = params_GRASS$installation_type[[which(params_GRASS$version == max(params_GRASS$version))]]
      
      
    }
    
    # if a set_default_GRASS7 was provided take this 
  } else {
    gisbase_GRASS <- setenvGRASSw(root_GRASS = set_default_GRASS7[1],
                                  grass_version = set_default_GRASS7[2], 
                                  installation_type = set_default_GRASS7[3],
                                  quiet =quiet)  
    grass_version = set_default_GRASS7[2]
    installation_type = set_default_GRASS7[3]
    params_GRASS<- data.frame(instDir = gisbase_GRASS, 
                              version = grass_version, 
                              installation_type = installation_type,
                              stringsAsFactors = FALSE)
    
  }
  grass<-list()
  grass$gisbase_GRASS<-gisbase_GRASS
  grass$version <- grass_version
  grass$type <- installation_type
  grass$installed <- params_GRASS
  return(grass)
}



#'@title Search recursivly valid 'GRASS GIS' installation(s) on a given 'Windows' drive 
#'@name searchGRASSW
#'@title Search for valid OSGeo4W 'GRASS GIS' installation(s) on a given 'Windows' drive 
#'@description  Provides an  list of valid 'GRASS GIS' installation(s) on your 'Windows' system. There is a major difference between osgeo4W and stand_alone installations. The functions trys to find all valid installations by analysing the calling batch scripts.
#'@param DL drive letter to be searched, default is "C:"
#'@param quiet boolean  switch for supressing console messages default is TRUEs
#'@return A dataframe with the 'GRASS GIS' root folder(s), version name(s) and installation type code(s)
#'@author Chris Reudenbach
#'@export searchGRASSW
#'@keywords internal
#'@examples
#' \dontrun{
#' # get all valid 'GRASS GIS' installation folders and params at "C:"
#' searchGRASSW()
#' }

searchGRASSW <- function(DL = "C:",
                         quiet =TRUE){
  
  if (DL=="default") DL <- "C:"
  
  # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
  # recursive dir for grass*.bat returns all version of grass bat files
  if (!quiet) cat("\nsearching for GRASS installations - this may take a while\n")
  if (!quiet) cat("For providing the path manually see ?searchGRASSW \n")
  raw_GRASS <- try(system(paste0("cmd.exe /c dir /B /S ", DL, "\\grass*.bat"), intern = T))
  options(warn=-1)
  if(grepl(raw_GRASS,pattern = "Datei nicht gefunden") || grepl(raw_GRASS,pattern = "File not found")) 
    {stop("\n ********* No GRASS installation found ************\n")}
  options(warn=0)
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
#'@name paramGRASSx
#'@description Initialize and set up \link{rgrass7}  for 'Linux'
#'@details During the rsession you will have full access to GRASS7 GIS via the \link{rgrass7} wrappe. Additionally you may use also use the API calls of GRASS7 via the command line.
#'@param set_default_GRASS7 default = NULL will force a search for 'GRASS GIS' You may provide a valid combination as c("C:/OSGeo4W64","grass-7.0.5","osgeo4w")
#'@param MP mount point to be searched. default is "usr"
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@param ver_select if TRUE you must interactivley selcect between alternative installations
#'@export paramGRASSx
#'
#'@examples
#' \dontrun{
#' # automatic retrieval of the GRASS7 enviroment settings
#' paramsGRASSx()
#' 
#' # typical stand_alone installation
#' paramGRASSx("/usr/bin/grass72")
#' 
#' # typical user defined installation (compiled sources)
#' paramGRASSx("/usr/local/bin/grass72")
#' }

paramGRASSx <- function(set_default_GRASS7=NULL, 
                        MP = "/usr",
                        ver_select = FALSE, 
                        quiet =TRUE){
  if (ver_select =='T') ver_select <- TRUE
  if (ver_select == "F" && !is.numeric(ver_select)) ver_select <- FALSE
  # (R) set pathes  of 'GRASS' binaries depending on 'Windows' OS
  if (is.null(set_default_GRASS7)) {
    
    # if no path is provided  we have to search
    params_GRASS <- findGRASS(searchLocation = MP)
    
    # if just one valid installation was found take it
    if (nrow(params_GRASS) == 1) {  
      gisbase_GRASS <- params_GRASS$instDir
      
      # if more than one valid installation was found you have to choose 
    } else if (nrow(params_GRASS) > 1 & is.numeric(ver_select) & ver_select > 0 ) {
      cat("You have more than one valid GRASS version installed!\n")
      print(params_GRASS)
      cat("You have selected version: ",ver_select,"\n")
      gisbase_GRASS <- params_GRASS$instDir[[ver_select]]
    }
    
    else if (nrow(params_GRASS) > 1 & !ver_select ) {
      cat("You have more than one valid GRASS version installed!\n")
      cat("The latest installed version (",which(params_GRASS$version == max(params_GRASS$version)),")has been selected \n")
      print(params_GRASS)
      cat("\n")
     gisbase_GRASS <- params_GRASS$instDir[[which(params_GRASS$version == max(params_GRASS$version))]]
    }
    else if (nrow(params_GRASS) > 1 & ver_select ) {
      cat("You have more than one valid GRASS version installed!\n")
      print(params_GRASS)
      cat("\n")
      ver <- as.numeric(readline(prompt = "Please select one:  "))
      gisbase_GRASS <- params_GRASS$instDir[[ver]]
    }
    
    # if a set_default_GRASS7 was provided take this 
  } else {
    gisbase_GRASS <- set_default_GRASS7
  }
  grass<-list()
  grass$gisbase_GRASS<-gisbase_GRASS
  grass$installed <- params_GRASS
  return(grass)
  
  #return(gisbase_GRASS)
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
  #cat(raw_GRASS)
  if (length(raw_GRASS) > 0) {
    installations_GRASS <- lapply(seq(length(raw_GRASS)), function(i){
      # grep line containing GISBASE and extract the substring 
      root_dir <- try(grep(readLines(raw_GRASS[[i]]),pattern = 'gisbase = "',value = TRUE),silent = TRUE)
      if(!class(root_dir) == "try-error" && length(root_dir) > 0) {
        #print(root_dir)
        root_dir <- substr(root_dir, gregexpr(pattern = '"', root_dir)[[1]][1] + 1, nchar(root_dir) - 1)
        ver_char <- grep(readLines(raw_GRASS[[i]]),pattern = 'grass_version = "',value = TRUE)
        ver_char <- substr(ver_char, gregexpr(pattern = '"', ver_char)[[1]][1] + 1, nchar(ver_char) - 1)
        cmd <- grep(readLines(raw_GRASS[[i]]),pattern = 'cmd_name = "',value = TRUE)
        cmd <- substr(cmd, gregexpr(pattern = '"', cmd)[[1]][1] + 1, nchar(cmd) - 1)
        
        # put it in data frame
        data.frame(instDir = root_dir, version = ver_char, installation_type = cmd , stringsAsFactors = FALSE)
        
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



#'@title Usually for internally usage, create valid 'GRASS GIS 7.xx' rsession environment settings according to the selected GRASS GIS 7.x and Windows Version
#'@name setenvGRASSw
#'@description  Initializes and set up  access to 'GRASS GIS 7.xx' via the \link{rgrass7} wrapper or command line packages. Set and returns all necessary environment variables and additionally returns the GISBASE directory as string.
#'@param root_GRASS  grass root directory i.e. "C:\\OSGEO4~1",
#'@param grass_version grass version name i.e. "grass-7.0.5"
#'@param installation_type two options "osgeo4w" as installed by the 'OSGeo4W'-installer and "NSIS" that is typical for a stand_alone installtion of 'GRASS GIS'.
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@param jpgmem jpeg2000 memory allocation size. Default is 1000000
#'@author Chris Reudenbach
#'@export setenvGRASSw
#'
#'@examples
#' \dontrun{
#' # set choosen'GRASS GIS' installation folders 
#' setenvGRASSw(root_GRASS = "C:\\PROGRA~1\\QGIS2~1.18",
#'              grass_version =  "grass-7.2.1",
#'              installation_type =  "osgeo4W")
#' }

setenvGRASSw <- function(root_GRASS=NULL,
                         grass_version = NULL,
                         installation_type = NULL,
                         jpgmem = 1000000,
                         quiet = TRUE) {
  if (Sys.info()["sysname"] == "Windows") {
    if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv())  
    #.GRASS_CACHE <- new.env(FALSE parent=globalenv())
    if (is.null(root_GRASS) || is.null(grass_version) || is.null(installation_type)) {
     stop("Please run findGRASS first and provide valid arguments")
    }
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
      if (!quiet) system(paste0(root_GRASS,"/bin/o-help.bat"))
      
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
#'@param searchLocation drive letter to be searched, for Windows systems default For Windows Systems it is mandatory to use Capitel letters with colon only
#' is \code{C:}, for Linux systems default is \code{/usr}.
#'@param ver_select boolean default is FALSE. If there is more than one 'SAGA GIS' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'SAGA GIS' version 
#'@param quiet boolean  switch for supressing console messages default is TRUE
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
findGRASS <- function(searchLocation = "default",
                      ver_select = FALSE,
                      quiet=TRUE) {
  
  if (Sys.info()["sysname"] == "Windows") {
    if (searchLocation=="default") searchLocation <- "C:"
    if (searchLocation %in% paste0(LETTERS,":") )
    link = link2GI::searchGRASSW(DL = searchLocation)  
    else stop("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon" )
  } else {
    if (searchLocation=="default") searchLocation <- "/usr"
    if (grepl(searchLocation,pattern = ":"))  stop("You are running Linux - please choose a suitable searchLocation argument" )
    else link = link2GI::searchGRASSX(MP = searchLocation)
  }
  return(link)
}
