#'@title Searches recursively for existing 'Windows' 'SAGA GIS' installation(s)
#'@name searchSAGAX
#'@description  Search for valid 'GRASS GIS' installations at a given 'Linux' mount point
#'@param MP default mount point is \code{/usr/bin}
#'@param quiet Boolean  switch for suppressing console messages default is TRUE
#'@return A data frame containing the 'SAGA GIS' root folder(s), the version name(s) and the installation type(s)
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
searchSAGAX <- function(MP = "/usr/bin", quiet = TRUE) {
  if (MP == "default")
    MP <- "/usr/bin"
  if (Sys.info()["sysname"] == "Linux")
  {
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
    # recursive dir for saga_cmd.exe returns all version of otb bat files
    if (!quiet)
      cat("\nsearching for SAGA GIS installations - this may take a while\n")
    if (!quiet)
      cat("For providing the path manually see ?searchSAGAX \n")
    # find saga and SAGA lib path(es)
    options(show.error.messages = FALSE)
    options(warn = -1)
    rawSAGA <- try(system2("find", paste(MP, " ! -readable -prune -o -type f -executable -iname 'saga_cmd' -print"),
                           stdout = TRUE))
    rawSAGALib <- try(system2("find", paste(MP, " ! -readable -prune -o -type f  -iname 'libio_gdal.so' -print"),
                              stdout = TRUE))
    if (grepl(rawSAGA, pattern = "File not found") | grepl(rawSAGA, pattern = "Datei nicht gefunden")) {
      class(rawSAGA) <- c("try-error", class(rawSAGA))
    }
    options(show.error.messages = TRUE)
    options(warn = 0)
    if (!methods::is(rawSAGA, "try-error")) {
      # split the search returns of existing SAGA GIS installation(s
      sagaPath <- lapply(seq(length(rawSAGA)), function(i) {
        root_dir <- substr(rawSAGA[i], 1, gregexpr(pattern = "saga_cmd", rawSAGA[i])[[1]][1] - 1)
        moduleDir <- substr(rawSAGALib[i], 1, gregexpr(pattern = "libio_gdal.so", rawSAGALib[i])[[1]][1] - 1)
        # put the result in a data frame
        data.frame(binDir = gsub("\\\\$", "", root_dir), moduleDir = gsub("\\\\$", "", moduleDir), stringsAsFactors = FALSE)
      })  # end lapply
      # bind df
      sagaPath <- do.call("rbind", sagaPath)
    } else {
      if (!quiet)
        cat(paste("Did not find any valid SAGA installation at mount point", MP))
      return(sagaPath <- FALSE)
    }
  }  # end of sysname = Windows
  else {
    sagaPath <- NULL
    cat("No SAGA GIS found.\n")
  }
  return(sagaPath)
}
#'@title Searches recursively for existing 'Windows' 'SAGA GIS' installation(s)
#'@name searchSAGAW
#'@description  Searches recursively for existing 'SAGA GIS' installation(s) on a given 'Windows' drive 
#'@param DL drive letter default is \code{C:/}
#'@param quiet boolean  switch for suppressing messages default is TRUE
#'@return A data frame containing the 'SAGA GIS' root folder(s), the version name(s) and the installation type(s)
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
searchSAGAW <- function(DL = "C:/", quiet = TRUE) {
  DL <- bf_wpath(DL)
  if (Sys.info()["sysname"] == "Windows") {
    sagaPath <- NULL  #checkPCDomain('saga')  
    if (is.null(sagaPath))
    {
      # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name
      # recursive dir for saga_cmd.exe returns all version of otb bat files
      if (!quiet)
        cat("\nsearching for SAGA GIS installations - this may take a while\n")
      if (!quiet)
        cat("For providing the path manually see ?searchSAGAW \n")
      cmd <- "saga_cmd.exe"
      # for a straightforward use of a correct codetable using the cmd command 'dir' is used
      options(show.error.messages = FALSE)
      options(warn = -1)
      # Windows defaults paths windows.defaults.paths = c('C:/Progra~1/SAGA', 'C:/Progra~2/SAGA',
      # 'C:/Progra~1/SAGA-GIS', 'C:/Progra~2/SAGA-GIS', 'C:/SAGA-GIS', 'C:/OSGeo4W/apps/saga',
      # 'C:/OSGeo4W64/apps/saga', 'C:/OSGeo4W64/apps/saga-ltr') # Check if one path is valid for (pa in
      # windows.defaults.paths) { if (file.exists(file.path(pa, cmd))) { rawSAGA = pa break } }
      # If no default path is correct, search for SAGA GIS on entire drive if (is.null(rawSAGA)) {
      # cat('SAGA command line program not found in the following default paths:\n',
      # paste0(windows.defaults.paths, collapse = '\n'), '\nSearch on the entire hard drive...\n', sep='')
      # Search starts in root directory path.list = list.files(path = 'C:/', pattern = 'saga_cmd.exe',
      # recursive = TRUE, full.names = TRUE)
      # rawSAGA <- try(system(paste0('cmd.exe /c dir /B /s ',DL,'\\',cmd),intern = TRUE))
      rawSAGA <- try(system(paste0("cmd.exe /c WHERE /R ", DL, " ", cmd), intern = TRUE))
      rawSAGA <- utils::shortPathName(rawSAGA)
      # Remove cmd name from path path.list = gsub(paste0('.{',nchar(cmd),'}$'), '', path.list)
      # Stop if no saga_cmd.exe is found
      if (length(rawSAGA) == 0) {
        print("No Saga installation found")
        return()
      }
      if (grepl(rawSAGA, pattern = "File not found")[1] | grepl(rawSAGA, pattern = "Datei nicht gefunden")[1]) {
        rawSAGA <- "message"
        class(rawSAGA) <- c("try-error", class(rawSAGA))
      }
      options(show.error.messages = TRUE)
      options(warn = 0)
      if (class(rawSAGA)[1] != "try-error") {
        # trys to identify valid SAGA GIS installation(s) & version number(s)
        sagaPath <- lapply(seq(length(rawSAGA)), function(i) {
          cmdfileLines <- rawSAGA[i]
          installerType <- ""
          if (substr(cmdfileLines, 1, 1) == "\\")
            rawSAGA[i] <- substr(cmdfileLines, 3, nchar(cmdfileLines))
          # if 'OSGeo4W64'
          if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), rawSAGA[i], value = TRUE))) > 0) {
            root_dir <- unique(grep(paste("OSGeo4W64", collapse = "|"), rawSAGA[i], value = TRUE))
            root_dir <- substr(root_dir, 1, gregexpr(pattern = "saga_cmd.exe", root_dir)[[1]][1] - 1)
            installDir <- root_dir
            installerType <- "osgeo4w64SAGA"
          } else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), rawSAGA[i], value = TRUE))) > 0) {
            # if 'OSGeo4W'
            root_dir <- unique(grep(paste("OSGeo4W", collapse = "|"), rawSAGA[i], value = TRUE))
            root_dir <- substr(root_dir, 1, gregexpr(pattern = "saga_cmd.exe", root_dir)[[1]][1] - 1)
            installerType <- "osgeo4wSAGA"
          } else if (length(unique(grep(paste("QGIS", collapse = "|"), rawSAGA[i], value = TRUE))) > 0) {
            # if 'QGIS'
            root_dir <- unique(grep(paste("QGIS", collapse = "|"), rawSAGA[i], value = TRUE))
            root_dir <- substr(root_dir, 1, gregexpr(pattern = "saga_cmd.exe", root_dir)[[1]][1] - 1)
            installerType <- "qgisSAGA"
          } else {
            root_dir <- substr(rawSAGA[i], 1, gregexpr(pattern = "saga_cmd.exe", rawSAGA[i])[[1]][1] - 1)
            installerType <- "soloSAGA"
          }
          if (file.exists(paste0(root_dir, "\\tools"))) {
            moduleDir <- paste0(root_dir, "tools")
          } else {
            moduleDir <- paste0(root_dir, "modules")
          }
          # put the result in a data frame
          data.frame(binDir = gsub("\\\\$", "", root_dir), moduleDir = gsub("\\\\$", "", moduleDir), installation_type = installerType,
                     stringsAsFactors = FALSE)
        })  # end lapply
        # bind df
        sagaPath <- do.call("rbind", sagaPath)
      } else {
        if (!quiet)
          cat(paste("Did not find any valid SAGA installation at mount point", DL))
        sagaPath <- FALSE
      }
    }  #  end of is.null(sagaPath)
  } else {
    # end of sysname = Windows
    sagaPath <- NULL
    cat("Sorry no Windows system...")
  }
  return(sagaPath)
}
#'@title Search recursively existing 'SAGA GIS' installation(s) at a given drive/mount point 
#'@name findSAGA
#'@description  Provides an  list of valid 'SAGA GIS' installation(s) 
#'on your 'Windows' system. There is a major difference between osgeo4W and 
#'stand_alone installations. The functions tries to find all valid 
#'installations by analyzing the calling batch scripts.
#'@param searchLocation drive letter to be searched, for Windows systems default
#' is \code{C:/}, for Linux systems default is \code{/usr/bin}.
#'@param quiet boolean  switch for suppressing console messages default is TRUE
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
findSAGA <- function(searchLocation = "default", quiet = TRUE) {
  if (Sys.info()["sysname"] == "Windows") {
    if (searchLocation == "default") {
      searchLocation <- "C:/"
    } else {
      searchLocation <- normalizePath(searchLocation)
    }
    if (grepl(paste0(LETTERS, ":", collapse = "|"), substr(toupper(searchLocation), start = 1, stop = 2))) {
      link <- link2GI::searchSAGAW(DL = searchLocation, quiet = quiet)
    } else {
      stop("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon")
    }
  } else {
    if (searchLocation == "default")
      searchLocation <- "/usr/bin"
    if (grepl(searchLocation, pattern = ":")) {
      stop("You are running Linux - please choose a suitable searchLocation argument")
    } else {
      link <- link2GI::searchSAGAX(MP = searchLocation, quiet = quiet)
    }
  }
  return(link)
}
# split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
split_path <- function(path) {
  if (dirname(path) %in% c(".", path))
    return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}
# getrowSagaVer<- function (paths){ #tmp<-c() scmd = ifelse(Sys.info()['sysname']=='Windows', 'saga_cmd.exe',
# 'saga_cmd') sep = ifelse(Sys.info()['sysname']=='Windows', '/', '/') highestVer<-'2.0.8' batfileFN= '' for (i in
# 1:nrow(paths)){ sp = strsplit(paths[i,1],'apps')[[1]][1] batfileFN= paste0(sp,'OSGeo4W.bat') test =
# system(paste0('cmd.exe /c ; ',paste0(batfileFN, ' ; ' ,gsub('\\\\','/',paths$binDir[i]),sep,scmd),' --version'))
# if(!identical(nchar(test), integer(0))){ tmp<- try(strsplit(x = system(paste0(paste0(batfileFN, ' ;
# ',paths$binDir[i],sep,scmd),' --version'),intern = TRUE),split = 'SAGA Version: ')[[1]][2],silent = TRUE) if
# (!inherits(tmp ,'try-error')) {highestVer <- max(tmp,highestVer) pathI <- i}} } return (pathI) }
getSagaVer <- function(paths) {
  sep <- ifelse(Sys.info()["sysname"] == "Windows", "\\", "/")
  scmd <- ifelse(Sys.info()["sysname"] == "Windows", "saga_cmd.exe", "saga_cmd")
  if (grepl(paths, pattern = "OSGeo")) {
    batfileFN <- "C:\\OSGeo4W\\OSGeo4W.bat"
    sagaVersion <- strsplit(x = system(paste0(paste0(batfileFN, " ; ", paths, sep, scmd), " --version"), intern = TRUE),
                            split = "SAGA Version: ")[[1]][2]
  } else {
    batfileFN <- ""
    sagaVersion <- strsplit(x = system(paste0(paste0(batfileFN, " ; ", paths, sep, scmd), " --version"), intern = TRUE),
                            split = "SAGA Version: ")[[1]][2]
  }
  # sagaVersion<- strsplit(x = system(paste0paths,sep,scmd),' --version'),intern = TRUE),split = 'SAGA Version:
  # ')[[1]][2]
  return(sagaVersion)
}