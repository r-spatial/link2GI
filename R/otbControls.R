#'@title  Usually for internally usage, initializes and set up  access to the 'OTB' command line interface
#'@name setenvOTB
#'@description  Initializes and set up  access to the 'OTB' command line interface
#'  
#'@param bin_OTB  string contains the path to the 'OTB' binaries
#'@param root_OTB string contains the full string to the root folder
#'  containing the 'OTB' installation'
#'@return Adds 'OTB' pathes to the environment and creates the variable global string variable \code{otbCmd}, that contains the path to the 'OTB' binaries.
#'@keywords internal
#'  
#'@examples
#' \dontrun{
#'## example for the most common default OSGeo4W64 installation of OTB
#'setenvOTB(bin_OTB = 'C:\\OSGeo4W64\\bin\\',
#'           root_OTB = 'C:\\OSGeo4W64')
#'}
setenvOTB <- function(bin_OTB = NULL, root_OTB = NULL) {
  # (R) set pathes of otb modules and binaries depending on OS
  if (Sys.info()["sysname"] == "Windows") {
    if (!exists("GiEnv"))
      GiEnv <- new.env(parent = globalenv())
    # makGlobalVar('otbPath', bin_OTB)
    add2Path(bin_OTB)
    Sys.setenv(OSGEO4W_ROOT = root_OTB)
    Sys.setenv(GEOTIFF_CSV = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\share\\epsg_csv"), envir = GiEnv)
  }
  # else { makGlobalVar('otbPath', '(usr/bin/') }
  return(bin_OTB)
}
#'@title Search recursively for valid 'OTB' installation(s) on a 'Windows' OS
#'@name searchOTBW
#'@description  Search for valid 'OTB' installations on a 'Windows' OS
#'@param DL drive letter default is \code{C:/}
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@return A dataframe with the 'OTB' root folder(s) the version name(s) and the installation type(s).
#'@author Chris Reudenbach
#'@export searchOTBW
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' # get all valid OTB installation folders and params
#' searchOTBW()
#' }
searchOTBW <- function(DL = "default", quiet = TRUE) {
  DL <- bf_wpath(DL)
  if (Sys.info()["sysname"] == "Windows") {
    if (!exists("GiEnv"))
      GiEnv <- new.env(parent = globalenv())
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name recursive
    # dir for otb*.bat returns all version of otb bat files
    if (!quiet)
      cat("\nsearching for Orfeo Toolbox installations - this may take a while\n")
    if (!quiet)
      cat("For providing the path manually see ?searchOTBW \n")
    options(show.error.messages = FALSE)
    options(warn = -1)
    # switch backslash to slash and expand path to full path
    raw_OTB <- try(system(paste0("cmd.exe /c WHERE /R ", DL, " ", "otbcli.bat"), intern = TRUE))
    # raw_OTB <- try(system(paste0('cmd.exe',' /c dir /B /S ',DL,'\\','otbcli.bat'),intern=TRUE))
    if (identical(raw_OTB, character(0)))
      raw_OTB <- "File not found"
    if (grepl(raw_OTB[1], pattern = "File not found") | grepl(raw_OTB[1], pattern = "Datei nicht gefunden") | grepl(raw_OTB[1],
                                                                                                                    pattern = "INFORMATION:") | grepl(raw_OTB[1], pattern = "FEHLER:") | grepl(raw_OTB[1], pattern = "ERROR:")) {
      class(raw_OTB) <- c("try-error", class(raw_OTB))
      message("::: NO OTB installation found at: '", DL, "'")
      message("::: NOTE: Links or symbolic links like 'C:/Documents' are searched...")
      stop()
    }
    options(show.error.messages = TRUE)
    options(warn = 0)
    if (class(raw_OTB)[1] != "try-error") {
      # trys to identify valid otb installations and their version numbers
      otbInstallations <- lapply(seq(length(raw_OTB)), function(i) {
        # convert codetable according to cmd.exe using type
        batchfile_lines <- raw_OTB[i]
        installerType <- ""
        # if the the tag 'OSGEO4W64' exists set installation_type
        if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), raw_OTB[i], value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("OSGeo4W64", collapse = "|"), raw_OTB[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "otbcli.bat", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          installerType <- "osgeo4w64OTB"
        } else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), raw_OTB[i], value = TRUE))) > 0) {
          # if the the tag 'OSGEO4W' exists set installation_type
          root_dir <- unique(grep(paste("OSGeo4W", collapse = "|"), raw_OTB[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "otbcli.bat", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          installerType <- "osgeo4wOTB"
        } else if (length(unique(grep(paste("QGIS", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          # if the the tag 'QGIS' exists set installation_type
          root_dir <- unique(grep(paste("QGIS", collapse = "|"), raw_OTB[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "otbcli.bat", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          installerType <- "qgisOTB"
        } else if (length(unique(grep(paste("OTB", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          # if the the tag 'OTB-' exists set installation_type
          root_dir <- unique(grep(paste("OTB", collapse = "|"), raw_OTB[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "otbcli.bat", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          installerType <- "OTB"
        }
        # put the existing binary root directory in a data frame data.frame(binDir = root_dir, baseDir =
        # installDir, installation_type = installerType, stringsAsFactors = FALSE)
        data.frame(binDir = root_dir, baseDir = installDir, otbCmd = paste0(root_dir, "otbcli"), stringsAsFactors = FALSE)
      })  # end lapply
      # bind the df lines
      otbInstallations <- do.call("rbind", otbInstallations)
    } else {
      if (!quiet)
        cat("Did not find any valid OTB installation at mount point", DL)
      return(otbInstallations <- FALSE)
    }
  } else {
    otbInstallations <- NULL
    cat("Sorry no Windows system...")
  }
  return(otbInstallations)
}
#'@title Search recursively for valid 'OTB' installation(s) on a 'Linux' OS
#'@name searchOTBX
#'@description  Search for valid 'OTB' installations on a 'Linux' OS
#'@param MP default mount point is the home directory '~' (as suggested by the OTB team) 
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@return A dataframe with the 'OTB' root folder(s) the version name(s) and the installation type(s).
#'@author Chris Reudenbach
#'@export searchOTBX
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' # get all valid OTB installation folders and params
#' searchOTBX()
#' }
searchOTBX <- function(MP = "default", quiet = TRUE) {
  if (MP == "default")
    MP <- "/usr/bin"
  if (!exists("GiEnv"))
    GiEnv <- new.env(parent = globalenv())
  # trys to find a osgeo4w installation at the mounting point disk returns root directory and version name recursive
  # dir for otb*.bat returns all version of otb bat files
  if (!quiet)
    cat("\nsearching for Orfeo Toolbox installations - this may take a while\n")
  if (!quiet)
    cat("For providing the path manually see ?searchOTBX \n")
  raw_OTB <- options(show.error.messages = FALSE)
  options(warn = -1)
  raw_OTB <- try(system2("find", paste(MP, " ! -readable -prune -o -type f -executable -iname 'otbcli' -print"), stdout = TRUE))
  if (identical(raw_OTB, character(0))) {
    # raw_OTB <- 'File not found' if (grepl(raw_OTB,pattern = 'File not found') | grepl(raw_OTB,pattern = 'Datei
    # nicht gefunden')) {
    class(raw_OTB) <- c("try-error", class(raw_OTB))
  }
  options(show.error.messages = TRUE)
  options(warn = 0)
  if (class(raw_OTB)[1] != "try-error") {
    # if (!grepl(MP,raw_OTB)) stop('\n At ',MP,' no OTB installation found') trys to identify valid otb
    # installations and their version numbers
    otbInstallations <- lapply(seq(length(raw_OTB)), function(i) {
      # TODO strip version from OTB /usr/bin/otbcli_BandMath -version 'This is the BandMath application, version
      # 6.0.0'
      # if the the tag 'OSGEO4W64' exists set installation_type
      root_dir <- substr(raw_OTB[i], 1, gregexpr(pattern = "otbcli", raw_OTB[i])[[1]][1] - 1)
      # put the existing GISBASE directory, version number and installation type in a data frame
      data.frame(binDir = root_dir, otbCmd = paste0(root_dir, "otbcli"), stringsAsFactors = FALSE)
    })  # end lapply
    # bind the df lines
    otbInstallations <- do.call("rbind", otbInstallations)
  } else {
    if (!quiet)
      cat("Did not find any valid OTB installation at mount point", MP)
    return(otbInstallations <- FALSE)
  }
  return(otbInstallations)
}
#'@title Search recursivly existing 'Orfeo Toolbox' installation(s) at a given drive/mountpoint 
#'@name findOTB
#'@description  Provides an  list of valid 'OTB' installation(s) 
#'on your 'Windows' system. There is a major difference between osgeo4W and 
#'stand_alone installations. The functions trys to find all valid 
#'installations by analysing the calling batch scripts.
#'@param searchLocation drive letter to be searched, for Windows systems default is \code{C:/}, for Linux systems default is \code{/usr/bin}.
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@return A dataframe with the 'OTB' root folder(s),  and command line executable(s)
#'@author Chris Reudenbach
#'@export findOTB
#'
#'@examples
#' \dontrun{
#' # find recursively all existing 'Orfeo Toolbox' installations folders starting 
#' # at the default search location
#' findOTB()
#' }
findOTB <- function(searchLocation = "default", quiet = TRUE) {
  if (Sys.info()["sysname"] == "Windows") {
    if (searchLocation == "default")
      searchLocation <- "C:/" else searchLocation <- normalizePath(searchLocation)
      if (grepl(paste0(LETTERS, ":", collapse = "|"), substr(toupper(searchLocation), start = 1, stop = 2)))
        link <- link2GI::searchOTBW(DL = searchLocation, quiet = TRUE) else stop("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon")
  } else {
    if (searchLocation == "default")
      searchLocation <- "/usr/bin/"
    if (grepl(searchLocation, pattern = ":"))
      stop("You are running Linux - please choose a suitable searchLocation argument") else link <- link2GI::searchOTBX(MP = searchLocation, quiet = TRUE)
  }
  return(link)
}
getrowotbVer <- function(paths) {
  scmd <- ifelse(Sys.info()["sysname"] == "Windows", "otbcli_LocalStatisticExtraction.bat", "otbcli_LocalStatisticExtraction")
  # sep = ifelse(Sys.info()['sysname']=='Windows', '\\', '/')
  oldversion <- "0.0.0"
  ver <- 1
  for (i in 1:length(paths)) {
    if (file.exists(paste0(paths[i], "../VERSION")))
      tmp <- strsplit(grep("OTB Version", readLines(paste0(paths[i], "../VERSION")), value = TRUE), "OTB Version: ")[[1]][2] else if (grepl("OTB-", paths[i]))
        tmp <- substr(strsplit(paths[i], "OTB-")[[1]][2], start = 1, stop = 5)
      # highestVer <- max(tmp,highestVer)
      if (!is.na(tmp))
        if (oldversion < tmp) {
          ver <- i
          oldversion <- tmp
        }
  }
  return(ver)
}
getotbVer <- function(paths) {
  scmd <- ifelse(Sys.info()["sysname"] == "Windows", "otbcli_LocalStatisticExtraction.bat ", "otbcli_LocalStatisticExtraction ")
  sep <- ifelse(Sys.info()["sysname"] == "Windows", "\\", "/")
  otbVersion <- strsplit(x = system(paste0(paste0(shQuote(paths), sep, scmd), " -version"), intern = FALSE), split = " version ")[[1]][2]
  otbVersion <- strsplit(x = otbVersion, split = "version ")[[1]][2]
  return(otbVersion)
}