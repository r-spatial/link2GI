#'@title  Usually for internally usage, initializes and set up  access to the 'GDAL' command line interface
#'@name setenvGDAL
#'@description  Initializes and set up  access to the 'GDAL' command line interface
#'  
#'@param bin_GDAL  string contains the path to the 'GDAL' binaries
#'@return Adds 'GDAL' paths to the environment and creates the variable global string variable \code{gdalCmd}, that contains the path to the 'GDAL' binaries.
#'@keywords internal
#'  
#'@examples
#'
#' run = FALSE
#' if (run) {
#'## example for the most common default OSGeo4W64 installation of GDAL
#'setenvGDAL(bin_GDAL = 'C:/OSGeo4W64/bin/',
#'           root_GDAL = 'C:/OSGeo4W64')
#'}
setenvGDAL <- function(bin_GDAL = NULL) {
  # (R) set paths of gdal modules and binaries depending on OS
  if (Sys.info()["sysname"] == "Windows") {
    if (!exists("GiEnv"))
      GiEnv <- new.env(parent = globalenv())
    # makGlobalVar('gdalPath', bin_GDAL)
    add2Path(bin_GDAL)
    Sys.setenv(GDAL_DATA = bin_GDAL)
  } else {
    if (!exists("GiEnv"))
      GiEnv <- new.env(parent = globalenv())
    # makGlobalVar('gdalPath', bin_GDAL)
    add2Path((bin_GDAL))
    p <- system(paste0((bin_GDAL), "gdal-config --datadir"), intern = TRUE, ignore.stdout = FALSE, ignore.stderr = FALSE)
    # Sys.setenv(GDAL_DRIVER_PATH = root_GDAL)
    Sys.setenv(GDAL_DATA = p)
  }
  return(bin_GDAL)
}
#'@title Search recursively for valid 'GDAL' installation(s) on a 'Windows' OS
#'@name searchGDALW
#'@description  Search for valid 'GDAL' installations on a 'Windows' OS
#'@param DL drive letter default is 'C:/'
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@return A dataframe with the 'GDAL' root folder(s) the version name(s) and the installation type(s).
#'@author Chris Reudenbach
#'@export 
#'@keywords internal
#'
#'@examples
#'
#' run = FALSE
#' if (run) {
#' # get all valid GDAL installation folders and params
#' searchGDALW()
#' }
searchGDALW <- function(DL = "C:/", quiet = TRUE) {
  DL <- bf_wpath(DL)
  if (Sys.info()["sysname"] == "Windows") {
    if (!exists("GiEnv"))
      GiEnv <- new.env(parent = globalenv())
    # trys to find a osgeo4w installation on the whole C: disk returns root directory and version name recursive
    # dir for gdal*.bat returns all version of gdal bat files
    if (!quiet)
      cat("\nsearching for GDAL installations - this may take a while\n")
    if (!quiet)
      cat("For providing the path manually see ?searchGDALW \n")
    options(show.error.messages = FALSE)
    options(warn = -1)
    raw_GDAL <- try(system(paste0("cmd.exe /c WHERE /R ", DL, " ", "gdalinfo.exe"), intern = TRUE))
    if (identical(raw_GDAL, character(0)))
      raw_GDAL <- "File not found"
    if (any(grepl(raw_GDAL, pattern = "File not found")) | any(grepl(raw_GDAL, pattern = "Datei nicht gefunden"))) {
      class(raw_GDAL) <- c("try-error", class(raw_GDAL))
    }
    options(show.error.messages = TRUE)
    options(warn = 0)
    gdal1 <- gdal_py <- gdal_bin <- list()
    if (class(raw_GDAL)[1] != "try-error") {
      # if (!grepl(DL,raw_GDAL)) stop('\n At ',DL,' no GDAL installation found')
      # trys to identify valid gdal installations and their version numbers
      gdalInstallations <- lapply(seq(length(raw_GDAL)), function(i) {
        # convert codetable according to cmd.exe using type
        batchfile_lines <- raw_GDAL[i]
        installerType <- ""
        installDir <- ""
        root_dir <- ""
        # if the the tag 'OSGEO4W64' exists set installation_type
        if (length(unique(grep(paste("OSGeo4W64", collapse = "|"), raw_GDAL[i], value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("OSGeo4W64", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- "osgeo4w64"
          } else {
            installerType <- "unknown"
          }
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        } else if (length(unique(grep(paste("OSGeo4W", collapse = "|"), raw_GDAL[i], value = TRUE))) > 0) {
          # if the the tag 'OSGEO4W' exists set installation_type
          root_dir <- unique(grep(paste("OSGeo4W", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- "osgeo4w"
          } else {
            installerType <- "unknown"
          }
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        } else if (length(unique(grep(paste("QGIS", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          # if the the tag 'QGIS' exists set installation_type
          root_dir <- unique(grep(paste("QGIS", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- "qgis"
          } else {
            installerType <- "unknown"
          }
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        } else if (length(unique(grep(paste("GDAL", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          # if the the tag 'GDAL-' exists set installation_type
          root_dir <- unique(grep(paste("GDAL", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- "GDAL"
          } else {
            installerType <- "unknown"
          }
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
        } else if (length(unique(grep(paste("GRASS", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          # if the the tag 'GRASS-' exists set installation_type
          root_dir <- unique(grep(paste("GRASS", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          bundle <- substr(root_dir, gregexpr(pattern = "GRASS", root_dir)[[1]][1], gregexpr(pattern = "bin", root_dir)[[1]][1] -
                             7)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- bundle
          } else {
            installerType <- "unknown"
          }
        } else if (length(unique(grep(paste("OTB", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          # if the the tag 'OTB-' exists set installation_type
          root_dir <- unique(grep(paste("OTB", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- "OTB"
          } else {
            installerType <- "unknown"
          }
        } else if (length(unique(grep(paste("conda", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("conda", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- "Conda_Miniconda"
          } else {
            installerType <- "unknown"
          }
        } else if (length(unique(grep(paste("Progra~1", collapse = "|"), batchfile_lines, value = TRUE))) > 0) {
          root_dir <- unique(grep(paste("GRASS", collapse = "|"), raw_GDAL[i], value = TRUE))
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- "GRASS_standalone"
          } else {
            installerType <- "unknown"
          }
        } else {
          root_dir <- raw_GDAL[i]
          root_dir <- substr(root_dir, 1, gregexpr(pattern = "gdalinfo.exe", root_dir)[[1]][1] - 1)
          installDir <- substr(root_dir, 1, gregexpr(pattern = "bin", root_dir)[[1]][1] - 2)
          if (file.exists(file.path(root_dir, "gdalinfo.exe"))) {
            installerType <- "Miniconda"
          } else {
            installerType <- "unknown"
          }
        }
        # put the existing binary directory, version number and installation type in a data frame
        data.frame(binDir = root_dir, baseDir = installDir, installation_type = installerType, stringsAsFactors = FALSE)
      })  # end lapply
      # bind the df lines
      gdalInstallations <- do.call("rbind", gdalInstallations)
      for (i in 1:nrow(gdalInstallations)) {
        gdal_bin[[i]] <- as.data.frame(grep(Sys.glob(file.path(R.utils::getAbsolutePath(gdalInstallations[[1]][i]),
                                                               "gdal*")), pattern = "\\.(py)$", value = TRUE, invert = TRUE))
        names(gdal_bin[[i]]) <- "gdal_bin"
        gdal_py[[i]] <- as.data.frame(grep(Sys.glob(file.path(R.utils::getAbsolutePath(gdalInstallations[[1]][i]),
                                                              "gdal*")), pattern = "\\.(py)$", value = TRUE))
        names(gdal_py[[i]]) <- "gdal_py"
      }
    } else {
      if (!quiet)
        cat("Did not find any valid GDAL installation at mount point", DL)
      return(gdalInstallations <- FALSE)
    }
  } else {
    gdalInstallations <- NULL
    cat("Sorry no Windows system...")
  }
  gdalInstallations$binDir <- utils::shortPathName(gdalInstallations$binDir)
  gdalInstallations$baseDir <- utils::shortPathName(gdalInstallations$baseDir)
  df <- gdalInstallations[gdalInstallations$installation_type != "unknown", ]
  rownames(df) <- 1:nrow(df)
  gdal1$gdalInstallations <- df
  gdal1$bin <- gdal_bin[gdalInstallations$installation_type != "unknown"]
  gdal1$py <- gdal_py[gdalInstallations$installation_type != "unknown"]
  return(gdal1)
}
#' Search recursively for valid GDAL installation(s) on Linux/macOS
#'
#' Searches for an executable `gdalinfo` and returns a normalized installations
#' table plus best-effort lists of GDAL binaries (`gdal*`) and python tools (`*.py`)
#' found alongside the detected `gdalinfo`.
#'
#' This implementation is portable: it does NOT use GNU-only `find` primaries
#' such as `-readable`, and it uses `system2(..., args=...)` with proper token
#' separation (no shell parsing assumptions).
#'
#' @param MP Character. Search root. `"default"` expands to `c("~","/opt","/usr/local","/usr")`.
#'   You may also pass a single directory (e.g. `"/usr"`).
#' @param quiet Logical. If `TRUE`, suppress messages.
#'
#' @return A list with:
#' \describe{
#'   \item{gdalInstallations}{data.frame with columns `binDir`, `baseDir`, `installation_type`.}
#'   \item{bin}{list of data.frames (column `gdal_bin`) with detected GDAL binaries per installation.}
#'   \item{py}{list of data.frames (column `gdal_py`) with detected GDAL python tools per installation.}
#' }
#'
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' x <- searchGDALX()
#' x$gdalInstallations
#' }
searchGDALX <- function(MP = "default", quiet = TRUE) {
  
  # ---- 1) mountpoints / roots ----
  if (identical(MP, "default")) {
    MP <- c("~", "/opt", "/usr/local", "/usr")
  } else {
    MP <- as.character(MP)
  }
  
  MP <- path.expand(MP)
  MP <- MP[file.exists(MP)]
  
  if (!length(MP)) {
    if (!quiet) message("No valid search mount points.")
    return(list(
      gdalInstallations = data.frame(
        binDir = character(0),
        baseDir = character(0),
        installation_type = character(0),
        stringsAsFactors = FALSE
      ),
      bin = list(),
      py  = list()
    ))
  }
  
  if (!quiet) {
    cat("\nsearching for GDAL installations in:\n")
    cat(paste0(" - ", MP, collapse = "\n"), "\n")
  }
  
  # ---- helpers ----
  normp <- function(p) normalizePath(p, mustWork = FALSE)
  
  # Prefer `gdalinfo` (no extension) on Unix-like systems.
  # We search for executable files named gdalinfo (case-insensitive).
  find_gdalinfo <- function(root) {
    out <- suppressWarnings(try(system2(
      "find",
      args   = c(root, "-type", "f", "-executable", "-iname", "gdalinfo", "-print"),
      stdout = TRUE,
      stderr = TRUE
    ), silent = TRUE))
    
    if (inherits(out, "try-error") || !length(out)) return(character(0))
    
    # Remove common noise; keep real paths only.
    out <- out[nzchar(trimws(out))]
    out <- out[!grepl("Permission denied", out, fixed = TRUE)]
    unique(out)
  }
  
  # Given a gdalinfo path, derive binDir/baseDir and classify type.
  derive_install_row <- function(gdalinfo_path) {
    gdalinfo_path <- normp(gdalinfo_path)
    binDir <- normp(dirname(gdalinfo_path))
    baseDir <- normp(file.path(binDir, ".."))
    
    # Rough classification (best-effort)
    installation_type <- "system"
    lp <- tolower(gdalinfo_path)
    if (grepl("/osgeo4w", lp, fixed = TRUE)) installation_type <- "osgeo4w"
    if (grepl("/qgis",    lp, fixed = TRUE)) installation_type <- "qgis"
    if (grepl("/conda",   lp, fixed = TRUE) || grepl("/miniconda", lp, fixed = TRUE)) installation_type <- "conda"
    if (grepl("/grass",   lp, fixed = TRUE)) installation_type <- "grass"
    if (grepl("/otb",     lp, fixed = TRUE)) installation_type <- "otb"
    
    data.frame(
      binDir = binDir,
      baseDir = baseDir,
      installation_type = installation_type,
      stringsAsFactors = FALSE
    )
  }
  
  # List candidate tools in binDir
  list_bin_and_py <- function(binDir) {
    binDir <- normp(binDir)
    
    # "gdal*" excluding *.py
    gdal_bins <- character(0)
    gdal_pys  <- character(0)
    
    if (dir.exists(binDir)) {
      # Use Sys.glob to mimic your legacy behaviour but safely
      g <- Sys.glob(file.path(binDir, "gdal*"))
      if (length(g)) {
        g <- normp(g)
        gdal_pys  <- g[grepl("\\.py$", g, ignore.case = TRUE)]
        gdal_bins <- g[!grepl("\\.py$", g, ignore.case = TRUE)]
      }
    }
    
    list(
      bin = data.frame(gdal_bin = gdal_bins, stringsAsFactors = FALSE),
      py  = data.frame(gdal_py  = gdal_pys,  stringsAsFactors = FALSE)
    )
  }
  
  # ---- 2) collect gdalinfo hits ----
  hits <- unique(unlist(lapply(MP, find_gdalinfo), use.names = FALSE))
  
  if (!length(hits)) {
    if (!quiet) message("::: NO GDAL installation found in given locations.")
    return(list(
      gdalInstallations = data.frame(
        binDir = character(0),
        baseDir = character(0),
        installation_type = character(0),
        stringsAsFactors = FALSE
      ),
      bin = list(),
      py  = list()
    ))
  }
  
  # ---- 3) build normalized installations table ----
  inst_rows <- lapply(hits, derive_install_row)
  inst_df <- do.call(rbind, inst_rows)
  
  # De-duplicate by binDir (multiple gdalinfo in same bin)
  inst_df <- inst_df[!duplicated(inst_df$binDir), , drop = FALSE]
  rownames(inst_df) <- seq_len(nrow(inst_df))
  
  # ---- 4) collect gdal bin/py lists per installation ----
  gdal_bin <- vector("list", nrow(inst_df))
  gdal_py  <- vector("list", nrow(inst_df))
  
  for (i in seq_len(nrow(inst_df))) {
    x <- list_bin_and_py(inst_df$binDir[i])
    gdal_bin[[i]] <- x$bin
    gdal_py[[i]]  <- x$py
  }
  
  out <- list(
    gdalInstallations = inst_df,
    bin = gdal_bin,
    py  = gdal_py
  )
  
  if (!quiet) {
    message("::: Found ", nrow(inst_df), " GDAL installation(s).")
  }
  
  out
}


#'@title Search recursivly existing 'GDAL binaries' installation(s) at a given drive/mountpoint 
#'@name findGDAL
#'@description  Provides an  list of valid 'GDAL' installation(s) 
#'on your 'Windows' system. There is a major difference between osgeo4W and 
#'stand_alone installations. The functions trys to find all valid 
#'installations by analysing the calling batch scripts.
#'@param searchLocation drive letter to be searched, for Windows systems default
#' is \code{C:/}, for Linux systems default is \code{/usr/bin}.
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@return A dataframe with the 'GDAL' root folder(s),  and command line executable(s)
#'@author Chris Reudenbach
#'@export 
#'
#'@examples
#'
#' run = FALSE
#' if (run) {
#' # find recursively all existing 'GDAL' installations folders starting 
#' # at the default search location
#' findGDAL()
#' }
findGDAL <- function(searchLocation = "default", quiet = TRUE) {
  if (Sys.info()["sysname"] == "Windows") {
    if (searchLocation == "default") {
      searchLocation <- "C:/"
    } else {
      searchLocation <- normalizePath(searchLocation)
    }
    if (grepl(paste0(LETTERS, ":", collapse = "|"), substr(toupper(searchLocation), start = 1, stop = 2))) {
      link <- link2GI::searchGDALW(DL = searchLocation, quiet = TRUE)
    } else {
      stop("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon")
    }
  } else {
    if (searchLocation == "default")
      searchLocation <- "/usr/bin"
    if (grepl(searchLocation, pattern = ":")) {
      stop("You are running Linux - please choose a suitable searchLocation argument")
    } else {
      link <- link2GI::searchGDALX(MP = searchLocation, quiet = TRUE)
    }
  }
  return(link)
}
getrowGDALVer <- function(paths) {
  # tmp<-c()
  scmd <- ifelse(Sys.info()["sysname"] == "Windows", "gdalinfo.exe", "gdalinfo")
  sep <- ifelse(Sys.info()["sysname"] == "Windows", "\\", "/")
  highestVer <- "1.2.0"
  options(show.error.messages = FALSE)
  options(warn = -1)
  for (i in 1:nrow(paths)) {
    ret <- try(system(paste0(paste0(shQuote(paths$binDir[i]), sep, scmd), " --version"), intern = TRUE))
    if (substr(ret, 1, 4) == "GDAL" && length(ret) > 0) {
      tmp <- strsplit(x = ret, split = "GDAL ")[[1]][2]
      tmp2 <- strsplit(x = tmp, split = ", released ")[[1]][1]
      if (tmp2 > highestVer ){
        highestVer <- tmp2
        pathI <- i
      }
    }
  }
  options(show.error.messages = TRUE)
  options(warn = 0)
  return(pathI)
}
# getGDALVer<- function (paths){ sep = ifelse(Sys.info()['sysname']=='Windows', '\\', '/') scmd =
# ifelse(Sys.info()['sysname']=='Windows', 'gdalinfo.exe', 'gdalinfo') tmp<- strsplit(x =
# system(paste0(paste0(shQuote(paths$binDir[i]),sep,scmd),' --version'),intern = TRUE),split = 'GDAL ')[[1]][2]
# gdalVersion<- strsplit(x = tmp,split = ', released ')[[1]][1] return (gdalVersion) }
