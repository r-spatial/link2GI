.link2GI_cache <- new.env(parent = emptyenv())

.get_GRASS_CACHE <- function() {
  if (!exists(".GRASS_CACHE", envir = .link2GI_cache, inherits = FALSE)) {
    .link2GI_cache$.GRASS_CACHE <- new.env(parent = emptyenv())
  }
  .link2GI_cache$.GRASS_CACHE
}



#'@title Usually for internally usage, get 'GRASS GIS' and \code{rgrass} parameters on 'Linux' OS
#'@name paramGRASSx
#'@description Initialize and set up \code{rgrass}  for 'Linux'
#'@details During the rsession you will have full access to GRASS7 GIS via the \code{rgrass} wrapper. Additionally you may use also use the API calls of GRASS via the command line.
#'@param set_default_GRASS, default is NULL. will force a search for 'GRASS GIS' You may provide a valid combination as 
#'                                    c('/usr/lib/grass74','7.4.1','grass74')
#'@param MP, default is '/usr/bin'. mount point to be searched. 
#'@param quiet boolean, default is TRUE.  switch for suppressing console messages 
#'@param ver_select if TRUE you must interactively select between alternative installations
#'@keywords internal
#'
#' @examples
#' 
#' run = FALSE
#' if (run) {
#' # automatic retrieval of the GRASS environment settings
#' paramGRASSx()
#' 
#' 
#' # typical stand_alone installation
#' paramGRASSx('/usr/bin/grass72')
#' 
#' # typical user defined installation (compiled sources)
#' paramGRASSx('/usr/local/bin/grass72')
#' }
paramGRASSx <- function(set_default_GRASS = NULL, MP = "/usr/bin", ver_select = FALSE, quiet = TRUE) {
  
  if (ver_select == "T") ver_select <- TRUE
  if (ver_select == "F" && !is.numeric(ver_select)) ver_select <- FALSE
  
  if (Sys.info()["sysname"] == "Windows") {
    return(cat("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon"))
  }
  
  # If we know nothing about grass paths we have to search
  if (is.null(set_default_GRASS)) {
    params_GRASS <- findGRASS(searchLocation = MP, quiet = quiet)
  } else {
    params_GRASS <- rbind.data.frame(set_default_GRASS)
    names(params_GRASS) <- c("instDir", "version", "installation_type")
  }
  
  # robust "found" check
  if (isFALSE(params_GRASS) || !is.data.frame(params_GRASS) || nrow(params_GRASS) < 1) {
    return(list(exist = FALSE))
  }
  
  # choose the desired installation depending on ver_select options
  if (nrow(params_GRASS) == 1) {
    
    gisbase_GRASS <- as.character(params_GRASS$instDir[[1]])
    
  } else if (nrow(params_GRASS) > 1 && is.numeric(ver_select) &&
             (ver_select > 0 && ver_select <= nrow(params_GRASS))) {
    
    if (!quiet) {
      cat("You have more than one valid GRASS version installed!\n")
      print(params_GRASS)
      cat("Selected version is: ", ver_select, "\n")
    }
    gisbase_GRASS <- as.character(params_GRASS$instDir[[ver_select]])
    
  } else if (nrow(params_GRASS) > 1 && !isTRUE(ver_select)) {
    
    # if ver_select is FALSE take the one with the highest version number
    v <- suppressWarnings(numeric_version(params_GRASS$version))
    idx <- which(v == max(v))[1]
    
    if (!quiet) {
      cat("You have more than one valid GRASS version installed!\n")
      cat("The latest installed version (", idx, ") has been selected \n")
      print(params_GRASS)
      cat("\n")
    }
    gisbase_GRASS <- as.character(params_GRASS$instDir[[idx]])
    
  } else {
    
    # ver_select == TRUE: interactively select
    cat("You have more than one valid GRASS version installed!\n")
    print(params_GRASS)
    cat("\n")
    ver <- as.numeric(readline(prompt = "Please select one:  "))
    gisbase_GRASS <- as.character(params_GRASS$instDir[[ver]])
  }
  
  grass <- list()
  grass$gisbase_GRASS <- gisbase_GRASS
  grass$installed <- params_GRASS
  grass$exist <- TRUE
  
  return(grass)
}



#'@title Usually for internally usage get 'GRASS GIS' and \code{rgrass} parameters on 'Windows' OS
#'@name paramGRASSw
#'@description Initialize the enviroment variables on a 'Windows' OS for using 
#'  'GRASS GIS' via \code{rgrass}
#'@details The concept is very straightforward but for an all days usage pretty
#'  helpful. You need to provide a \code{terra} or a \code{sf} object. The derived properties are used to initialize a temporary but static
#'  \href{https://CRAN.R-project.org/package=rgrass}{rgrass} environment. During the rsession you will have full access to
#'  GRASS both via the wrapper package as well as the command line. paramGRASSw initializes the usage of GRASS.
#'@param DL character search location default = \code{C:}
#'@param ver_select boolean default is FALSE. If there is more than one 'SAGA GIS' installation and \code{ver_select} = TRUE the user can select interactively the preferred 'SAGA GIS' version 
#'@param set_default_GRASS default = NULL forces a full search for 'GRASS GIS' binaries. You may
#'  alternatively provide a vector containing paths and keywords. c('C:/OSGeo4W64','grass-7.0.5','osgeo4w') is valid for a typical osgeo4w installation.
#'  
#'@param quiet boolean  switch for supressing console messages default is TRUE
#'@keywords internal
#'  
#' @examples
#' 
#' run = FALSE
#' if (run) {
#' # automatic retrieval of valid 'GRASS GIS' environment settings 
#' # if more than one is found the user has to choose.
#' paramGRASSw()
#'
#' # typical OSGeo4W64 installation
#' paramGRASSw(c('C:/OSGeo4','grass7.8','osgeo4W'))
#' }
paramGRASSw <- function(set_default_GRASS = NULL, DL = "C:/", ver_select = FALSE, quiet = TRUE) {
  
  if (ver_select == "T") ver_select <- TRUE
  if (ver_select == "F" && !is.numeric(ver_select)) ver_select <- FALSE
  
  if (Sys.info()["sysname"] == "Linux") {
    return(cat("You are running Linux - please choose a suitable searchLocation argument"))
  }
  
  # (R) set paths of 'GRASS' binaries depending on 'WINDOWS'
  if (is.null(set_default_GRASS)) {
    if (DL == "default" || is.null(DL)) DL <- "C:/"
    params_GRASS <- findGRASS(searchLocation = DL, quiet = quiet)
  } else {
    params_GRASS <- rbind.data.frame(set_default_GRASS)
    names(params_GRASS) <- c("instDir", "version", "installation_type")
  }
  
  # --- robust success check ---
  if (isFALSE(params_GRASS) || !is.data.frame(params_GRASS) || nrow(params_GRASS) < 1) {
    return(list(exist = FALSE))
  }
  
  # --- select version ---
  if (nrow(params_GRASS) == 1) {
    
    gisbase_GRASS <- setenvGRASSw(
      root_GRASS = params_GRASS$instDir[[1]],
      grass_version = params_GRASS$version[[1]],
      installation_type = params_GRASS$installation_type[[1]],
      quiet = quiet
    )
    
    grass_version <- params_GRASS$version[[1]]
    installation_type <- params_GRASS$installation_type[[1]]
    
  } else if (nrow(params_GRASS) > 1 && is.numeric(ver_select) &&
             (ver_select > 0 && ver_select <= nrow(params_GRASS))) {
    
    if (!quiet) {
      cat("You have more than one valid GRASS GIS version\n")
      print(params_GRASS)
      cat("You have selected version: ", ver_select, "\n")
    }
    
    gisbase_GRASS <- normalizePath(
      setenvGRASSw(
        root_GRASS = params_GRASS$instDir[[ver_select]],
        grass_version = params_GRASS$version[[ver_select]],
        installation_type = params_GRASS$installation_type[[ver_select]],
        quiet = quiet
      ),
      winslash = "/"
    )
    
    grass_version <- params_GRASS$version[[ver_select]]
    installation_type <- params_GRASS$installation_type[[ver_select]]
    
  } else if (nrow(params_GRASS) > 1 && !isTRUE(ver_select)) {
    
    # take the latest installed version (semantic version compare)
    v <- suppressWarnings(numeric_version(params_GRASS$version))
    idx <- which(v == max(v))[1]
    
    if (!quiet) {
      cat("You have more than one valid GRASS version installed!\n")
      cat("The latest installed version (", idx, ") has been selected \n")
    }
    
    gisbase_GRASS <- setenvGRASSw(
      root_GRASS = params_GRASS$instDir[[idx]],
      grass_version = params_GRASS$version[[idx]],
      installation_type = params_GRASS$installation_type[[idx]],
      quiet = quiet
    )
    
    grass_version <- params_GRASS$version[[idx]]
    installation_type <- params_GRASS$installation_type[[idx]]
    
  } else {
    
    # interactive selection
    cat("You have more than one valid GRASS GIS version\n")
    print(params_GRASS)
    cat("\n")
    ver <- as.numeric(readline(prompt = "Please select one:  "))
    
    gisbase_GRASS <- normalizePath(
      setenvGRASSw(
        root_GRASS = params_GRASS$instDir[[ver]],
        grass_version = params_GRASS$version[[ver]],
        installation_type = params_GRASS$installation_type[[ver]],
        quiet = quiet
      ),
      winslash = "/"
    )
    
    grass_version <- params_GRASS$version[[ver]]
    installation_type <- params_GRASS$installation_type[[ver]]
  }
  
  grass <- list()
  grass$gisbase_GRASS <- gsub("\\\\", "/", gisbase_GRASS)
  grass$version <- grass_version
  grass$type <- installation_type
  grass$installed <- params_GRASS
  grass$exist <- TRUE
  
  return(grass)
}




#'@title Search recursivly valid 'GRASS GIS' installation(s) on a given 'Windows' drive 
#'@name searchGRASSW
#'@title Search for valid OSGeo4W 'GRASS GIS' installation(s) on a given 'Windows' drive 
#'@description  Provides an  list of valid 'GRASS GIS' installation(s) on your 'Windows' system. There is a major difference between osgeo4W and stand_alone installations. The functions trys to find all valid installations by analysing the calling batch scripts.
#'@param DL drive letter to be searched, default is \code{C:/}
#'@param quiet boolean  switch for supressing console messages default is TRUEs
#'@return A dataframe with the 'GRASS GIS' root folder(s), version name(s) and installation type code(s)
#'@author Chris Reudenbach
#'@export searchGRASSW
#'@keywords internal
#'@examples
#' \dontrun{
#' # get all valid 'GRASS GIS' installation folders and params at 'C:/'
#' searchGRASSW()
#' }
searchGRASSW <- function(DL = "C:/", quiet = TRUE) {
  
  DL <- bf_wpath(DL)
  
  if (!quiet) {
    cat("\nsearching for GRASS installations - this may take a while\n")
    cat("For providing the path manually see ?searchGRASSW \n")
  }
  
  # save + restore options safely
  old_show_err <- getOption("show.error.messages")
  old_warn <- getOption("warn")
  on.exit({
    options(show.error.messages = old_show_err)
    options(warn = old_warn)
  }, add = TRUE)
  
  options(show.error.messages = FALSE)
  options(warn = -1)
  
  raw_GRASS <- try(system(paste0("cmd.exe /c WHERE /R ", DL, " ", "grass*.bat"), intern = TRUE), silent = TRUE)
  
  # restore warning display for subsequent logic/messages (on.exit will also restore)
  options(show.error.messages = TRUE)
  options(warn = 0)
  
  if (methods::is(raw_GRASS, "try-error") || length(raw_GRASS) == 0) {
    message("::: NO GRASS installation found at: '", DL, "'")
    message("::: NOTE:  Links or symbolic links like 'C:/Documents' are not searched...")
    return(FALSE)
  }
  
  # detect standard "not found" outputs
  not_found <- unique(
    grepl(raw_GRASS, pattern = "File not found") |
      grepl(raw_GRASS, pattern = "Datei nicht gefunden") |
      grepl(raw_GRASS, pattern = "INFORMATION:") |
      grepl(raw_GRASS, pattern = "FEHLER:") |
      grepl(raw_GRASS, pattern = "ERROR:")
  )
  
  if (isTRUE(not_found)) {
    message("::: NO GRASS installation found at: '", DL, "'")
    message("::: NOTE:  Links or symbolic links like 'C:/Documents' are not searched...")
    return(FALSE)
  }
  
  installations_GRASS <- lapply(seq_along(raw_GRASS), function(i) {
    
    batchfile_lines <- system(
      paste0("cmd.exe /C TYPE  ", utils::shortPathName(raw_GRASS[i])),
      ignore.stdout = FALSE,
      intern = TRUE
    )
    
    osgeo4w <- FALSE
    stand_alone <- FALSE
    root_dir <- ""
    ver_char <- NA_character_
    installerType <- NA_character_
    
    if (length(unique(grep("OSGEO4W", batchfile_lines, value = TRUE))) > 0) {
      osgeo4w <- TRUE
      stand_alone <- FALSE
    }
    
    if (length(unique(grep("NSIS installer", batchfile_lines, value = TRUE))) > 0) {
      osgeo4w <- FALSE
      stand_alone <- TRUE
    }
    
    if (osgeo4w) {
      
      bn <- basename(utils::shortPathName(raw_GRASS[i]))
      
      if (bn %in% c("grass78.bat", "grass79.bat", "grass83.bat")) {
        
        root_dir <- dirname(dirname(utils::shortPathName(raw_GRASS[i])))
        ver_char <- substr(bn, 6, 7)
        installerType <- "osgeo4W"
        
      } else {
        
        if (length(grep("PREREM~1", utils::shortPathName(raw_GRASS[i]))) == 0 &&
            length(grep("extrabin", utils::shortPathName(raw_GRASS[i]))) == 0) {
          
          root_dir <- unique(grep("OSGEO4W_ROOT", batchfile_lines, value = TRUE))
          if (length(root_dir) > 0) {
            root_dir <- substr(root_dir, gregexpr(pattern = "=", root_dir)[[1]][1] + 1, nchar(root_dir))
          }
          
          ver_char <- unique(grep("\\benv.bat\\b", batchfile_lines, value = TRUE))
          if (length(root_dir) > 0 && length(ver_char) > 0) {
            ver_char <- substr(ver_char, gregexpr(pattern = "\\grass-", ver_char)[[1]][1], nchar(ver_char))
            ver_char <- substr(ver_char, 1, gregexpr(pattern = "\\\\", ver_char)[[1]][1] - 1)
          }
        }
        
        installerType <- "osgeo4W"
      }
    }
    
    if (stand_alone) {
      
      root_dir <- unique(grep("set GISBASE=", batchfile_lines, value = TRUE))
      if (length(root_dir) > 0) {
        root_dir <- substr(root_dir, gregexpr(pattern = "=", root_dir)[[1]][1] + 1, nchar(root_dir))
      }
      
      ver_char <- root_dir
      if (length(root_dir) > 0) {
        ver_char <- substr(ver_char, gregexpr(pattern = "GRASS", ver_char)[[1]][1], nchar(ver_char))
      }
      
      installerType <- "NSIS"
    }
    
    exist <- FALSE
    if (length(root_dir) > 0 && !is.na(root_dir)) {
      # keep your original "strip after =" behavior but guard indices
      if (length(gregexpr(pattern = "=", root_dir)[[1]]) > 0 && gregexpr(pattern = "=", root_dir)[[1]][1] > 0) {
        root_dir <- substr(root_dir[[1]], gregexpr(pattern = "=", root_dir)[[1]][1] + 1, nchar(root_dir))
      } else {
        root_dir <- root_dir[[1]]
      }
      exist <- file.exists(file.path(root_dir))
    }
    
    if (length(root_dir) > 0 && exist) {
      data.frame(
        instDir = root_dir,
        version = ver_char,
        installation_type = installerType,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })
  
  installations_GRASS <- do.call("rbind", installations_GRASS)
  
  if (is.null(installations_GRASS) || nrow(installations_GRASS) == 0) {
    if (!quiet) cat("Did not find any valid GRASS installation at mount point", DL)
    return(FALSE)
  }
  
  return(installations_GRASS)
}


#'@title Return attributes of valid 'GRASS GIS' installation(s) in 'Linux'
#'@name searchGRASSX
#'@description Searches recursively for valid 'GRASS GIS' installations at a given 'Linux' mount point.
#'Returns attributes for each installation.
#'@param MP default is /usr. This is the directory from which the grass executable file is searched, i.e. one executable for each GRASS installation on the system.
#'@return data frame containing 'GRASS GIS' binary folder(s) (i.e. where the individual GRASS commands are installed), version name(s) and installation type code(s)
#'@param quiet boolean.  switch for suppressing console messages default is TRUE
#'@author Chris Reudenbach
#'@export searchGRASSX
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' # get all valid 'GRASS GIS' installation folders in the /usr/bin directory (typical location)
#' searchGRASSX('/usr/bin')
#' 
#' # get all valid 'GRASS GIS' installation folders in the home directory
#' searchGRASSX('~/')
#' }
searchGRASSX <- function(MP = "/usr/bin", quiet = TRUE) {
  
  if (MP == "default") MP <- "/usr/bin"
  
  raw_GRASS <- system2(
    "find",
    paste(MP, " ! -readable -prune -o -type f -executable -iname 'grass??' -print"),
    stdout = TRUE,
    stderr = FALSE
  )
  
  if (length(raw_GRASS) == 0) {
    raw_GRASS <- system2(
      "find",
      paste(MP, " ! -readable -prune -o -type f -executable -iname 'grass' -print"),
      stdout = TRUE,
      stderr = FALSE
    )
  }
  
  if (length(raw_GRASS) > 0) {
    
    installations_GRASS <- lapply(seq_along(raw_GRASS), function(i) {
      
      rg <- strsplit(raw_GRASS[[i]], split = "/", fixed = TRUE)[[1]]
      last <- rg[length(rg)]
      
      lines <- try(readLines(raw_GRASS[[i]], warn = FALSE), silent = TRUE)
      if (methods::is(lines, "try-error")) {
        return(NULL)
      }
      
      root_dir <- NA_character_
      ver_char <- NA_character_
      cmd <- NA_character_
      
      # Heuristic branch for some launcher scripts (keep logic, but fix indexing)
      if (last %in% c("grass78", "grass")) {
        
        ver_line <- grep("GRASS_VERSION = \"", lines, value = TRUE)
        if (length(ver_line) > 0) {
          ver_char <- substr(ver_line[1], gregexpr("\"", ver_line[1])[[1]][1] + 1, nchar(ver_line[1]) - 1)
        }
        
        cmd_line <- grep("CMD_NAME = \"", lines, value = TRUE)
        if (length(cmd_line) > 0) {
          cmd <- substr(cmd_line[1], gregexpr("\"", cmd_line[1])[[1]][1] + 1, nchar(cmd_line[1]) - 1)
        }
        
        rootdir <- grep("GISBASE = os.path.normpath", lines, value = TRUE)
        if (length(rootdir) >= 2) {
          root_dir <- substr(rootdir[2], gregexpr("\"", rootdir[2])[[1]][1] + 1, nchar(rootdir[2]) - 2)
        } else if (length(rootdir) == 1) {
          # fallback: try the only line if second is missing
          root_dir <- substr(rootdir[1], gregexpr("\"", rootdir[1])[[1]][1] + 1, nchar(rootdir[1]) - 2)
        }
        
        if (!is.na(root_dir) && !file.exists(root_dir)) {
          # keep your fallback behavior
          root_dir <- "/opt/grass"
        }
        
      } else {
        
        root_line <- try(grep("gisbase = \"", lines, value = TRUE), silent = TRUE)
        if (!methods::is(root_line, "try-error") && length(root_line) > 0) {
          root_dir <- substr(root_line[1], gregexpr("\"", root_line[1])[[1]][1] + 1, nchar(root_line[1]) - 1)
          
          ver_line <- grep("grass_version = \"", lines, value = TRUE)
          if (length(ver_line) > 0) {
            ver_char <- substr(ver_line[1], gregexpr("\"", ver_line[1])[[1]][1] + 1, nchar(ver_line[1]) - 1)
          }
          
          cmd_line <- grep("cmd_name = \"", lines, value = TRUE)
          if (length(cmd_line) > 0) {
            cmd <- substr(cmd_line[1], gregexpr("\"", cmd_line[1])[[1]][1] + 1, nchar(cmd_line[1]) - 1)
          }
        }
      }
      
      data.frame(
        instDir = root_dir,
        version = ver_char,
        installation_type = cmd,
        stringsAsFactors = FALSE
      )
    })
    
    installations_GRASS <- do.call("rbind", installations_GRASS)
    
    return(installations_GRASS)
    
  } else {
    
    if (!quiet) cat("Did not find any valid GRASS installation at mount point", MP)
    return(installations_GRASS <- FALSE)
  }
}



#'@title Usually for internally usage, create valid 'GRASS GIS 7.xx' rsession environment settings according to the selected GRASS GIS 7.x and Windows Version
#'@name setenvGRASSw
#'@description  Initializes and set up  access to 'GRASS GIS 7.xx' via the \code{rgrass} wrapper or command line packages. Set and returns all necessary environment variables and additionally returns the GISBASE directory as string.
#'@param root_GRASS  grass root directory i.e. 'C:\\OSGEO4~1',
#'@param grass_version grass version name i.e. 'grass-7.0.5'
#'@param installation_type two options 'osgeo4w' as installed by the 'OSGeo4W'-installer and 'NSIS' that is typical for a stand_alone installation of 'GRASS GIS'.
#'@param quiet boolean  switch for suppressing console messages default is TRUE
#'@param jpgmem jpeg2000 memory allocation size. Default is 1000000
#'@author Chris Reudenbach
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' # set selected 'GRASS GIS' installation folders 
#' setenvGRASSw(root_GRASS = 'C:\\PROGRA~1\\QGIS2~1.18',
#'              grass_version =  'grass-7.2.1',
#'              installation_type =  'osgeo4W')
#' }
setenvGRASSw <- function(root_GRASS = NULL, grass_version = NULL,
                         installation_type = NULL,
                         jpgmem = 1e+06, quiet = TRUE) {
  
  if (Sys.info()["sysname"] == "Windows") {
    
    # ---- package-private cache (CRAN safe) ----
    .GRASS_CACHE <- .get_GRASS_CACHE()
    # ------------------------------------------
    
    if (is.null(root_GRASS) || is.null(grass_version) || is.null(installation_type)) {
      stop("Please run findGRASS first and provide valid arguments")
    }
    
    if (installation_type == "osgeo4W" || installation_type == "OSGeo4W64") {
      
      Sys.setenv(OSGEO4W_ROOT = root_GRASS)
      
      gisbase_GRASS <- paste0(root_GRASS, "\\apps\\grass\\grass", grass_version)
      Sys.setenv(GISBASE = gisbase_GRASS)
      
      assign("SYS", "WinNat", envir = .GRASS_CACHE)
      assign("addEXE", ".exe", envir = .GRASS_CACHE)
      assign("WN_bat", "", envir = .GRASS_CACHE)
      assign("legacyExec", "windows", envir = .GRASS_CACHE)
      
      Sys.setenv(
        GRASS_PYTHON    = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\bin\\python.exe"),
        PYTHONHOME      = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\apps\\Python27"),
        PYTHONPATH      = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\apps\\grass\\",
                                 grass_version, "\\etc\\python"),
        GRASS_PROJSHARE = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\share\\proj"),
        PROJ_LIB        = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\share\\proj"),
        GDAL_DATA       = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\share\\gdal"),
        GEOTIFF_CSV     = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\share\\epsg_csv"),
        FONTCONFIG_FILE = paste0(Sys.getenv("OSGEO4W_ROOT"), "\\etc\\fonts.conf"),
        GDAL_DRIVER_PATH= paste0(Sys.getenv("OSGEO4W_ROOT"), "\\bin\\gdalplugins"),
        JPEGMEM         = as.character(jpgmem),
        GISRC           = paste(Sys.getenv("HOME"), "\\.grassrc7", sep = "")
      )
      
      Sys.setenv(
        PATH = paste0(
          gisbase_GRASS, ";",
          root_GRASS, "\\apps\\Python27\\lib\\site-packages\\numpy\\core", ";",
          root_GRASS, "\\apps\\grass\\", grass_version, "\\bin", ";",
          root_GRASS, "\\apps\\grass\\", grass_version, "\\lib", ";",
          root_GRASS, "\\apps\\grass\\", grass_version, "\\etc", ";",
          root_GRASS, "\\apps\\grass\\", grass_version, "\\etc\\python", ";",
          root_GRASS, "\\apps\\Python27\\Scripts", ";",
          root_GRASS, "\\bin", ";",
          root_GRASS, "\\apps", ";",
          paste0(Sys.getenv("WINDIR"), "/WBem"), ";",
          Sys.getenv("PATH")
        )
      )
      
      if (!quiet) {
        system(paste0(root_GRASS, "/bin/o-help.bat"))
      }
      
    } else {
      # ---- NSIS stand-alone installation ----
      
      Sys.setenv(GRASS_ROOT = root_GRASS)
      
      gisbase_GRASS <- normalizePath(root_GRASS)
      Sys.setenv(GISBASE = gisbase_GRASS)
      
      assign("SYS", "WinNat", envir = .GRASS_CACHE)
      assign("addEXE", ".exe", envir = .GRASS_CACHE)
      assign("WN_bat", "", envir = .GRASS_CACHE)
      assign("legacyExec", "windows", envir = .GRASS_CACHE)
      
      Sys.setenv(
        GRASS_PYTHON    = paste0(Sys.getenv("GRASS_ROOT"), "\\bin\\python.exe"),
        PYTHONHOME      = paste0(Sys.getenv("GRASS_ROOT"), "\\Python27"),
        PYTHONPATH      = paste0(Sys.getenv("GRASS_ROOT"), "\\etc\\python"),
        GRASS_PROJSHARE = paste0(Sys.getenv("GRASS_ROOT"), "\\share\\proj"),
        PROJ_LIB        = paste0(Sys.getenv("GRASS_ROOT"), "\\share\\proj"),
        GDAL_DATA       = paste0(Sys.getenv("GRASS_ROOT"), "\\share\\gdal"),
        GEOTIFF_CSV     = paste0(Sys.getenv("GRASS_ROOT"), "\\share\\epsg_csv"),
        FONTCONFIG_FILE = paste0(Sys.getenv("GRASS_ROOT"), "\\etc\\fonts.conf"),
        GDAL_DRIVER_PATH= paste0(Sys.getenv("GRASS_ROOT"), "\\bin\\gdalplugins"),
        JPEGMEM         = as.character(jpgmem),
        GISRC           = paste(Sys.getenv("HOME"), "\\.grassrc7", sep = "")
      )
      
      Sys.setenv(
        PATH = paste0(
          gisbase_GRASS, ";",
          root_GRASS, "\\Python27\\lib\\site-packages\\numpy\\core", ";",
          root_GRASS, "\\bin", ";",
          root_GRASS, "\\extrabin", ";",
          root_GRASS, "\\lib", ";",
          root_GRASS, "\\etc", ";",
          root_GRASS, "\\etc\\python", ";",
          root_GRASS, "\\Scripts", ";",
          root_GRASS, ";",
          paste0(Sys.getenv("WINDIR"), "/WBem"), ";",
          Sys.getenv("PATH")
        )
      )
    }
    
  } else {
    gisbase_GRASS <- "Sorry no Windows System..."
  }
  
  return(gisbase_GRASS)
}



checkGisdbase <- function(x = NULL, gisdbase = NULL, location = NULL,
                          gisdbase_exist = FALSE, obj_name = NULL) {
  
  if (isTRUE(gisdbase_exist)) {
    # Link to an existing GRASS database/location
    linkGRASS(gisdbase = gisdbase, location = location, gisdbase_exist = TRUE)
  } else {
    # Create/init GRASS database/location using spatial reference from x
    linkGRASS(x = x, gisdbase = gisdbase, location = location)
  }
  
  path <- Sys.getenv("GISDBASE")
  
  if (is.null(obj_name) || is.na(obj_name) || !nzchar(obj_name)) {
    sq_name <- NA_character_
  } else {
    sq_name <- gsub("-", "_", tolower(paste0(obj_name, ".sqlite")))
  }
  
  return(list(gisbase_path = path, sqlite = sq_name))
}



#'@title Returns attributes of valid 'GRASS GIS' installation(s) on the system.
#'@name findGRASS
#'@description Retrieve a list of valid 'GRASS GIS' installation(s) on your system. There is a big difference between osgeo4W and stand_alone installations. The function tries to find all valid installations by analyzing the calling batch scripts.
#'@param searchLocation Location to search for the grass executable, i.e. one executable for each GRASS installation on the system. For Windows systems it is mandatory to include an uppercase Windows drive letter and a colon.
#'Default for Windows systems 
#'is \code{C:/}, for Linux systems the default is \code{/usr/bin}.
#'@param ver_select boolean, Default is FALSE. If there is more than one 'GRASS GIS' installation and \code{ver_select} = TRUE, the user can interactively select the preferred 'GRASS GIS' version. 
#'@param quiet boolean, default is TRUE. switch to suppress console messages
#'@return data frame with the 'GRASS GIS' binary folder(s) (i.e. where the individual 
#'individual GRASS commands are installed), version name(s) and 
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
findGRASS <- function(searchLocation = "default", ver_select = FALSE, quiet = TRUE) {
  
  if (Sys.info()["sysname"] == "Windows") {
    
    # NOTE: do not stop here. It's normal that GRASS is not yet on PATH.
    check <- try(system("o-help", intern = TRUE), silent = TRUE)
    if (methods::is(check, "try-error") && !quiet) {
      message(
        "PLEASE NOTE: If you use GRASS version > 7.8 and/or the OSGeo4W installation you may need:\n",
        " 1) start the OSGeo4W shell\n",
        " 2) start grassxx --gtext\n",
        " 3) start Rstudio from command line in the shell\n",
        "Then both link2GI and rgrass should work.\n",
        "Continuing with search..."
      )
    }
    
    if (searchLocation == "default") {
      searchLocation <- "C:/"
    } else {
      searchLocation <- normalizePath(searchLocation)
    }
    
    if (grepl(paste0(LETTERS, ":", collapse = "|"), substr(toupper(searchLocation), start = 1, stop = 2))) {
      link <- link2GI::searchGRASSW(DL = searchLocation)
    } else {
      return(cat("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon"))
    }
    
  } else {
    
    if (searchLocation == "default") {
      searchLocation <- "/usr/bin"
    }
    
    if (grepl(searchLocation, pattern = ":")) {
      return(cat("You are running Linux - please choose a suitable searchLocation argument"))
    } else {
      link <- link2GI::searchGRASSX(MP = searchLocation)
    }
  }
  
  return(link)
}
