.link2GI_cache <- new.env(parent = emptyenv())

.get_GRASS_CACHE <- function() {
  if (!exists(".GRASS_CACHE", envir = .link2GI_cache, inherits = FALSE)) {
    .link2GI_cache$.GRASS_CACHE <- new.env(parent = emptyenv())
  }
  .link2GI_cache$.GRASS_CACHE
}

is_osgeo4w_env <- function(root = NULL) {
  o <- Sys.getenv("OSGEO4W_ROOT")
  p <- Sys.getenv("PATH")
  has_bin <- nzchar(o) && grepl(gsub("\\\\", "/", file.path(o, "bin")), gsub("\\\\", "/", p), fixed = TRUE)
  list(
    in_env = nzchar(o) && has_bin,
    OSGEO4W_ROOT = o,
    has_bin_in_path = has_bin,
    root = root
  )
}


#' Search for valid GRASS GIS installations on Windows
#'
#' Searches for GRASS GIS installations on **Windows** using a *bounded* set of
#' plausible installation roots (no full-disk crawl). The function supports:
#' \itemize{
#'   \item OSGeo4W / QGIS-style layouts via \code{<root>/apps/grass/grass*/etc/VERSIONNUMBER}
#'   \item Standalone GRASS installs via \code{<Program Files>/GRASS GIS */etc/VERSIONNUMBER}
#'   \item Optional per-user OSGeo4W installs under
#'         \code{<USERPROFILE>/AppData/Local/Programs/OSGeo4W}
#' }
#'
#' The argument \code{DL} can be a full path or a Windows drive root
#' (e.g. \code{"C:"} or \code{"C:/"}). Drive roots are expanded to a fixed set of
#' candidate directories:
#' \code{OSGeo4W64}, \code{OSGeo4W}, \code{Program Files}, \code{Program Files (x86)}.
#'
#' @param DL Character. Search location or drive root on Windows.
#'   Accepts \code{"C:"}, \code{"C:/"}, or a concrete directory path.
#'   Backslashes are normalized to forward slashes.
#' @param quiet Logical. If \code{TRUE} (default), suppress informational messages.
#'
#' @return
#' Returns \code{FALSE} if no installation was detected.
#' Otherwise returns a \code{data.frame} with columns:
#' \describe{
#'   \item{instDir}{Root directory of the installation candidate.}
#'   \item{version}{Parsed version string (from \code{VERSIONNUMBER}) or \code{NA}.}
#'   \item{installation_type}{One of \code{"osgeo4w"}, \code{"qgis"}, \code{"standalone"}.}
#' }
#' The result is sorted by decreasing semantic version (unknown versions treated as \code{0.0.0}).
#'
#' @details
#' This function is intentionally conservative to remain fast and deterministic on
#' large Windows volumes. It does **not** recurse the entire drive.
#'
#' If multiple installations are present under the searched roots, all are returned.
#' Version parsing extracts the first \code{x.y[.z...]} pattern from the first line
#' of \code{VERSIONNUMBER}.
#'
#' @examples
#' \dontrun{
#' # Search from the C: drive root (bounded roots, no full-disk scan)
#' searchGRASSW("C:/", quiet = FALSE)
#'
#' # Search a concrete directory only
#' searchGRASSW("C:/OSGeo4W64", quiet = FALSE)
#'
#' # Drive letter without slash is accepted
#' searchGRASSW("C:", quiet = TRUE)
#' }
#'
#' @export
searchGRASSW <- function(DL = "C:/", quiet = TRUE) {
  
  .msg <- function(...) if (!isTRUE(quiet)) message(...)
  
  if (!identical(Sys.info()[["sysname"]], "Windows")) {
    stop("searchGRASSW() is Windows-only.")
  }
  
  # ------------------------------------------------------------
  # FIX 1: Windows drive semantics — MUST happen BEFORE normalizePath()
  # ------------------------------------------------------------
  DL <- gsub("\\\\", "/", DL)
  if (grepl("^[A-Za-z]:$", DL)) {
    DL <- paste0(DL, "/")
  }
  
  DL <- normalizePath(path.expand(DL), winslash = "/", mustWork = FALSE)
  
  if (is.na(DL) || !grepl("^[A-Za-z]:(/|$)", DL)) {
    return(FALSE)
  }
  
  is_drive_root <- grepl("^[A-Za-z]:(/)?$", DL)
  drive <- substr(DL, 1, 2)  # "C:"
  
  # ------------------------------------------------------------
  # Candidate roots (bounded, no full-disk crawl)
  # ------------------------------------------------------------
  candidates <- if (is_drive_root) {
    c(
      paste0(drive, "/OSGeo4W64"),
      paste0(drive, "/OSGeo4W"),
      paste0(drive, "/Program Files"),
      paste0(drive, "/Program Files (x86)")
    )
  } else {
    c(DL)
  }
  
  candidates <- unique(candidates)
  candidates <- candidates[dir.exists(candidates)]
  
  # ------------------------------------------------------------
  # FIX 2: deterministic per-user OSGeo4W
  # ------------------------------------------------------------
  up <- normalizePath(Sys.getenv("USERPROFILE"), winslash = "/", mustWork = FALSE)
  cand_user <- file.path(up, "AppData/Local/Programs/OSGeo4W")
  if (dir.exists(cand_user)) {
    candidates <- unique(c(candidates, cand_user))
  }
  
  if (length(candidates) == 0) {
    .msg("::: NO GRASS installation found: no plausible search roots exist on ", drive, "/")
    return(FALSE)
  }
  
  .read_version <- function(f) {
    x <- try(readLines(f, warn = FALSE), silent = TRUE)
    if (inherits(x, "try-error") || length(x) == 0) return(NA_character_)
    m <- regmatches(x[1], regexpr("[0-9]+(\\.[0-9]+)+", x[1]))
    if (length(m) == 0) NA_character_ else m
  }
  
  found <- list()
  
  # ------------------------------------------------------------
  # A) OSGeo4W / QGIS layout
  # ------------------------------------------------------------
  for (root in candidates) {
    
    apps_grass <- file.path(root, "apps", "grass")
    if (!dir.exists(apps_grass)) next
    
    grass_dirs <- list.dirs(apps_grass, full.names = TRUE, recursive = FALSE)
    grass_dirs <- grass_dirs[grepl("^grass[0-9]+", basename(grass_dirs), ignore.case = TRUE)]
    
    for (gd in grass_dirs) {
      vf <- file.path(gd, "etc", "VERSIONNUMBER")
      if (!file.exists(vf)) next
      
      instDir <- normalizePath(root, winslash = "/", mustWork = FALSE)
      
      installation_type <- "osgeo4w"
      if (grepl("/QGIS", instDir, ignore.case = TRUE)) {
        installation_type <- "qgis"
      }
      
      found[[length(found) + 1]] <- data.frame(
        instDir = instDir,
        version = .read_version(vf),
        installation_type = installation_type,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # ------------------------------------------------------------
  # B) Standalone layout
  # ------------------------------------------------------------
  pf_roots <- intersect(
    c(paste0(drive, "/Program Files"), paste0(drive, "/Program Files (x86)")),
    candidates
  )
  
  for (root in pf_roots) {
    
    grass_pf <- list.dirs(root, full.names = TRUE, recursive = FALSE)
    grass_pf <- grass_pf[grepl("^GRASS GIS", basename(grass_pf), ignore.case = TRUE)]
    
    for (gd in grass_pf) {
      vf <- file.path(gd, "etc", "VERSIONNUMBER")
      if (!file.exists(vf)) next
      
      found[[length(found) + 1]] <- data.frame(
        instDir = normalizePath(gd, winslash = "/", mustWork = FALSE),
        version = .read_version(vf),
        installation_type = "standalone",
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(found) == 0) {
    .msg("::: NO GRASS installation found at: '", DL, "'")
    .msg("::: NOTE: Avoid scanning full drive roots; install roots differ (OSGeo4W/QGIS/user-local).")
    return(FALSE)
  }
  
  out <- unique(do.call(rbind, found))
  
  suppressWarnings({
    vnum <- numeric_version(ifelse(is.na(out$version), "0.0.0", out$version))
    out <- out[order(vnum, decreasing = TRUE), , drop = FALSE]
  })
  
  if (!quiet) {
    message("::: Found ", nrow(out), " GRASS installation(s) derived from: '", DL, "'")
    print(out)
  }
  
  invisible(out)
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
    if (!quiet) {
      message("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon")
    }
    return(FALSE)  }
  
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





#' Search for valid GRASS GIS installations on Windows
#'
#' @param DL Character. Search root (e.g. `"C:/"`).
#' @param quiet Logical. Suppress messages.
#'
#' @return `FALSE` or a `data.frame` with columns `instDir`, `version`, `installation_type`.
#' @keywords internal
#' @export
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
#'@export 
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
setenvGRASSw <- function(root_GRASS, grass_version = NULL, installation_type = NULL, quiet = TRUE) {
  
  root_GRASS <- normalizePath(root_GRASS, winslash = "/", mustWork = FALSE)
  
  # If user passed OSGeo4W root, derive real GISBASE = .../apps/grass/grassXX
  if (!dir.exists(file.path(root_GRASS, "scripts")) &&
      dir.exists(file.path(root_GRASS, "apps", "grass"))) {
    
    cand <- list.dirs(file.path(root_GRASS, "apps", "grass"),
                      full.names = TRUE, recursive = FALSE)
    
    cand <- cand[grepl("^grass[0-9]+", basename(cand), ignore.case = TRUE)]
    cand <- cand[dir.exists(file.path(cand, "scripts"))]
    
    if (length(cand) > 0) {
      # choose highest numeric suffix (e.g., grass84 > grass83)
      suf <- suppressWarnings(as.integer(gsub(".*grass", "", basename(cand), ignore.case = TRUE)))
      cand <- cand[order(suf, decreasing = TRUE)]
      root_GRASS <- cand[1]
    }
  }
  
  # return GISBASE (must contain scripts/)
  gisbase_GRASS <- root_GRASS
  
  if (!dir.exists(file.path(gisbase_GRASS, "scripts"))) {
    stop(gisbase_GRASS, " does not contain scripts/")
  }
  
  gisbase_GRASS
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




#' @title Returns attributes of valid 'GRASS GIS' installation(s) on the system.
#' @name findGRASS
#' @description Retrieve a list of valid 'GRASS GIS' installation(s) on your system.
#' On Windows, uses searchGRASSW() (cmd-free). On Unix, uses searchGRASSX().
#' @param searchLocation On Windows MUST start with drive letter + colon, e.g. "C:", "C:/", "C:/Users/...".
#' Defaults to "C:/". On Unix defaults to "/usr/bin".
#' @param ver_select If TRUE and more than one installation is found, interactively select one.
#' @param quiet Suppress messages.
#' @return FALSE or data.frame(instDir, version, installation_type)
#' @export
findGRASS <- function(searchLocation = "default", ver_select = FALSE, quiet = TRUE) {
  
  .msg <- function(...) if (!isTRUE(quiet)) message(...)
  
  # ---------------------------
  # Windows
  # ---------------------------
  if (Sys.info()[["sysname"]] == "Windows") {
    
    # Resolve default
    if (identical(searchLocation, "default") || is.null(searchLocation)) {
      searchLocation <- "C:/"
    } else {
      searchLocation <- normalizePath(path.expand(searchLocation), winslash = "/", mustWork = FALSE)
    }
    
    # Validate Windows drive prefix (NO vector-pattern bug; NO cat()/NULL)
    if (is.na(searchLocation) || !grepl("^[A-Za-z]:(/|$)", searchLocation)) {
      .msg(
        "You are running Windows - Please choose a suitable searchLocation argument ",
        "that MUST include a Windows drive letter and colon"
      )
      return(FALSE)
    }
    
    # Run Windows finder (cmd-free)
    link <- searchGRASSW(DL = searchLocation, quiet = quiet)
    
    # Optional hint ONLY if not found and GRASS not on PATH
    if (isFALSE(link)) {
      check <- try(system("o-help", intern = TRUE), silent = TRUE)
      if (methods::is(check, "try-error") && !quiet) {
        message(
          "PLEASE NOTE: If you use GRASS version > 7.8 and/or the OSGeo4W installation you may need:\n",
          " 1) start the OSGeo4W shell\n",
          " 2) start grassxx --gtext\n",
          " 3) start Rstudio from command line in the shell\n",
          "Then both link2GI and rgrass should work.\n"
        )
      }
    }
    
    # ---------------------------
    # Unix / Linux / macOS
    # ---------------------------
  } else {
    
    if (identical(searchLocation, "default") || is.null(searchLocation)) {
      searchLocation <- "/usr/bin"
    }
    
    # Reject Windows drive syntax on Unix
    if (grepl(":", searchLocation, fixed = TRUE)) {
      .msg("You are running Linux/Unix - please choose a suitable searchLocation argument")
      return(FALSE)
    }
    
    link <- link2GI::searchGRASSX(MP = searchLocation)
  }
  
  # ---------------------------
  # Optional interactive selection
  # ---------------------------
  if (isTRUE(ver_select) && is.data.frame(link) && nrow(link) > 1) {
    if (!quiet) {
      cat("You have more than one valid GRASS GIS version\n")
      print(link)
      cat("\n")
    }
    ver <- suppressWarnings(as.numeric(readline(prompt = "Please select one:  ")))
    if (!is.na(ver) && ver >= 1 && ver <= nrow(link)) {
      link <- link[ver, , drop = FALSE]
      rownames(link) <- NULL
    }
  }
  
  link
}

.activate_osgeo4w_env <- function(osgeo4w_root, quiet = TRUE) {
  osgeo4w_root <- normalizePath(osgeo4w_root, winslash = "/", mustWork = TRUE)
  
  # minimal set of vars that rgrass/initGRASS typically checks for OSGeo4W
  Sys.setenv(OSGEO4W_ROOT = osgeo4w_root)
  
  # prepend OSGeo4W/bin to PATH (DLL search path)
  bin <- normalizePath(file.path(osgeo4w_root, "bin"), winslash = "/", mustWork = TRUE)
  p <- Sys.getenv("PATH")
  if (!grepl("OSGeo4W[/\\\\]bin", p, ignore.case = TRUE)) {
    Sys.setenv(PATH = paste(bin, p, sep = .Platform$path.sep))
  }
  
  if (!quiet) {
    message("::: Activated OSGeo4W environment: ", osgeo4w_root)
  }
  
  invisible(TRUE)
}

.in_osgeo4w_env <- function() {
  nzchar(Sys.getenv("OSGEO4W_ROOT")) || grepl("OSGeo4W[/\\\\]bin", Sys.getenv("PATH"), ignore.case = TRUE)
}
