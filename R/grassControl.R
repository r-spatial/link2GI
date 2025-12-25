

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


#'@title Usually for internally usage, get 'GRASS GIS' and \code{rgrass} parameters on 'Linux' OS
#'@name paramGRASSx
#'@description Initialize and set up \code{rgrass} for 'Linux'
#'@details During the rsession you will have full access to GRASS GIS via the \code{rgrass} wrapper.
#'@param set_default_GRASS default is NULL. will force a search for 'GRASS GIS'
#'   You may provide a valid combination as c('/usr/lib/grass83','8.3.2','grass')
#'@param MP default is '/usr/bin'. mount point to be searched.
#'@param quiet boolean, default is TRUE. switch for suppressing console messages
#'@param ver_select if TRUE you must interactively select between alternative installations
#'@keywords internal
paramGRASSx <- function(set_default_GRASS = NULL, MP = "/usr/bin", ver_select = FALSE, quiet = TRUE) {
  
  if (identical(ver_select, "T")) ver_select <- TRUE
  if (identical(ver_select, "F") && !is.numeric(ver_select)) ver_select <- FALSE
  
  if (Sys.info()[["sysname"]] == "Windows") {
    if (!quiet) {
      message(
        "You are running Windows - Please choose a suitable searchLocation ",
        "argument that MUST include a Windows drive letter and colon"
      )
    }
    return(list(exist = FALSE))
  }
  
  # ------------------------------------------------------------
  # Detect installations
  # ------------------------------------------------------------
  if (is.null(set_default_GRASS)) {
    params_GRASS <- findGRASS(searchLocation = MP, quiet = quiet)
  } else {
    params_GRASS <- rbind.data.frame(set_default_GRASS)
    names(params_GRASS) <- c("instDir", "version", "installation_type")
  }
  
  if (isFALSE(params_GRASS) || !is.data.frame(params_GRASS) || nrow(params_GRASS) < 1) {
    return(list(exist = FALSE))
  }
  
  # ------------------------------------------------------------
  # Select installation index
  # ------------------------------------------------------------
  sel_idx <- 1L
  
  if (nrow(params_GRASS) == 1) {
    sel_idx <- 1L
    
  } else if (nrow(params_GRASS) > 1 &&
             is.numeric(ver_select) &&
             ver_select > 0 &&
             ver_select <= nrow(params_GRASS)) {
    
    sel_idx <- as.integer(ver_select)
    
    if (!quiet) {
      cat("You have more than one valid GRASS version installed!\n")
      print(params_GRASS)
      cat("Selected version is:", sel_idx, "\n")
    }
    
  } else if (nrow(params_GRASS) > 1 && !isTRUE(ver_select)) {
    
    v <- suppressWarnings(numeric_version(params_GRASS$version))
    sel_idx <- which(v == max(v))[1]
    
    if (!quiet) {
      cat("You have more than one valid GRASS version installed!\n")
      cat("The latest installed version (", sel_idx, ") has been selected\n")
      print(params_GRASS)
      cat("\n")
    }
    
  } else {
    # interactive selection
    cat("You have more than one valid GRASS version installed!\n")
    print(params_GRASS)
    cat("\n")
    ver <- suppressWarnings(as.numeric(readline(prompt = "Please select one: ")))
    if (!is.na(ver) && ver >= 1 && ver <= nrow(params_GRASS)) {
      sel_idx <- as.integer(ver)
    } else {
      stop("Invalid GRASS version selection.")
    }
  }
  
  # ------------------------------------------------------------
  # Build return object (Windows parity)
  # ------------------------------------------------------------
  grass <- list()
  grass$gisbase_GRASS <- as.character(params_GRASS$instDir[[sel_idx]])
  grass$version       <- params_GRASS$version[[sel_idx]]
  grass$installed     <- params_GRASS
  grass$exist         <- TRUE
  
  it <- params_GRASS$installation_type[[sel_idx]]
  grass$type <- if (is.null(it) || is.na(it) || !nzchar(it)) NA_character_ else tolower(as.character(it))
  
  grass
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
  
  DL <- .bf_wpath(DL)
  
  if (!quiet) {
    cat("\nsearching for GRASS installations - this may take a while\n")
    cat("For providing the path manually see ?searchGRASSW \n")
  }
  
  old_show_err <- getOption("show.error.messages")
  old_warn <- getOption("warn")
  on.exit({
    options(show.error.messages = old_show_err)
    options(warn = old_warn)
  }, add = TRUE)
  
  options(show.error.messages = FALSE)
  options(warn = -1)
  
  raw_GRASS <- try(
    system(paste0("cmd.exe /c WHERE /R ", DL, " grass*.bat"), intern = TRUE),
    silent = TRUE
  )
  
  options(show.error.messages = TRUE)
  options(warn = 0)
  
  if (methods::is(raw_GRASS, "try-error") || length(raw_GRASS) == 0) {
    if (!quiet) {
      message("::: NO GRASS installation found at: '", DL, "'")
      message("::: NOTE: Links or symbolic links like 'C:/Documents' are not searched...")
    }
    return(FALSE)
  }
  
  # standard "not found" outputs
  not_found <- any(
    grepl("File not found", raw_GRASS, fixed = TRUE) |
      grepl("Datei nicht gefunden", raw_GRASS, fixed = TRUE) |
      grepl("INFORMATION:", raw_GRASS, fixed = TRUE) |
      grepl("FEHLER:", raw_GRASS, fixed = TRUE) |
      grepl("ERROR:", raw_GRASS, fixed = TRUE)
  )
  
  if (isTRUE(not_found)) {
    if (!quiet) {
      message("::: NO GRASS installation found at: '", DL, "'")
      message("::: NOTE: Links or symbolic links like 'C:/Documents' are not searched...")
    }
    return(FALSE)
  }
  
  installations_GRASS <- lapply(seq_along(raw_GRASS), function(i) {
    
    bf <- utils::shortPathName(raw_GRASS[i])
    
    batchfile_lines <- try(
      system(paste0("cmd.exe /C TYPE ", bf), intern = TRUE),
      silent = TRUE
    )
    if (methods::is(batchfile_lines, "try-error") || length(batchfile_lines) == 0) return(NULL)
    
    osgeo4w <- length(grep("OSGEO4W", batchfile_lines, value = TRUE)) > 0
    nsis    <- length(grep("NSIS installer", batchfile_lines, value = TRUE)) > 0
    
    root_dir <- NA_character_
    ver_char <- NA_character_
    installerType <- NA_character_
    
    if (isTRUE(osgeo4w)) {
      
      bn <- basename(bf)
      
      # common launcher names (adjust if you want more)
      if (tolower(bn) %in% c("grass78.bat", "grass79.bat", "grass83.bat", "grass84.bat")) {
        root_dir <- dirname(dirname(bf))           # OSGeo4W root
        ver_char <- gsub("[^0-9]", "", bn)         # "83" from "grass83.bat"
        installerType <- "osgeo4w"
      } else {
        
        # try to read OSGEO4W_ROOT=...
        rd <- grep("OSGEO4W_ROOT", batchfile_lines, value = TRUE)
        if (length(rd) > 0) {
          rd <- rd[1]
          pos <- regexpr("=", rd, fixed = TRUE)
          if (pos > 0) root_dir <- substr(rd, pos + 1, nchar(rd))
        }
        
        # try to infer version from env.bat path fragment containing grass-*
        vv <- grep("\\benv\\.bat\\b", batchfile_lines, value = TRUE)
        if (length(vv) > 0 && !is.na(root_dir)) {
          vv <- vv[1]
          m <- regmatches(vv, regexpr("grass-[0-9]+(\\.[0-9]+)*", vv))
          if (length(m) > 0) ver_char <- m
        }
        
        installerType <- "osgeo4w"
      }
    }
    
    if (isTRUE(nsis)) {
      rd <- grep("set GISBASE=", batchfile_lines, value = TRUE)
      if (length(rd) > 0) {
        rd <- rd[1]
        pos <- regexpr("=", rd, fixed = TRUE)
        if (pos > 0) root_dir <- substr(rd, pos + 1, nchar(rd))
      }
      if (!is.na(root_dir)) {
        m <- regmatches(root_dir, regexpr("[0-9]+(\\.[0-9]+)+", root_dir))
        if (length(m) > 0) ver_char <- m
      }
      installerType <- "nsis"
    }
    
    if (!is.na(root_dir) && nzchar(root_dir) && file.exists(root_dir)) {
      data.frame(
        instDir = root_dir,
        version = ver_char,
        installation_type = tolower(installerType),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })
  
  installations_GRASS <- do.call(rbind, installations_GRASS)
  
  if (is.null(installations_GRASS) || nrow(installations_GRASS) == 0) {
    if (!quiet) cat("Did not find any valid GRASS installation at mount point", DL, "\n")
    return(FALSE)
  }
  
  installations_GRASS
}

#' @title Get GRASS GIS parameters on Windows
#'
#' @description
#' Detects available GRASS GIS installations on Windows systems, selects
#' a suitable installation, and optionally activates the GRASS runtime
#' environment for use with \pkg{rgrass}.
#'
#' This function is primarily intended for internal use by
#' \code{\link{linkGRASS}} and related helpers.
#'
#' @details
#' The function searches for valid GRASS GIS installations using
#' \code{\link{findGRASS}}. If multiple installations are found, the selection
#' can be controlled via \code{ver_select}.
#'
#' If \code{activate = TRUE}, the GRASS environment variables are initialized
#' via \code{\link{setenvGRASSw}}, enabling immediate use of GRASS GIS through
#' \pkg{rgrass} and command-line calls.
#'
#' No spatial data object is required at this stage; spatial initialization
#' is handled later by \code{\link{linkGRASS}}.
#'
#' @param set_default_GRASS Optional predefined GRASS installation.
#'   Must be coercible to a data frame with columns
#'   \code{instDir}, \code{version}, and \code{installation_type}.
#'
#' @param DL Character. Search root for GRASS installations.
#'   Defaults to \code{"C:/"}.
#'
#' @param ver_select Logical or numeric.
#'   If \code{FALSE} (default), the latest version is selected automatically.
#'   If numeric, selects the corresponding installation index.
#'   If \code{TRUE}, interactive selection is used.
#'
#' @param quiet Logical. If \code{TRUE} (default), suppress informational messages.
#'
#' @param activate Logical. If \code{TRUE} (default), activate the selected
#'   GRASS GIS environment.
#'
#' @param sysname Character. Internal/testing hook overriding
#'   \code{Sys.info()[["sysname"]]}.
#'
#' @return
#' A named list with elements:
#' \describe{
#'   \item{gisbase_GRASS}{Path to the selected GRASS GIS base directory.}
#'   \item{version}{Detected GRASS GIS version string (may be \code{NA}).}
#'   \item{type}{Installation type (e.g. \code{"osgeo4w"}, \code{"standalone"}).}
#'   \item{installed}{Data frame of all detected installations.}
#'   \item{exist}{Logical indicating whether a valid installation was found.}
#' }
#'
#' If no installation is found or the platform is not Windows,
#' \code{list(exist = FALSE)} is returned.
#'
#' @keywords internal
#'
#' @seealso
#' \code{\link{findGRASS}}, \code{\link{setenvGRASSw}}, \code{\link{linkGRASS}}
#'
#' @examples
#' \dontrun{
#' # automatic detection and activation
#' g <- paramGRASSw()
#'
#' # select a specific installation by index
#' g <- paramGRASSw(ver_select = 1)
#'
#' # predefined installation (advanced use)
#' g <- paramGRASSw(
#'   set_default_GRASS = c("C:/OSGeo4W64", "8.3.2", "osgeo4w")
#' )
#' }
paramGRASSw <- function(set_default_GRASS = NULL,
                        DL = "C:/",
                        ver_select = FALSE,
                        quiet = TRUE,
                        activate = TRUE,
                        sysname = Sys.info()[["sysname"]]) {
  
  # ------------------------------------------------------------
  # 1) Platform guard (testable, no side effects)
  # ------------------------------------------------------------
  if (sysname != "Windows") {
    return(list(exist = FALSE))
  }
  
  # normalize logical flags that may come as "T"/"F"
  if (identical(ver_select, "T")) ver_select <- TRUE
  if (identical(ver_select, "F")) ver_select <- FALSE
  
  # ------------------------------------------------------------
  # 2) Detect GRASS installations
  # ------------------------------------------------------------
  if (is.null(set_default_GRASS)) {
    if (is.null(DL) || identical(DL, "default")) DL <- "C:/"
    params_GRASS <- findGRASS(searchLocation = DL, quiet = quiet)
  } else {
    params_GRASS <- rbind.data.frame(set_default_GRASS)
    names(params_GRASS) <- c("instDir", "version", "installation_type")
  }
  
  if (isFALSE(params_GRASS) ||
      !is.data.frame(params_GRASS) ||
      nrow(params_GRASS) < 1) {
    return(list(exist = FALSE))
  }
  
  # ------------------------------------------------------------
  # 3) Select installation index (extracted logic)
  # ------------------------------------------------------------
  n <- nrow(params_GRASS)
  sel_idx <- 1L
  
  if (n == 1) {
    sel_idx <- 1L
    
  } else if (is.numeric(ver_select) &&
             ver_select >= 1 &&
             ver_select <= n) {
    
    sel_idx <- as.integer(ver_select)
    
    if (!quiet) {
      message("Multiple GRASS installations detected:")
      print(params_GRASS)
      message("Selected installation: ", sel_idx)
    }
    
  } else if (!isTRUE(ver_select)) {
    
    v <- suppressWarnings(numeric_version(params_GRASS$version))
    sel_idx <- which(v == max(v))[1]
    
    if (!quiet) {
      message("Multiple GRASS installations detected.")
      message("Automatically selected latest version (", sel_idx, ").")
    }
    
  } else {
    
    message("Multiple GRASS installations detected:")
    print(params_GRASS)
    ver <- suppressWarnings(as.numeric(readline("Please select one: ")))
    
    if (is.na(ver) || ver < 1 || ver > n) {
      stop("Invalid GRASS version selection.")
    }
    
    sel_idx <- as.integer(ver)
  }
  
  # ------------------------------------------------------------
  # 4) Normalize selected installation metadata
  # ------------------------------------------------------------
  instDir <- as.character(params_GRASS$instDir[[sel_idx]])
  version <- params_GRASS$version[[sel_idx]]
  type    <- tolower(as.character(params_GRASS$installation_type[[sel_idx]]))
  
  # ------------------------------------------------------------
  # 5) Optional environment activation (side effects isolated)
  # ------------------------------------------------------------
  if (isTRUE(activate)) {
    gisbase_GRASS <- normalizePath(
      setenvGRASSw(
        root_GRASS = instDir,
        grass_version = version,
        installation_type = type,
        quiet = quiet
      ),
      winslash = "/",
      mustWork = FALSE
    )
  } else {
    gisbase_GRASS <- normalizePath(instDir, winslash = "/", mustWork = FALSE)
  }
  
  # ------------------------------------------------------------
  # 6) Stable, cross-platform return object
  # ------------------------------------------------------------
  grass <- list(
    gisbase_GRASS = gsub("\\\\", "/", gisbase_GRASS),
    version       = if (!nzchar(version)) NA_character_ else version,
    type          = if (!nzchar(type)) NA_character_ else type,
    installed     = params_GRASS,
    exist         = TRUE
  )
  
  grass
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
  
  # keep your existing path normalizer (must exist in your package)
  DL <- .bf_wpath(DL)
  
  if (!quiet) {
    cat("\nsearching for GRASS installations - this may take a while\n")
    cat("For providing the path manually see ?searchGRASSW \n")
  }
  
  # ---- run bounded Windows search via WHERE ----
  raw_GRASS <- try(
    .link2gi_sys2(
      "cmd.exe",
      args   = c("/c", "where", "/R", DL, "grass*.bat"),
      stdout = TRUE,
      stderr = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(raw_GRASS, "try-error") || length(raw_GRASS) == 0) {
    if (!quiet) {
      message("::: NO GRASS installation found at: '", DL, "'")
      message("::: NOTE:  Links or symbolic links like 'C:/Documents' are not searched...")
    }
    return(FALSE)
  }
  
  # normalize typical WHERE output noise / localized "not found" messages
  raw_GRASS <- trimws(raw_GRASS)
  raw_GRASS <- raw_GRASS[nzchar(raw_GRASS)]
  
  not_found <- any(
    grepl("File not found", raw_GRASS, fixed = TRUE) |
      grepl("Datei nicht gefunden", raw_GRASS, fixed = TRUE) |
      grepl("INFORMATION:", raw_GRASS, fixed = TRUE) |
      grepl("FEHLER:", raw_GRASS, fixed = TRUE) |
      grepl("ERROR:", raw_GRASS, fixed = TRUE)
  )
  
  if (isTRUE(not_found) || length(raw_GRASS) == 0) {
    if (!quiet) {
      message("::: NO GRASS installation found at: '", DL, "'")
      message("::: NOTE:  Links or symbolic links like 'C:/Documents' are not searched...")
    }
    return(FALSE)
  }
  
  installations_GRASS <- lapply(seq_along(raw_GRASS), function(i) {
    
    bat <- raw_GRASS[i]
    bat_sp <- utils::shortPathName(bat)
    
    batchfile_lines <- try(
      .link2gi_sys2(
        "cmd.exe",
        args   = c("/c", "type", bat_sp),
        stdout = TRUE,
        stderr = TRUE
      ),
      silent = TRUE
    )
    
    if (inherits(batchfile_lines, "try-error") || length(batchfile_lines) == 0) {
      return(NULL)
    }
    
    osgeo4w <- length(unique(grep("OSGEO4W", batchfile_lines, value = TRUE))) > 0
    stand_alone <- length(unique(grep("NSIS installer", batchfile_lines, value = TRUE))) > 0
    
    root_dir <- ""
    ver_char <- NA_character_
    installerType <- NA_character_
    
    if (isTRUE(osgeo4w)) {
      
      bn <- basename(bat_sp)
      
      if (bn %in% c("grass78.bat", "grass79.bat", "grass83.bat")) {
        
        root_dir <- dirname(dirname(bat_sp))
        ver_char <- substr(bn, 6, 7)
        installerType <- "osgeo4W"
        
      } else {
        
        if (length(grep("PREREM~1", bat_sp)) == 0 &&
            length(grep("extrabin", bat_sp)) == 0) {
          
          root_dir <- unique(grep("OSGEO4W_ROOT", batchfile_lines, value = TRUE))
          if (length(root_dir) > 0) {
            eq <- gregexpr(pattern = "=", root_dir[[1]])[[1]][1]
            if (!is.na(eq) && eq > 0) root_dir <- substr(root_dir[[1]], eq + 1, nchar(root_dir[[1]]))
          }
          
          ver_char <- unique(grep("\\benv.bat\\b", batchfile_lines, value = TRUE))
          if (length(root_dir) > 0 && length(ver_char) > 0) {
            p0 <- gregexpr(pattern = "\\grass-", ver_char[[1]])[[1]][1]
            if (!is.na(p0) && p0 > 0) {
              tmp <- substr(ver_char[[1]], p0, nchar(ver_char[[1]]))
              p1 <- gregexpr(pattern = "\\\\", tmp)[[1]][1]
              if (!is.na(p1) && p1 > 1) ver_char <- substr(tmp, 1, p1 - 1)
            }
          }
        }
        
        installerType <- "osgeo4W"
      }
    }
    
    if (isTRUE(stand_alone)) {
      
      root_dir <- unique(grep("set GISBASE=", batchfile_lines, value = TRUE))
      if (length(root_dir) > 0) {
        eq <- gregexpr(pattern = "=", root_dir[[1]])[[1]][1]
        if (!is.na(eq) && eq > 0) root_dir <- substr(root_dir[[1]], eq + 1, nchar(root_dir[[1]]))
      }
      
      ver_char <- root_dir
      if (length(root_dir) > 0) {
        p0 <- gregexpr(pattern = "GRASS", ver_char[[1]])[[1]][1]
        if (!is.na(p0) && p0 > 0) ver_char <- substr(ver_char[[1]], p0, nchar(ver_char[[1]]))
      }
      
      installerType <- "NSIS"
    }
    
    # final existence check
    exist <- FALSE
    if (length(root_dir) > 0 && !is.na(root_dir) && nzchar(root_dir)) {
      root_dir <- root_dir[[1]]
      exist <- .link2gi_dir_exists(root_dir) || .link2gi_file_exists(root_dir)
    }
    
    if (isTRUE(exist)) {
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
  
  installations_GRASS
}




#' Search for valid GRASS GIS installation(s) on Unix (Linux/macOS)
#'
#' Strategy:
#' 1) Prefer `grass --config path` (returns GISBASE on modern GRASS)
#' 2) Fallback: locate candidates under common roots (/usr/lib, /usr/local/lib, /opt)
#'
#' @param MP Character. Optional hint: directory or executable path.
#' @param quiet Logical.
#' @return FALSE or data.frame(instDir, version, installation_type)
#' @export
#' @keywords internal
searchGRASSX <- function(MP = "default", quiet = TRUE) {
  
  .msg <- function(...) if (!isTRUE(quiet)) message(...)
  
  # 0) Hint handling
  hint_dir <- NULL
  if (!is.null(MP) && !identical(MP, "default")) {
    if (.link2gi_file_exists(MP) && !.link2gi_dir_exists(MP)) hint_dir <- dirname(MP)
    if (.link2gi_dir_exists(MP)) hint_dir <- MP
  }
  
  # 1) Locate grass executable
  grass_exe <- unname(.link2gi_which("grass")[1])
  if (!nzchar(grass_exe) && !is.null(hint_dir)) {
    cand <- file.path(hint_dir, "grass")
    if (.link2gi_file_exists(cand)) grass_exe <- cand
  }
  
  if (!nzchar(grass_exe)) {
    .msg("searchGRASSX(): 'grass' not found on PATH.")
    return(FALSE)
  }
  
  # 2) Ask GRASS for GISBASE (preferred)
  gisbase_out <- try(.link2gi_sys2(grass_exe, c("--config", "path"), stdout = TRUE, stderr = TRUE), silent = TRUE)
  
  if (!inherits(gisbase_out, "try-error") && length(gisbase_out) > 0) {
    gisbase <- trimws(gisbase_out[1])
    if (nzchar(gisbase) && .link2gi_dir_exists(gisbase)) {
      
      # Version (best-effort)
      ver_out <- try(.link2gi_sys2(grass_exe, "--version", stdout = TRUE, stderr = TRUE), silent = TRUE)
      ver_char <- NA_character_
      if (!inherits(ver_out, "try-error") && length(ver_out) > 0) {
        m <- regmatches(ver_out[1], regexpr("[0-9]+(\\.[0-9]+)+", ver_out[1]))
        if (length(m) > 0) ver_char <- m
      }
      
      return(data.frame(
        instDir = gisbase,
        version = ver_char,
        installation_type = basename(grass_exe),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # 3) Fallback candidates (last resort)
  roots <- c("/usr/lib", "/usr/local/lib", "/opt")
  cand <- unlist(lapply(roots, function(r) .link2gi_glob(file.path(r, "grass*"))), use.names = FALSE)
  cand <- cand[nzchar(cand)]
  cand <- cand[.link2gi_dir_exists(cand)]
  
  # Keep only things that look like GISBASE
  ok <- vapply(cand, function(p) {
    .link2gi_dir_exists(file.path(p, "etc")) &&
      (.link2gi_file_exists(file.path(p, "etc", "VERSIONNUMBER")) || .link2gi_dir_exists(file.path(p, "scripts")))
  }, logical(1))
  cand <- cand[ok]
  
  if (length(cand) == 0) {
    .msg("searchGRASSX(): could not resolve GISBASE via '--config path' and no fallback candidates found.")
    return(FALSE)
  }
  
  # Choose highest numeric suffix if present (grass84 > grass83); otherwise first
  suf <- suppressWarnings(as.integer(gsub(".*grass", "", basename(cand), ignore.case = TRUE)))
  ord <- order(ifelse(is.na(suf), -Inf, suf), decreasing = TRUE)
  cand <- cand[ord]
  
  return(data.frame(
    instDir = gisbase,
    version = ver_char,
    installation_type = "system",
    stringsAsFactors = FALSE
  ))
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





#' @title Find GRASS GIS installation(s)
#'
#' @description
#' Detects valid GRASS GIS installation(s) on the current system and returns
#' their installation paths and version information.
#'
#' On Windows, \code{\link{searchGRASSW}} is used. On Unix-alike systems
#' (Linux/macOS), \code{\link{searchGRASSX}} is used.
#'
#' @param searchLocation Character. Search hint/root.
#' \itemize{
#'   \item On Windows this MUST include a drive letter and colon, e.g.
#'         \code{"C:"}, \code{"C:/"}, \code{"C:/OSGeo4W64"}.
#'         The default is \code{"C:/"}.
#'   \item On Unix the default is \code{"/usr/bin"}. You may pass either a
#'         directory or a full path to a \code{grass} executable; if a file is
#'         provided, its directory is used as hint.
#' }
#'
#' @param ver_select Logical. If \code{TRUE} and more than one installation is
#' found, a selection prompt is shown and only the chosen row is returned.
#'
#' @param quiet Logical. If \code{TRUE} (default), suppress informational messages.
#'
#' @return
#' Returns \code{FALSE} if no installation is detected.
#' Otherwise returns a \code{data.frame} with columns:
#' \describe{
#'   \item{instDir}{Installation directory (GISBASE).}
#'   \item{version}{Parsed version string (may be \code{NA}).}
#'   \item{installation_type}{Installation type identifier (e.g. \code{"osgeo4w"},
#'   \code{"standalone"}; on Unix this may be derived from the executable name).}
#' }
#'
#' @keywords internal
#' @export
#'
#' @seealso
#' \code{\link{searchGRASSW}}, \code{\link{searchGRASSX}}, \code{\link{linkGRASS}}
#'
#' @examples
#' \dontrun{
#' # Windows
#' findGRASS("C:/", quiet = FALSE)
#'
#' # Unix: hint directory
#' findGRASS("/usr/bin", quiet = FALSE)
#'
#' # Unix: explicit executable path
#' findGRASS("/usr/bin/grass", quiet = FALSE)
#' }
findGRASS <- function(searchLocation = "default", ver_select = FALSE, quiet = TRUE) {
  .msg <- function(...) if (!isTRUE(quiet)) message(...)
  
  if (Sys.info()[["sysname"]] == "Windows") {
    
    if (identical(searchLocation, "default") || is.null(searchLocation)) {
      searchLocation <- "C:/"
    } else {
      searchLocation <- normalizePath(path.expand(searchLocation), winslash = "/", mustWork = FALSE)
    }
    
    if (is.na(searchLocation) || !grepl("^[A-Za-z]:(/|$)", searchLocation)) {
      .msg(
        "You are running Windows - Please choose a suitable searchLocation argument ",
        "that MUST include a Windows drive letter and colon"
      )
      return(FALSE)
    }
    
    link <- searchGRASSW(DL = searchLocation, quiet = quiet)
    
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
    
  } else {
    
    if (identical(searchLocation, "default") || is.null(searchLocation)) {
      searchLocation <- "/usr/bin"
    }
    
    if (file.exists(searchLocation) && !dir.exists(searchLocation)) {
      searchLocation <- dirname(normalizePath(searchLocation, winslash = "/", mustWork = TRUE))
    }
    
    if (grepl(":", searchLocation, fixed = TRUE)) {
      .msg("You are running Linux/Unix - please choose a suitable searchLocation argument")
      return(FALSE)
    }
    
    # IMPORTANT: no link2GI:: prefix inside the package
    link <- searchGRASSX(MP = searchLocation, quiet = quiet)
  }
  
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
