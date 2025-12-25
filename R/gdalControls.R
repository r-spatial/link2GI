#'@title Usually for internally usage, initializes and set up access to the 'GDAL' command line interface
#'@name setenvGDAL
#'@description Initializes and sets up access to the 'GDAL' command line interface
#'
#'@param bin_GDAL Character. Path to GDAL bin directory OR full path to gdalinfo(.exe).
#'@return The normalized GDAL bin directory path (invisibly).
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' setenvGDAL("C:/OSGeo4W64/bin")
#' }
setenvGDAL <- function(bin_GDAL = NULL) {
  
  sys <- Sys.info()[["sysname"]]
  
  if (is.null(bin_GDAL) || !nzchar(bin_GDAL)) {
    stop("setenvGDAL(): 'bin_GDAL' must be a non-empty path.", call. = FALSE)
  }
  
  bin_GDAL <- path.expand(as.character(bin_GDAL))
  
  # allow passing gdalinfo(.exe) instead of bin directory
  if (file.exists(bin_GDAL) && !dir.exists(bin_GDAL)) {
    bin_GDAL <- dirname(bin_GDAL)
  }
  
  bin_GDAL <- normalizePath(bin_GDAL, winslash = "/", mustWork = FALSE)
  
  if (!dir.exists(bin_GDAL)) {
    stop("setenvGDAL(): bin directory does not exist: ", bin_GDAL, call. = FALSE)
  }
  
  # ensure package env exists (keep your legacy behavior)
  if (!exists("GiEnv", inherits = FALSE)) {
    GiEnv <- new.env(parent = globalenv())
  }
  
  # add to PATH (your legacy helper)
  add2Path(bin_GDAL)
  
  # --- determine GDAL_DATA robustly ---
  gdal_data <- NA_character_
  
  if (!identical(sys, "Windows")) {
    
    # Prefer gdal-config --datadir (most correct on Unix)
    gdal_config <- file.path(bin_GDAL, "gdal-config")
    if (!file.exists(gdal_config)) gdal_config <- "gdal-config"
    
    out <- try(
      system2(gdal_config, args = "--datadir", stdout = TRUE, stderr = TRUE),
      silent = TRUE
    )
    
    if (!inherits(out, "try-error") && length(out) > 0) {
      cand <- trimws(out[1])
      if (nzchar(cand) && dir.exists(cand)) gdal_data <- cand
    }
    
  } else {
    
    # Windows heuristics (OSGeo4W/QGIS layouts)
    # Typical OSGeo4W: <root>/share/gdal
    # Typical QGIS:    <root>/share/gdal or <root>/apps/qgis*/share/gdal
    root <- normalizePath(file.path(bin_GDAL, ".."), winslash = "/", mustWork = FALSE)
    
    cands <- c(
      file.path(root, "share", "gdal"),
      file.path(root, "apps", "qgis", "share", "gdal"),
      file.path(root, "apps", "qgis-ltr", "share", "gdal")
    )
    
    hit <- cands[dir.exists(cands)][1]
    if (length(hit) && nzchar(hit)) gdal_data <- hit
  }
  
  if (!is.na(gdal_data) && nzchar(gdal_data)) {
    Sys.setenv(GDAL_DATA = gdal_data)
  } else {
    # do not hard-fail; but don’t set nonsense either
    # leave GDAL_DATA untouched if we cannot resolve it
  }
  
  invisible(bin_GDAL)
}



#' Search recursively for valid GDAL installation(s) on Windows
#'
#' Finds `gdalinfo.exe` via `where /R`, derives `binDir` and `baseDir`,
#' classifies the installation type by path heuristics, and lists available
#' GDAL CLI tools (`gdal*`) + python tools (`*.py`) in each `binDir`.
#'
#' @param DL Character. Search root (e.g. `"C:/"`).
#' @param quiet Logical. Suppress messages.
#'
#' @return A list with:
#' \describe{
#'   \item{gdalInstallations}{data.frame with columns `binDir`, `baseDir`, `installation_type`.}
#'   \item{bin}{list of data.frames (column `gdal_bin`) with detected GDAL binaries per installation.}
#'   \item{py}{list of data.frames (column `gdal_py`) with detected GDAL python tools per installation.}
#' }
#'
#' @keywords internal
#' @export
searchGDALW <- function(DL = "C:/", quiet = TRUE) {
  
  if (!identical(Sys.info()[["sysname"]], "Windows")) {
    if (!quiet) message("searchGDALW(): Windows only.")
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
  
  # keep your existing Windows path normalizer (must exist in your package)
  DL <- .bf_wpath(DL)
  
  if (!quiet) {
    cat("\nsearching for GDAL installations - this may take a while\n")
    cat("For providing the path manually see ?searchGDALW \n")
  }
  
  # Use wrapper if present; fall back to system2 otherwise
  sys2 <- function(cmd, args, ...) {
    if (exists(".link2gi_sys2", mode = "function")) {
      return(.link2gi_sys2(cmd, args = args, ...))
    }
    system2(cmd, args = args, ...)
  }
  
  raw <- try(
    sys2(
      "cmd.exe",
      args   = c("/c", "where", "/R", DL, "gdalinfo.exe"),
      stdout = TRUE,
      stderr = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(raw, "try-error") || length(raw) == 0) {
    if (!quiet) message("::: NO GDAL installation found at: '", DL, "'")
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
  
  raw <- trimws(raw)
  raw <- raw[nzchar(raw)]
  
  # Filter common WHERE noise / localized messages
  bad <- grepl("File not found", raw, fixed = TRUE) |
    grepl("Datei nicht gefunden", raw, fixed = TRUE) |
    grepl("INFORMATION:", raw, fixed = TRUE) |
    grepl("FEHLER:", raw, fixed = TRUE) |
    grepl("ERROR:", raw, fixed = TRUE)
  raw <- raw[!bad]
  
  if (!length(raw)) {
    if (!quiet) message("::: NO GDAL installation found at: '", DL, "'")
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
  
  normp <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  
  classify_type <- function(p) {
    lp <- tolower(p)
    if (grepl("osgeo4w64", lp, fixed = TRUE)) return("osgeo4w64")
    if (grepl("osgeo4w",   lp, fixed = TRUE)) return("osgeo4w")
    if (grepl("qgis",      lp, fixed = TRUE)) return("qgis")
    if (grepl("conda",     lp, fixed = TRUE) || grepl("miniconda", lp, fixed = TRUE)) return("conda")
    if (grepl("grass",     lp, fixed = TRUE)) return("grass")
    if (grepl("otb",       lp, fixed = TRUE)) return("otb")
    if (grepl("gdal",      lp, fixed = TRUE)) return("gdal")
    "system"
  }
  
  # Build installation rows from gdalinfo.exe hits
  rows <- lapply(raw, function(gdalinfo_exe) {
    gdalinfo_exe <- normp(gdalinfo_exe)
    binDir  <- normp(dirname(gdalinfo_exe))
    baseDir <- normp(file.path(binDir, ".."))
    
    if (!dir.exists(binDir)) return(NULL)
    
    data.frame(
      binDir = binDir,
      baseDir = baseDir,
      installation_type = classify_type(gdalinfo_exe),
      stringsAsFactors = FALSE
    )
  })
  
  inst_df <- do.call(rbind, rows)
  if (is.null(inst_df) || nrow(inst_df) == 0) {
    if (!quiet) message("::: NO valid GDAL binDir detected.")
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
  
  # de-duplicate by binDir
  inst_df <- inst_df[!duplicated(inst_df$binDir), , drop = FALSE]
  rownames(inst_df) <- seq_len(nrow(inst_df))
  
  # List tools alongside each binDir
  gdal_bin <- vector("list", nrow(inst_df))
  gdal_py  <- vector("list", nrow(inst_df))
  
  for (i in seq_len(nrow(inst_df))) {
    bd <- inst_df$binDir[i]
    g  <- Sys.glob(file.path(bd, "gdal*"))
    g  <- normp(g)
    
    pys  <- g[grepl("\\.py$", g, ignore.case = TRUE)]
    bins <- g[!grepl("\\.py$", g, ignore.case = TRUE)]
    
    gdal_bin[[i]] <- data.frame(gdal_bin = bins, stringsAsFactors = FALSE)
    gdal_py[[i]]  <- data.frame(gdal_py  = pys,  stringsAsFactors = FALSE)
  }
  
  list(
    gdalInstallations = inst_df,
    bin = gdal_bin,
    py  = gdal_py
  )
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


#' Find GDAL installation(s)
#'
#' @description
#' Detects GDAL command line installations on Windows and Unix-like systems
#' (Linux/macOS). On Windows, the search is restricted to valid drive-letter
#' paths. On Unix systems, the function searches common system locations for
#' a `gdalinfo` executable.
#'
#' This function is a thin, platform-aware dispatcher that calls
#' [searchGDALW()] on Windows and [searchGDALX()] on Unix systems.
#'
#' @param searchLocation Character. Search root or hint.
#'   Use `"default"` (recommended) to search standard locations
#'   (`"C:/"` on Windows; `"~"`, `"/usr"`, `"/usr/local"`, `"/opt"` on Unix).
#' @param quiet Logical. If `TRUE`, suppress messages.
#'
#' @return
#' Platform-specific GDAL installation descriptor:
#' \itemize{
#'   \item On Windows: the result of [searchGDALW()] or `FALSE`
#'   \item On Unix: the result of [searchGDALX()]
#' }
#'
#' @keywords internal
#' @export

findGDAL <- function(searchLocation = "default", quiet = TRUE) {
  
  sys <- Sys.info()[["sysname"]]
  
  if (identical(sys, "Windows")) {
    
    if (identical(searchLocation, "default") || is.null(searchLocation)) {
      searchLocation <- "C:/"
    } else {
      searchLocation <- normalizePath(searchLocation, winslash = "/", mustWork = FALSE)
    }
    
    if (!grepl("^[A-Za-z]:(/|$)", searchLocation)) {
      if (!quiet) message("Windows requires searchLocation with drive letter, e.g. 'C:/'")
      return(FALSE)
    }
    
    return(searchGDALW(DL = searchLocation, quiet = quiet))
  }
  
  if (identical(searchLocation, "default") || is.null(searchLocation)) {
    searchLocation <- c("~", "/usr", "/usr/local", "/opt")
  }
  
  searchGDALX(MP = searchLocation, quiet = quiet)
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
      if (numeric_version(tmp2) > numeric_version(highestVer)) {
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
