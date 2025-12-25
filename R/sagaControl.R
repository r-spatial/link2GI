#' Search for valid SAGA GIS installation(s) on Unix (Linux/macOS)
#'
#' Strategy:
#' 1) Search for `saga_cmd` under given mountpoint(s) using portable `find` primaries
#'    (no GNU-only `-executable`, no `! -readable`).
#' 2) For each hit, derive `binDir` and `moduleDir` by checking sibling folders
#'    (`tools` preferred, then `modules`).
#' 3) Parse version best-effort via `saga_cmd --version`.
#'
#' @param MP Character. Search root(s). `"default"` expands to `c("~","/opt","/usr/local","/usr")`.
#' @param quiet Logical. Suppress messages.
#'
#' @return `FALSE` or a `data.frame` with columns `binDir`, `moduleDir`, `version`, `installation_type`.
#' @keywords internal
#' @export
searchSAGAX <- function(MP = "default", quiet = TRUE) {
  
  .msg <- function(...) if (!isTRUE(quiet)) message(...)
  
  if (identical(Sys.info()[["sysname"]], "Windows")) {
    .msg("searchSAGAX(): Windows detected; use searchSAGAW().")
    return(FALSE)
  }
  
  if (identical(MP, "default") || is.null(MP)) {
    MP <- c("~", "/opt", "/usr/local", "/usr")
  } else {
    MP <- as.character(MP)
  }
  
  MP <- path.expand(MP)
  MP <- MP[.link2gi_dir_exists(MP)]
  if (!length(MP)) {
    .msg("searchSAGAX(): no valid mountpoints.")
    return(FALSE)
  }
  
  find_saga_cmd <- function(root) {
    out <- suppressWarnings(try(
      .link2gi_sys2(
        "find",
        args   = c(root, "-type", "f", "-name", "saga_cmd", "-perm", "-111", "-print"),
        stdout = TRUE,
        stderr = TRUE
      ),
      silent = TRUE
    ))
    if (inherits(out, "try-error") || !length(out)) return(character(0))
    out <- trimws(out)
    out <- out[nzchar(out)]
    out <- out[!grepl("Permission denied", out, fixed = TRUE)]
    unique(out)
  }
  
  hits <- unique(unlist(lapply(MP, find_saga_cmd), use.names = FALSE))
  
  if (!length(hits)) {
    .msg("searchSAGAX(): no saga_cmd found.")
    return(FALSE)
  }
  
  classify_type <- function(p) {
    lp <- tolower(p)
    if (grepl("/osgeo4w", lp, fixed = TRUE)) return("osgeo4w")
    if (grepl("/qgis",    lp, fixed = TRUE)) return("qgis")
    if (grepl("/conda",   lp, fixed = TRUE) || grepl("/miniconda", lp, fixed = TRUE)) return("conda")
    if (grepl("/otb",     lp, fixed = TRUE)) return("otb")
    "system"
  }
  
  get_version <- function(exe) {
    vout <- suppressWarnings(try(
      .link2gi_sys2(exe, args = "--version", stdout = TRUE, stderr = TRUE),
      silent = TRUE
    ))
    if (inherits(vout, "try-error") || !length(vout)) return(NA_character_)
    # typically: "SAGA Version: x.y.z"
    m <- regmatches(vout[1], regexpr("[0-9]+(\\.[0-9]+)+", vout[1]))
    if (length(m) > 0) m else NA_character_
  }
  
  rows <- lapply(hits, function(exe) {
    
    exe <- normalizePath(exe, winslash = "/", mustWork = FALSE)
    binDir <- normalizePath(dirname(exe), winslash = "/", mustWork = FALSE)
    
    moduleDir <- NA_character_
    if (.link2gi_dir_exists(file.path(binDir, "tools"))) {
      moduleDir <- normalizePath(file.path(binDir, "tools"), winslash = "/", mustWork = FALSE)
    } else if (.link2gi_dir_exists(file.path(binDir, "modules"))) {
      moduleDir <- normalizePath(file.path(binDir, "modules"), winslash = "/", mustWork = FALSE)
    }
    
    data.frame(
      binDir = binDir,
      moduleDir = moduleDir,
      version = get_version(exe),
      installation_type = classify_type(exe),
      stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, rows)
  
  # de-duplicate by binDir
  df <- df[!duplicated(df$binDir), , drop = FALSE]
  rownames(df) <- NULL
  
  if (!nrow(df)) return(FALSE)
  df
}


#' Search for valid SAGA GIS installations on Windows
#'
#' Uses `where /R <DL> saga_cmd.exe` and derives `binDir` and `moduleDir`.
#'
#' @param DL Character. Search root (e.g. `"C:/"`).
#' @param quiet Logical. Suppress messages.
#'
#' @return `FALSE` or a `data.frame` with columns `binDir`, `moduleDir`, `installation_type`.
#' @keywords internal
#' @export
searchSAGAW <- function(DL = "C:/", quiet = TRUE) {
  
  # ---- OS guard first (prevents .bf_wpath() on non-Windows) ----
  if (!identical(Sys.info()[["sysname"]], "Windows")) {
    if (!quiet) message("searchSAGAW(): non-Windows detected.")
    return(FALSE)
  }
  
  DL <- .bf_wpath(DL)
  
  if (!quiet) {
    cat("\nsearching for SAGA GIS installations - this may take a while\n")
    cat("For providing the path manually see ?searchSAGAW \n")
  }
  
  raw <- suppressWarnings(try(
    .link2gi_sys2(
      "cmd.exe",
      args   = c("/c", "where", "/R", DL, "saga_cmd.exe"),
      stdout = TRUE,
      stderr = TRUE
    ),
    silent = TRUE
  ))
  
  if (inherits(raw, "try-error") || !length(raw)) return(FALSE)
  
  raw <- trimws(raw)
  raw <- raw[nzchar(raw)]
  
  not_found <- any(
    grepl("File not found", raw, fixed = TRUE) |
      grepl("Datei nicht gefunden", raw, fixed = TRUE) |
      grepl("INFORMATION:", raw, fixed = TRUE) |
      grepl("FEHLER:", raw, fixed = TRUE) |
      grepl("ERROR:", raw, fixed = TRUE)
  )
  if (isTRUE(not_found) || !length(raw)) return(FALSE)
  
  classify_type <- function(p) {
    lp <- tolower(p)
    if (grepl("osgeo4w64", lp, fixed = TRUE) || grepl("osgeo4w", lp, fixed = TRUE)) return("osgeo4w")
    if (grepl("qgis",     lp, fixed = TRUE)) return("qgis")
    if (grepl("conda",    lp, fixed = TRUE) || grepl("miniconda", lp, fixed = TRUE)) return("conda")
    "standalone"
  }
  
  rows <- lapply(raw, function(exe) {
    
    # shortPathName is in base, not utils
    exe <- utils::shortPathName(exe)
    exe <- normalizePath(exe, winslash = "/", mustWork = FALSE)
    
    binDir <- normalizePath(dirname(exe), winslash = "/", mustWork = FALSE)
    
    moduleDir <- NA_character_
    if (.link2gi_dir_exists(file.path(binDir, "tools"))) {
      moduleDir <- normalizePath(file.path(binDir, "tools"), winslash = "/", mustWork = FALSE)
    } else if (.link2gi_dir_exists(file.path(binDir, "modules"))) {
      moduleDir <- normalizePath(file.path(binDir, "modules"), winslash = "/", mustWork = FALSE)
    }
    
    data.frame(
      binDir = binDir,
      moduleDir = moduleDir,
      installation_type = classify_type(exe),
      stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, rows)
  df <- df[!duplicated(df$binDir), , drop = FALSE]
  rownames(df) <- NULL
  
  if (!nrow(df)) return(FALSE)
  df
}




#' Find SAGA GIS installation(s)
#'
#' @description
#' Detects SAGA GIS command line installations on Windows and Unix-like systems
#' (Linux/macOS). This is a thin, platform-aware dispatcher:
#' \itemize{
#'   \item Windows: calls [searchSAGAW()]
#'   \item Unix: calls [searchSAGAX()]
#' }
#'
#' @param searchLocation Character. Search root or hint.
#'   Use `"default"` to search standard locations (`"C:/"` on Windows; `"~"`,
#'   `"/usr"`, `"/usr/local"`, `"/opt"` on Unix).
#' @param quiet Logical. If `TRUE`, suppress messages.
#' @param sysname Character. Internal/testing hook overriding `Sys.info()[["sysname"]]`.
#'
#' @return
#' Platform-specific SAGA installation descriptor or `FALSE`.
#'
#' @keywords internal
#' @export
findSAGA <- function(searchLocation = "default",
                     quiet = TRUE,
                     sysname = Sys.info()[["sysname"]]) {
  
  if (identical(sysname, "Windows")) {
    
    if (identical(searchLocation, "default") || is.null(searchLocation)) {
      searchLocation <- "C:/"
    } else {
      searchLocation <- normalizePath(searchLocation, winslash = "/", mustWork = FALSE)
    }
    
    # Windows guard: needs drive letter
    if (!grepl("^[A-Za-z]:(/|$)", searchLocation)) {
      if (!quiet) message("Windows requires searchLocation with drive letter, e.g. 'C:/'")
      return(FALSE)
    }
    
    return(searchSAGAW(DL = searchLocation, quiet = quiet))
  }
  
  # Unix-like
  if (identical(searchLocation, "default") || is.null(searchLocation)) {
    searchLocation <- c("~", "/usr", "/usr/local", "/opt")
  }
  
  searchSAGAX(MP = searchLocation, quiet = quiet)
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