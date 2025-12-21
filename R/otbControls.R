#' Clean OTB argument list
#'
#' Removes inactive entries from an OTB argument list. Entries are dropped if
#' their value is NULL, NA, "NA", empty, or whitespace-only.
#'
#' @param x Named list of OTB arguments (e.g. built by otb_build_cmd()).
#'
#' @return A cleaned named list containing only active OTB parameters.
#'
#' @export
clean_otb_args <- function(x) {
  stopifnot(is.list(x), !is.null(names(x)))
  
  keep <- vapply(x, function(v) {
    if (is.null(v)) return(FALSE)
    if (length(v) == 0L) return(FALSE)
    
    if (length(v) == 1L) {
      if (is.na(v)) return(FALSE)
      if (is.character(v)) {
        vv <- trimws(v)
        if (!nzchar(vv)) return(FALSE)
        if (identical(vv, "NA")) return(FALSE)
      }
    }
    
    TRUE
  }, logical(1))
  
  x[keep]
}


# resolve an OTB CLI wrapper on Windows for an algorithm name
.resolve_otbcli_wrapper <- function(binDir, algo) {
  stopifnot(is.character(binDir), length(binDir) == 1L, nzchar(binDir))
  stopifnot(is.character(algo), length(algo) == 1L, nzchar(algo))
  
  # OTB naming variants we want to accept
  candidates <- c(
    file.path(binDir, paste0("otbcli_", algo)),          # legacy no-ext (rare)
    file.path(binDir, paste0("otbcli_", algo, ".exe")),
    file.path(binDir, paste0("otbcli_", algo, ".bat")),
    file.path(binDir, paste0("otbcli_", algo, ".ps1")),
    file.path(binDir, paste0("otbcli_", algo, ".cmd"))
  )
  
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit) || !nzchar(hit)) return(NA_character_)
  normalizePath(hit, winslash = "/", mustWork = FALSE)
}


#'@title  Usually for internally usage, initializes and set up  access to the 'OTB' command line interface
#'@name setenvOTB
#'@description  Initializes and set up  access to the 'OTB' command line interface
#'  
#'@param bin_OTB  string contains the path to the 'OTB' binaries
#'@param root_OTB string contains the full string to the root folder
#'  containing the 'OTB' installation'
#'@return Adds 'OTB' paths to the environment and creates the variable global string variable \code{otbCmd}, that contains the path to the 'OTB' binaries.
#'@keywords internal
#'  
#'@examples
#' \dontrun{
#'## example for the most common default OSGeo4W64 installation of OTB
#'setenvOTB(bin_OTB = 'C:\\OSGeo4W64\\bin\\',
#'           root_OTB = 'C:\\OSGeo4W64')
#'}
setenvOTB <- function(bin_OTB = NULL, root_OTB = NULL) {
  
  if (Sys.info()[["sysname"]] != "Windows") {
    return(bin_OTB)
  }
  
  if (is.null(bin_OTB) || !nzchar(bin_OTB)) {
    stop("setenvOTB(): 'bin_OTB' must be a non-empty path.")
  }
  
  # normalize paths (keep Windows slashes)
  bin_OTB <- normalizePath(bin_OTB, winslash = "\\", mustWork = FALSE)
  
  if (!is.null(root_OTB) && nzchar(root_OTB)) {
    root_OTB <- normalizePath(root_OTB, winslash = "\\", mustWork = FALSE)
  } else {
    root_OTB <- NA_character_
  }
  
  # package-local cache env (no .GlobalEnv pollution)
  ns <- asNamespace("link2GI")
  if (!exists(".link2GI_cache", envir = ns, inherits = FALSE)) {
    assign(".link2GI_cache", new.env(parent = emptyenv()), envir = ns)
  }
  .link2GI_cache <- get(".link2GI_cache", envir = ns, inherits = FALSE)
  
  # PATH update (required for OTB runtime)
  add2Path(bin_OTB)
  
  # OSGeo4W specifics (only if root provided and looks like OSGeo4W)
  if (!is.na(root_OTB) && grepl("OSGeo4W", root_OTB, ignore.case = TRUE)) {
    Sys.setenv(OSGEO4W_ROOT = root_OTB)
    
    # Some OSGeo4W installs rely on epsg_csv / gdal data being discoverable
    gt_csv <- file.path(root_OTB, "share", "epsg_csv")
    if (dir.exists(gt_csv)) Sys.setenv(GEOTIFF_CSV = gt_csv)
    
    # keep a tiny cache for debugging/inspection
    .link2GI_cache$OSGEO4W_ROOT <- root_OTB
    .link2GI_cache$GEOTIFF_CSV <- Sys.getenv("GEOTIFF_CSV")
  }
  
  # store bin path for internal use (again: package cache, not global)
  .link2GI_cache$otbPath <- bin_OTB
  
  return(bin_OTB)
}




#' Search recursively valid 'OTB' installation(s) on a given Windows drive/path
#'
#' Robust Windows OTB discovery for modern standalone bundles (OTB 9.x):
#' - detects root by presence of 'otbenv.ps1' (or 'otbenv.bat')
#' - checks for 'bin/otbApplicationLauncherCommandLine.exe'
#' - checks for at least one 'bin/otbcli_*.ps1' wrapper
#'
#' Backward compatible with legacy callers that use \code{DL} (as in older
#' link2GI versions). Internally, \code{DL} is treated as an alias for
#' \code{searchLocation}.
#'
#' @param searchLocation Character. Folder to search (default \code{"C:/"}).
#' @param DL Character. Backward-compatible alias for \code{searchLocation}.
#'   If both are provided, \code{searchLocation} wins.
#' @param maxdepth Integer. Max recursion depth (best effort).
#' @param quiet Logical. Suppress messages.
#' @return data.frame with columns:
#'   binDir, baseDir, otbCmd, envScript, launcher, installation_type
#' @export
searchOTBW <- function(searchLocation = NULL, DL = "C:/", maxdepth = 8L, quiet = FALSE) {
  
  # --- backward compat ---
  if (is.null(searchLocation) || !nzchar(searchLocation)) {
    searchLocation <- DL
  }
  
  stopifnot(is.character(searchLocation), length(searchLocation) == 1L, nzchar(searchLocation))
  maxdepth <- as.integer(maxdepth)
  if (is.na(maxdepth) || maxdepth < 1L) maxdepth <- 8L
  
  # ---- helpers ----
  normp <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  
  # best-effort "depth" filter
  depth_ok <- function(root, path, maxdepth) {
    root <- gsub("/+$", "", normp(root))
    path <- normp(path)
    rel  <- sub(paste0("^", gsub("([\\^\\$\\.|\\+\\(\\)\\[\\]\\{\\}\\\\])", "\\\\\\1", root), "/?"), "", path)
    if (!nzchar(rel)) return(TRUE)
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    d <- max(0L, length(parts) - 1L)
    d <= maxdepth
  }
  
  # minimal “is valid OTB root”
  validate_otb_root <- function(root) {
    root <- normp(root)
    
    env_ps1   <- file.path(root, "otbenv.ps1")
    env_bat   <- file.path(root, "otbenv.bat")
    bin_dir   <- file.path(root, "bin")
    launcher  <- file.path(bin_dir, "otbApplicationLauncherCommandLine.exe")
    
    has_env    <- file.exists(env_ps1) || file.exists(env_bat)
    has_bin    <- dir.exists(bin_dir)
    has_launch <- file.exists(launcher)
    
    # wrappers:
    cli_ps1 <- list.files(bin_dir, pattern = "^otbcli(_.*)?\\.ps1$", full.names = TRUE, ignore.case = TRUE)
    cli_bat <- list.files(bin_dir, pattern = "^otbcli(_.*)?\\.bat$", full.names = TRUE, ignore.case = TRUE)
    cli_exe <- list.files(bin_dir, pattern = "^otbcli(_.*)?\\.exe$", full.names = TRUE, ignore.case = TRUE)
    
    has_cli <- length(cli_ps1) > 0L || length(cli_bat) > 0L || length(cli_exe) > 0L
    
    list(
      root = root,
      binDir = normp(bin_dir),
      has_env = has_env,
      has_bin = has_bin,                # <---- FIX: this was missing
      envScript = if (file.exists(env_ps1)) normp(env_ps1) else if (file.exists(env_bat)) normp(env_bat) else NA_character_,
      has_launch = has_launch,
      launcher = if (file.exists(launcher)) normp(launcher) else NA_character_,
      has_cli = has_cli,
      otbCmd = {
        if (length(cli_bat)) normp(cli_bat[1])
        else if (length(cli_ps1)) normp(cli_ps1[1])
        else if (length(cli_exe)) normp(cli_exe[1])
        else NA_character_
      },
      installation_type = {
        if (length(cli_ps1) && file.exists(env_ps1) && file.exists(launcher)) "OTB_STANDALONE_PS1"
        else if (length(cli_bat)) "OTB_BAT"
        else if (length(cli_exe)) "OTB_EXE"
        else "OTB"
      }
    )
  }
  
  # ---- search ----
  root0 <- normp(searchLocation)
  if (!dir.exists(root0)) stop("searchLocation does not exist: ", root0)
  
  if (!quiet) {
    message("::: Searching OTB on Windows under: '", gsub("/", "\\\\", root0), "'")
  }
  
  env_hits <- list.files(root0, pattern = "^otbenv\\.(ps1|bat)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  env_hits <- env_hits[vapply(env_hits, function(p) depth_ok(root0, p, maxdepth), logical(1))]
  
  roots <- character(0)
  
  if (length(env_hits)) {
    roots <- unique(dirname(env_hits))
  } else {
    launch_hits <- list.files(root0, pattern = "^otbApplicationLauncherCommandLine\\.exe$", recursive = TRUE,
                              full.names = TRUE, ignore.case = TRUE)
    launch_hits <- launch_hits[vapply(launch_hits, function(p) depth_ok(root0, p, maxdepth), logical(1))]
    if (length(launch_hits)) {
      roots <- unique(normp(file.path(dirname(launch_hits), "..")))
    } else {
      cli_hits <- list.files(root0, pattern = "^otbcli(_.*)?\\.(ps1|bat|exe)$", recursive = TRUE,
                             full.names = TRUE, ignore.case = TRUE)
      cli_hits <- cli_hits[vapply(cli_hits, function(p) depth_ok(root0, p, maxdepth), logical(1))]
      if (length(cli_hits)) {
        roots <- unique(normp(file.path(dirname(cli_hits), "..")))
      }
    }
  }
  
  roots <- unique(roots)
  roots <- roots[dir.exists(roots)]
  
  if (!length(roots)) {
    if (!quiet) {
      message("::: NO OTB installation found at: '", gsub("/", "\\\\", root0), "'")
      message("::: NOTE: Links or symbolic links like 'C:/Documents' are searched...")
    }
    return(data.frame(
      binDir = character(0),
      baseDir = character(0),
      otbCmd = character(0),
      envScript = character(0),
      launcher = character(0),
      installation_type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  infos <- lapply(roots, validate_otb_root)
  infos <- infos[vapply(infos, function(x) isTRUE(x$has_env) && isTRUE(x$has_bin) && isTRUE(x$has_launch) && isTRUE(x$has_cli), logical(1))]
  
  if (!length(infos)) {
    if (!quiet) {
      message("::: Candidate roots found but none passed validation (env+bin+launcher+cli).")
    }
    return(data.frame(
      binDir = character(0),
      baseDir = character(0),
      otbCmd = character(0),
      envScript = character(0),
      launcher = character(0),
      installation_type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  df <- do.call(rbind, lapply(infos, function(x) {
    data.frame(
      binDir = x$binDir,
      baseDir = x$root,
      otbCmd = x$otbCmd,
      envScript = x$envScript,
      launcher = x$launcher,
      installation_type = x$installation_type,
      stringsAsFactors = FALSE
    )
  }))
  
  df <- df[!duplicated(df$baseDir), , drop = FALSE]
  rownames(df) <- NULL
  
  if (!quiet) {
    message("::: Found ", nrow(df), " OTB installation(s).")
  }
  
  df
}








#'@title Search recursively for valid 'OTB' installation(s) on a 'Linux' OS
#'@name searchOTBX
#'@description  Search for valid 'OTB' installations on a 'Linux' OS
#'@param MP default mount point is the home directory '~' (as suggested by the OTB team) 
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@return A dataframe with the 'OTB' root folder(s) the version name(s) and the installation type(s).
#'@author Chris Reudenbach
#'@export 
#'@keywords internal
#'
#'@examples
#' \dontrun{
#' # get all valid OTB installation folders and params
#' searchOTBX()
#' }
searchOTBX <- function(MP = "default", quiet = TRUE) {
  
  # 1) Default-Kandidaten (ohne Interaktion)
  if (identical(MP, "default")) {
    MP <- c("~", "/opt", "/usr/local", "/usr")
  } else {
    MP <- as.character(MP)
  }
  
  # 2) ~ expandieren und nur existierende Verzeichnisse behalten
  MP <- path.expand(MP)
  MP <- MP[file.exists(MP)]
  
  if (length(MP) == 0) {
    if (!quiet) message("No valid search mount points.")
    return(FALSE)
  }
  
  if (!quiet) {
    cat("\nsearching for Orfeo Toolbox installations in:\n")
    cat(paste0(" - ", MP, collapse = "\n"), "\n")
  }
  
  # 3) Für jedes Mountpoint find sauber via system2(args=...)
  hits <- unlist(lapply(MP, function(mp) {
    out <- suppressWarnings(try(system2(
      "find",
      args = c(mp, "-type", "f", "-executable", "-iname", "otbcli", "-print"),
      stdout = TRUE,
      stderr = TRUE
    ), silent = TRUE))
    
    if (inherits(out, "try-error") || length(out) == 0) return(character(0))
    out <- out[!grepl("Permission denied", out, fixed = TRUE)]
    out
  }), use.names = FALSE)
  
  hits <- unique(hits)
  if (length(hits) == 0) {
    if (!quiet) message("::: NO OTB installation found in default locations.")
    return(FALSE)
  }
  

  # 4) Treffer -> df (Linux contract aligned with Windows searchOTBW)
  otbInstallations <- lapply(seq_along(hits), function(i) {
    
    # hits[i] ends with ".../otbcli"
    binDir <- substr(hits[i], 1, regexpr("otbcli$", hits[i]) - 1)
    
    # normalize (no mustWork to tolerate odd mounts)
    binDir  <- normalizePath(binDir, mustWork = FALSE)
    baseDir <- normalizePath(file.path(binDir, ".."), mustWork = FALSE)
    
    # best-effort env script (standalone uses otbenv.profile)
    envScript <- file.path(baseDir, "otbenv.profile")
    if (!file.exists(envScript)) {
      envScript2 <- file.path(baseDir, "bin", "otbenv.profile")
      envScript  <- if (file.exists(envScript2)) envScript2 else NA_character_
    }
    
    data.frame(
      binDir = binDir,
      baseDir = baseDir,
      otbCmd = file.path(binDir, "otbcli"),
      envScript = envScript,
      installation_type = "OTB",
      stringsAsFactors = FALSE
    )
  })
  
  do.call("rbind", otbInstallations)
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
#'@export 
#'
#'@examples
#' \dontrun{
#' # find recursively all existing 'Orfeo Toolbox' installations folders starting 
#' # at the default search location
#' findOTB()
#' }
findOTB <- function(searchLocation = "default", quiet = TRUE) {
  
  sys <- Sys.info()[["sysname"]]
  
  if (sys == "Windows") {
    
    if (identical(searchLocation, "default") || is.null(searchLocation) || !nzchar(searchLocation)) {
      searchLocation <- "C:/"
    }
    
    # Normalize path if possible, but don't hard-fail here
    searchLocation_norm <- try(normalizePath(searchLocation, winslash = "/", mustWork = FALSE), silent = TRUE)
    if (!inherits(searchLocation_norm, "try-error") && nzchar(searchLocation_norm)) {
      searchLocation <- searchLocation_norm
    }
    
    # Windows drive letter check: "X:" at start
    if (!grepl("^[A-Z]:", toupper(searchLocation))) {
      stop("You are running Windows - Please choose a suitable searchLocation argument that MUST include a Windows drive letter and colon")
    }
    
    link <- searchOTBW(searchLocation = searchLocation, quiet = quiet)
    return(link)
    
  } else {
    
    if (identical(searchLocation, "default") || is.null(searchLocation) || !nzchar(searchLocation)) {
      searchLocation <- "/usr/bin/"
    }
    
    if (grepl(":", searchLocation, fixed = TRUE)) {
      stop("You are running Linux - please choose a suitable searchLocation argument")
    }
    
    link <- link2GI::searchOTBX(MP = searchLocation, quiet = quiet)
    return(link)
  }
}


#' Select newest OTB installation row index from a vector of binDir paths
#'
#' Heuristic order:
#' 1) ../VERSION file (preferred; parses "OTB Version: x.y.z")
#' 2) folder name pattern "OTB-x.y.z" (fallback)
#'
#' Uses numeric_version for correct semantic version comparison.
#'
#' @param paths Character vector of binDir paths (may have trailing slash).
#' @return Integer row index (1..length(paths)), defaults to 1 on failure.
#' @keywords internal
getrowotbVer <- function(paths) {
  if (!is.character(paths) || !length(paths)) return(1L)
  
  best_i <- 1L
  best_v <- numeric_version("0.0.0")
  
  for (i in seq_along(paths)) {
    p <- paths[[i]]
    if (!is.character(p) || !nzchar(p)) next
    
    # normalize but do not require existence (network / permissions)
    p_bin <- try(normalizePath(p, mustWork = FALSE), silent = TRUE)
    if (inherits(p_bin, "try-error")) p_bin <- p
    p_bin <- sub("[/\\\\]+$", "", p_bin)
    
    # 1) VERSION file one level above binDir
    ver_str <- NA_character_
    verfile <- file.path(p_bin, "..", "VERSION")
    if (file.exists(verfile)) {
      lines <- try(readLines(verfile, warn = FALSE), silent = TRUE)
      if (!inherits(lines, "try-error")) {
        hit <- grep("OTB Version", lines, value = TRUE)
        if (length(hit)) {
          ver_str <- sub(".*OTB Version\\s*:\\s*", "", hit[[1]])
          ver_str <- trimws(ver_str)
        }
      }
    }
    
    # 2) fallback: parse from folder name "OTB-x.y.z"
    if (is.na(ver_str) || !nzchar(ver_str)) {
      m <- regexec("OTB-([0-9]+\\.[0-9]+\\.[0-9]+)", p_bin, ignore.case = TRUE)
      mm <- regmatches(p_bin, m)[[1]]
      if (length(mm) >= 2) ver_str <- mm[[2]]
    }
    
    if (is.na(ver_str) || !nzchar(ver_str)) next
    
    # compare as semver
    v <- try(numeric_version(ver_str), silent = TRUE)
    if (inherits(v, "try-error")) next
    
    if (v > best_v) {
      best_v <- v
      best_i <- as.integer(i)
    }
  }
  
  if (!is.finite(best_i) || best_i < 1L || best_i > length(paths)) 1L else best_i
}






