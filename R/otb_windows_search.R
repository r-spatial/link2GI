# R/otb_windows_search.R

#' Search for OTB installations on Windows (bounded, cmd-free)
#'
#' Detects Orfeo Toolbox (OTB) installations on Windows using a bounded set of
#' plausible roots (no full-disk crawl). Modern standalone bundles (OTB 9.x)
#' are detected by the presence of:
#' \itemize{
#'   \item an environment script: \code{otbenv.ps1} (preferred) or \code{otbenv.bat}
#'   \item a launcher: \code{bin/otbApplicationLauncherCommandLine.exe}
#'   \item at least one CLI wrapper in \code{bin/}:
#'     \code{otbcli_*.ps1}, \code{otbcli_*.bat}, or \code{otbcli_*.exe}
#' }
#'
#' Backward compatibility: older callers may pass \code{DL} instead of
#' \code{searchLocation}. Internally, \code{DL} is treated as an alias for
#' \code{searchLocation}.
#'
#' @param searchLocation Character scalar. Root directory to search
#'   (default \code{"C:/"}).
#' @param DL Character scalar. Deprecated alias for \code{searchLocation}.
#' @param maxdepth Integer. Best-effort maximum recursion depth for the
#'   recursive \code{list.files()} search (default \code{8}).
#' @param quiet Logical. If \code{TRUE}, suppress messages.
#'
#' @return A \code{data.frame} with one row per detected installation and columns:
#' \describe{
#'   \item{binDir}{Normalized path to \code{<root>/bin}.}
#'   \item{baseDir}{Normalized OTB root directory.}
#'   \item{otbCmd}{Path to a detected CLI wrapper (ps1/bat/exe).}
#'   \item{envScript}{Path to \code{otbenv.ps1} or \code{otbenv.bat}.}
#'   \item{launcher}{Path to \code{otbApplicationLauncherCommandLine.exe}.}
#'   \item{installation_type}{Classification string (e.g., \code{"OTB_STANDALONE_PS1"}).}
#' }
#'
#' @examples
#' \dontrun{
#' # bounded search under C:/
#' searchOTBW("C:/", quiet = FALSE)
#'
#' # legacy alias
#' searchOTBW(DL = "C:/", quiet = FALSE)
#' }
#'
#' @export
searchOTBW <- function(searchLocation = "C:/", DL = NULL, maxdepth = 8L, quiet = FALSE) {
  
  # --- legacy alias handling -------------------------------------------------
  if (!is.null(DL) && (is.null(searchLocation) || !nzchar(searchLocation))) {
    searchLocation <- DL
  }
  if (!is.null(DL) && !is.null(searchLocation) && nzchar(searchLocation) &&
      !identical(DL, searchLocation) && !isTRUE(quiet)) {
    message("::: 'DL' is deprecated; use 'searchLocation'.")
  }
  
  stopifnot(is.character(searchLocation), length(searchLocation) == 1L, nzchar(searchLocation))
  
  maxdepth <- as.integer(maxdepth)
  if (is.na(maxdepth) || maxdepth < 1L) maxdepth <- 8L
  
  # --- helpers ---------------------------------------------------------------
  normp <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  
  # best-effort "depth" filter
  depth_ok <- function(root, path, maxdepth) {
    root <- gsub("/+$", "", normp(root))
    path <- normp(path)
    
    # escape regex metacharacters in root
    root_rx <- gsub("([\\^\\$\\.|\\+\\(\\)\\[\\]\\{\\}\\\\])", "\\\\\\1", root)
    rel <- sub(paste0("^", root_rx, "/?"), "", path)
    
    if (!nzchar(rel)) return(TRUE)
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    d <- max(0L, length(parts) - 1L)
    d <= maxdepth
  }
  
  empty_df <- function() {
    data.frame(
      binDir = character(0),
      baseDir = character(0),
      otbCmd = character(0),
      envScript = character(0),
      launcher = character(0),
      installation_type = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  # minimal “is valid OTB root”
  validate_otb_root <- function(root) {
    root <- normp(root)
    
    env_ps1  <- file.path(root, "otbenv.ps1")
    env_bat  <- file.path(root, "otbenv.bat")
    bin_dir  <- file.path(root, "bin")
    launcher <- file.path(bin_dir, "otbApplicationLauncherCommandLine.exe")
    
    has_env    <- file.exists(env_ps1) || file.exists(env_bat)
    has_bin    <- dir.exists(bin_dir)
    has_launch <- file.exists(launcher)
    
    # wrappers
    cli_ps1 <- if (has_bin) list.files(bin_dir, pattern = "^otbcli(_.*)?\\.ps1$", full.names = TRUE, ignore.case = TRUE) else character(0)
    cli_bat <- if (has_bin) list.files(bin_dir, pattern = "^otbcli(_.*)?\\.bat$", full.names = TRUE, ignore.case = TRUE) else character(0)
    cli_exe <- if (has_bin) list.files(bin_dir, pattern = "^otbcli(_.*)?\\.exe$", full.names = TRUE, ignore.case = TRUE) else character(0)
    
    has_cli <- length(cli_ps1) > 0L || length(cli_bat) > 0L || length(cli_exe) > 0L
    
    envScript <- if (file.exists(env_ps1)) normp(env_ps1) else if (file.exists(env_bat)) normp(env_bat) else NA_character_
    launcherP <- if (file.exists(launcher)) normp(launcher) else NA_character_
    
    otbCmd <- {
      if (length(cli_bat)) normp(cli_bat[1])
      else if (length(cli_ps1)) normp(cli_ps1[1])
      else if (length(cli_exe)) normp(cli_exe[1])
      else NA_character_
    }
    
    installation_type <- {
      if (length(cli_ps1) && file.exists(env_ps1) && file.exists(launcher)) "OTB_STANDALONE_PS1"
      else if (length(cli_bat) && file.exists(env_bat) && file.exists(launcher)) "OTB_BAT"
      else if (length(cli_exe) && file.exists(launcher)) "OTB_EXE"
      else "OTB"
    }
    
    list(
      root = root,
      binDir = normp(bin_dir),
      has_env = has_env,
      has_bin = has_bin,
      envScript = envScript,
      has_launch = has_launch,
      launcher = launcherP,
      has_cli = has_cli,
      otbCmd = otbCmd,
      installation_type = installation_type
    )
  }
  
  # --- search ----------------------------------------------------------------
  root0 <- normp(searchLocation)
  if (!dir.exists(root0)) stop("searchLocation does not exist: ", root0)
  
  if (!isTRUE(quiet)) {
    message("::: Searching OTB on Windows under: '", gsub("/", "\\\\", root0), "'")
  }
  
  env_hits <- list.files(
    root0,
    pattern = "^otbenv\\.(ps1|bat)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  env_hits <- env_hits[vapply(env_hits, function(p) depth_ok(root0, p, maxdepth), logical(1))]
  
  roots <- character(0)
  
  if (length(env_hits)) {
    # env located directly under root
    roots <- unique(dirname(env_hits))
  } else {
    launch_hits <- list.files(
      root0,
      pattern = "^otbApplicationLauncherCommandLine\\.exe$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    launch_hits <- launch_hits[vapply(launch_hits, function(p) depth_ok(root0, p, maxdepth), logical(1))]
    if (length(launch_hits)) {
      roots <- unique(normp(file.path(dirname(launch_hits), "..")))
    } else {
      cli_hits <- list.files(
        root0,
        pattern = "^otbcli(_.*)?\\.(ps1|bat|exe)$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
      cli_hits <- cli_hits[vapply(cli_hits, function(p) depth_ok(root0, p, maxdepth), logical(1))]
      if (length(cli_hits)) {
        roots <- unique(normp(file.path(dirname(cli_hits), "..")))
      }
    }
  }
  
  roots <- unique(roots)
  roots <- roots[dir.exists(roots)]
  
  if (!length(roots)) {
    if (!isTRUE(quiet)) {
      message("::: NO OTB installation found at: '", gsub("/", "\\\\", root0), "'")
    }
    return(empty_df())
  }
  
  infos <- lapply(roots, validate_otb_root)
  infos <- infos[vapply(infos, function(x) {
    isTRUE(x$has_env) && isTRUE(x$has_bin) && isTRUE(x$has_launch) && isTRUE(x$has_cli)
  }, logical(1))]
  
  if (!length(infos)) {
    if (!isTRUE(quiet)) {
      message("::: Candidate roots found but none passed validation (env+bin+launcher+cli).")
    }
    return(empty_df())
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
  
  if (!isTRUE(quiet)) {
    message("::: Found ", nrow(df), " OTB installation(s).")
  }
  
  df
}

# ------------------------------------------------------------------
# FOOTER — Migration / Workflow Information
# ------------------------------------------------------------------
# File:     R/otb_windows_search.R
# Workflow: C (new consolidated OTB workflow)
#
# Role:
#   - Windows bounded search for standalone OTB bundles
#   - Returns a normalized installations table with launcher + envScript + otbCmd
#
# Notes:
#   - No PATH/ENV mutation
#   - Uses list.files(recursive=TRUE) bounded by maxdepth (best effort)
#   - Keeps legacy alias DL (deprecated) for backward compatibility
# ------------------------------------------------------------------
