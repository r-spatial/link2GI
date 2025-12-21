# R/otb_linux_link.R
# Internal implementation: linkOTB_linux()
# Drop-in version (Workflow C): includes new-core aliases + defensive selection

# --- helpers (internal) -------------------------------------------------------

#' @keywords internal
.otb_root_from_gili <- function(gili) {
  stopifnot(isTRUE(gili$exist))
  
  # prefer canonical/alias root fields
  if (!is.null(gili$otbRoot) && nzchar(gili$otbRoot)) {
    return(normalizePath(gili$otbRoot, mustWork = TRUE))
  }
  if (!is.null(gili$rootDir) && nzchar(gili$rootDir)) {
    return(normalizePath(gili$rootDir, mustWork = TRUE))
  }
  
  # prefer canonical/alias bin fields
  bin <- NULL
  if (!is.null(gili$pathOTB) && nzchar(gili$pathOTB)) bin <- gili$pathOTB
  if (is.null(bin) && !is.null(gili$binDir) && nzchar(gili$binDir)) bin <- gili$binDir
  
  p <- normalizePath(bin, mustWork = TRUE)            # .../bin/
  normalizePath(file.path(p, ".."), mustWork = TRUE)  # .../
}

#' @keywords internal
.otb_env_linux <- function(otb_root) {
  otb_root <- normalizePath(otb_root, mustWork = TRUE)
  
  app_paths <- c(
    file.path(otb_root, "lib",   "otb", "applications"),
    file.path(otb_root, "lib64", "otb", "applications")
  )
  app_paths <- app_paths[dir.exists(app_paths)]
  if (!length(app_paths)) stop("No OTB applications dir found under: ", otb_root)
  
  lib_paths <- c(file.path(otb_root, "lib"), file.path(otb_root, "lib64"))
  lib_paths <- lib_paths[dir.exists(lib_paths)]
  if (!length(lib_paths)) stop("No OTB lib dir found under: ", otb_root)
  
  env_named <- c(
    OTB_INSTALL_DIR      = otb_root,
    CMAKE_PREFIX_PATH    = otb_root,
    OTB_APPLICATION_PATH = paste(app_paths, collapse = ":"),
    GDAL_DATA            = file.path(otb_root, "share", "gdal"),
    PROJ_LIB             = file.path(otb_root, "share", "proj"),
    GDAL_DRIVER_PATH     = "disable",
    LC_NUMERIC           = "C",
    PATH                 = paste(c(file.path(otb_root, "bin"), Sys.getenv("PATH")), collapse = ":"),
    PYTHONPATH           = paste(c(file.path(otb_root, "lib", "otb", "python"),
                                   Sys.getenv("PYTHONPATH")), collapse = ":"),
    LD_LIBRARY_PATH      = paste(c(lib_paths, Sys.getenv("LD_LIBRARY_PATH")), collapse = ":")
  )
  
  env_named <- env_named[nzchar(env_named)]
  env_named
}

# --- public-ish internal (called by linkOTB wrapper) --------------------------

#' Locate and describe Orfeo ToolBox (OTB) bindings (Linux/macOS)
#'
#' Internal OS-specific implementation. Returns a gili list with a valid
#' `launcher` (otbApplicationLauncherCommandLine) if present.
#'
#' @param bin_OTB Optional. Path to OTB bin directory.
#' @param root_OTB Optional. Path to OTB root directory.
#' @param type_OTB Optional installation type filter.
#' @param searchLocation Default `/usr/bin/` for auto-detect, or a mountpoint.
#' @param ver_select Selection logic (FALSE = newest, TRUE = interactive, numeric = row).
#' @param quiet Logical.
#' @param returnPaths Logical.
#' @return gili list.
#' @keywords internal
linkOTB_linux <- function(bin_OTB = NULL, root_OTB = NULL, type_OTB = NULL,
                          searchLocation = NULL, ver_select = FALSE, quiet = TRUE,
                          returnPaths = TRUE) {
  
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Windows")) stop("linkOTB_linux() called on Windows.")
  
  # defaults
  if (is.null(searchLocation)) searchLocation <- "/usr/bin/"
  
  # ---- manual override ----
  if (!is.null(bin_OTB) && nzchar(bin_OTB)) {
    
    pathOTB <- normalizePath(bin_OTB, mustWork = FALSE)
    otbRoot <- if (!is.null(root_OTB) && nzchar(root_OTB)) {
      normalizePath(root_OTB, mustWork = FALSE)
    } else {
      normalizePath(file.path(pathOTB, ".."), mustWork = FALSE)
    }
    
    cand_cmd <- c(file.path(pathOTB, "otbcli"), file.path(pathOTB, "otbcli.sh"))
    cand_cmd <- cand_cmd[file.exists(cand_cmd)]
    otbCmd <- if (length(cand_cmd)) cand_cmd[[1]] else NA_character_
    
    cand_env <- c(file.path(otbRoot, "otbenv.profile"), file.path(otbRoot, "bin", "otbenv.profile"))
    cand_env <- cand_env[file.exists(cand_env)]
    envScript <- if (length(cand_env)) cand_env[[1]] else NA_character_
    
    # launcher (critical for new workflow)
    cand_launcher <- c(
      file.path(pathOTB, "otbApplicationLauncherCommandLine"),
      file.path(pathOTB, "otbApplicationLauncherCommandLine.sh"),
      file.path(otbRoot, "bin", "otbApplicationLauncherCommandLine"),
      file.path(otbRoot, "bin", "otbApplicationLauncherCommandLine.sh")
    )
    cand_launcher <- cand_launcher[file.exists(cand_launcher)]
    launcher <- if (length(cand_launcher)) cand_launcher[[1]] else NA_character_
    
    version_df <- data.frame(
      binDir = pathOTB,
      baseDir = otbRoot,
      otbCmd = otbCmd,
      envScript = envScript,
      installation_type = "OTB",
      stringsAsFactors = FALSE
    )
    
    gili <- list(
      exist     = isTRUE(dir.exists(pathOTB)) &&
        !is.na(otbCmd) && file.exists(otbCmd) &&
        !is.na(launcher) && file.exists(launcher),
      
      # canonical paths
      pathOTB   = pathOTB,
      otbRoot   = otbRoot,
      
      # aliases (new core contract)
      binDir    = pathOTB,
      rootDir   = otbRoot,
      
      otbCmd    = otbCmd,
      launcher  = launcher,
      envScript = envScript,
      
      version      = version_df,
      version_tbl  = version_df
    )
    
    if (isTRUE(returnPaths)) return(gili)
    return(invisible(NULL))
  }
  
  # ---- autodetect ----
  params_OTB <- findOTB(searchLocation = searchLocation, quiet = quiet)
  
  if (identical(params_OTB, FALSE) || is.null(params_OTB) ||
      (is.list(params_OTB) && length(params_OTB) == 1 && identical(params_OTB[[1]], FALSE))) {
    gili <- list(exist = FALSE)
    if (isTRUE(returnPaths)) return(gili)
    return(invisible(NULL))
  }
  
  if (!is.data.frame(params_OTB) || nrow(params_OTB) == 0) {
    gili <- list(exist = FALSE)
    if (isTRUE(returnPaths)) return(gili)
    return(invisible(NULL))
  }
  
  # optional installation-type filter
  if (!is.null(type_OTB) && nzchar(type_OTB) && ("installation_type" %in% names(params_OTB))) {
    params2 <- params_OTB[params_OTB$installation_type %in% type_OTB, , drop = FALSE]
    if (nrow(params2) > 0) params_OTB <- params2
  }
  
  # select installation
  if (nrow(params_OTB) == 1) {
    idx <- 1L
  } else if (is.numeric(ver_select) && ver_select > 0) {
    idx <- as.integer(ver_select)
  } else if (isTRUE(ver_select)) {
    if (!quiet) {
      cat("You have more than one valid OTB version\n")
      print(params_OTB, right = FALSE, row.names = TRUE)
    }
    idx <- suppressWarnings(as.integer(readline(prompt = "Please choose one:  ")))
  } else {
    sel_paths <- if ("binDir" %in% names(params_OTB)) params_OTB$binDir else params_OTB$pathOTB
    idx <- getrowotbVer(sel_paths)
  }
  if (is.na(idx) || idx < 1L || idx > nrow(params_OTB)) idx <- 1L
  
  pathOTB <- normalizePath(params_OTB$binDir[[idx]], mustWork = FALSE)
  
  otbRoot <- if ("baseDir" %in% names(params_OTB) && nzchar(params_OTB$baseDir[[idx]])) {
    normalizePath(params_OTB$baseDir[[idx]], mustWork = FALSE)
  } else {
    normalizePath(file.path(pathOTB, ".."), mustWork = FALSE)
  }
  
  cand_cmd <- c(
    if ("otbCmd" %in% names(params_OTB)) params_OTB$otbCmd[[idx]] else NA_character_,
    file.path(pathOTB, "otbcli")
  )
  cand_cmd <- unique(cand_cmd[!is.na(cand_cmd) & nzchar(cand_cmd)])
  cand_cmd <- cand_cmd[file.exists(cand_cmd)]
  otbCmd <- if (length(cand_cmd)) cand_cmd[[1]] else NA_character_
  
  envScript <- NA_character_
  if ("envScript" %in% names(params_OTB)) {
    es <- params_OTB$envScript[[idx]]
    if (!is.na(es) && nzchar(es) && file.exists(es)) {
      envScript <- normalizePath(es, mustWork = FALSE)
    }
  }
  if (is.na(envScript)) {
    cand_env <- c(
      file.path(otbRoot, "otbenv.profile"),
      file.path(otbRoot, "bin", "otbenv.profile")
    )
    cand_env <- cand_env[file.exists(cand_env)]
    envScript <- if (length(cand_env)) cand_env[[1]] else NA_character_
  }
  
  # launcher: required for capabilities/help on Linux
  cand_launcher <- c(
    file.path(pathOTB, "otbApplicationLauncherCommandLine"),
    file.path(pathOTB, "otbApplicationLauncherCommandLine.sh"),
    file.path(otbRoot, "bin", "otbApplicationLauncherCommandLine"),
    file.path(otbRoot, "bin", "otbApplicationLauncherCommandLine.sh")
  )
  cand_launcher <- cand_launcher[file.exists(cand_launcher)]
  launcher <- if (length(cand_launcher)) cand_launcher[[1]] else NA_character_
  
  gili <- list(
    # canonical paths
    pathOTB   = pathOTB,
    otbRoot   = otbRoot,
    
    # aliases (new core contract)
    binDir    = pathOTB,
    rootDir   = otbRoot,
    
    otbCmd    = otbCmd,
    launcher  = launcher,
    envScript = envScript,
    
    version      = params_OTB,
    version_tbl  = params_OTB,
    
    exist     = isTRUE(dir.exists(pathOTB)) &&
      !is.na(otbCmd) && file.exists(otbCmd) &&
      !is.na(launcher) && file.exists(launcher)
  )
  
  if (isTRUE(returnPaths)) return(gili)
  invisible(NULL)
}

# ------------------------------------------------------------------
# FOOTER — Migration / Workflow Information
# ------------------------------------------------------------------
# File:     R/otb_linux_link.R
# Workflow: C (new consolidated OTB workflow)
#
# Role:
#   - Linux/macOS OTB discovery + selection result -> gili descriptor
#   - exist=TRUE requires otbCmd + launcher (launcher required for help/capabilities)
#   - Adds new-core aliases: binDir/rootDir/version_tbl
#   - No PATH/ENV mutation (ENV is built explicitly via .otb_env_linux() when running)
#
# ------------------------------------------------------------------
