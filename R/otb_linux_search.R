# R/otb_linux_search.R
# Public API: findOTB()  (OS dispatcher, but Linux implementation included here)
# Public API: searchOTBX() (Linux recursive search)
#
# Contract (NEW workflow C):
# - searchOTBX() returns a data.frame with at least:
#   binDir, baseDir, otbCmd, envScript, installation_type
# - binDir points to the directory containing `otbcli`
# - baseDir is inferred OTB root (one level above binDir)
# - otbCmd is the full path to otbcli (Linux: .../bin/otbcli)
# - envScript is best-effort (Linux: .../otbenv.profile or .../bin/otbenv.profile)
# - installation_type currently "OTB" (reserved for OSGeo/QGIS later)

#' Locate Orfeo ToolBox (OTB) installations
#'
#' Dispatcher that calls the OS-specific search function and returns a
#' normalized installations table.
#'
#' @param searchLocation Character. On Linux: mountpoints/roots to search.
#'   If NULL, defaults to "default" which expands to c("~","/opt","/usr/local","/usr").
#' @param quiet Logical.
#' @return data.frame of installations or FALSE.
#' @export
findOTB <- function(searchLocation = NULL, quiet = TRUE) {
  
  sys <- Sys.info()[["sysname"]]
  
  if (identical(sys, "Windows")) {
    # Windows search lives in its own file in the Windows phase.
    return(searchOTBW(searchLocation = searchLocation, quiet = quiet))
  }
  
  mp <- if (is.null(searchLocation)) "default" else searchLocation
  return(searchOTBX(MP = mp, quiet = quiet))
}

#' Search recursively for valid OTB installation(s) on Linux/macOS
#'
#' Searches for an executable `otbcli` and validates presence of
#' `otbApplicationLauncherCommandLine` in the same bin folder (or base/bin).
#'
#' @param MP Character. "default" expands to c("~","/opt","/usr/local","/usr").
#'   Otherwise, one or more mountpoints/roots.
#' @param quiet Logical.
#' @return data.frame with OTB installations (or FALSE).
#' @export
#' @keywords internal
searchOTBX <- function(MP = "default", quiet = TRUE) {
  
  # 1) mountpoints
  if (identical(MP, "default")) {
    MP <- c("~", "/opt", "/usr/local", "/usr")
  } else {
    MP <- as.character(MP)
  }
  
  MP <- path.expand(MP)
  MP <- MP[file.exists(MP)]
  
  if (!length(MP)) {
    if (!quiet) message("No valid search mount points.")
    return(FALSE)
  }
  
  if (!quiet) {
    cat("\nsearching for Orfeo Toolbox installations in:\n")
    cat(paste0(" - ", MP, collapse = "\n"), "\n")
  }
  
  # 2) collect hits for otbcli
  hits <- unlist(lapply(MP, function(mp) {
    out <- suppressWarnings(try(system2(
      "find",
      args   = c(mp, "-type", "f", "-executable", "-iname", "otbcli", "-print"),
      stdout = TRUE,
      stderr = TRUE
    ), silent = TRUE))
    
    if (inherits(out, "try-error") || !length(out)) return(character(0))
    
    out <- out[!grepl("Permission denied", out, fixed = TRUE)]
    out
  }), use.names = FALSE)
  
  hits <- unique(hits)
  if (!length(hits)) {
    if (!quiet) message("::: NO OTB installation found in given locations.")
    return(FALSE)
  }
  
  # 3) normalize each candidate by validating launcher + deriving baseDir/envScript
  rows <- lapply(hits, function(otbcli_path) {
    
    otbcli_path <- normalizePath(otbcli_path, mustWork = FALSE)
    
    # binDir = directory containing otbcli
    binDir <- dirname(otbcli_path)
    binDir <- normalizePath(binDir, mustWork = FALSE)
    
    # baseDir = one level above binDir
    baseDir <- normalizePath(file.path(binDir, ".."), mustWork = FALSE)
    
    # validate launcher
    cand_launcher <- c(
      file.path(binDir, "otbApplicationLauncherCommandLine"),
      file.path(baseDir, "bin", "otbApplicationLauncherCommandLine")
    )
    cand_launcher <- cand_launcher[file.exists(cand_launcher)]
    if (!length(cand_launcher)) return(NULL)
    
    # envScript best-effort
    envScript <- file.path(baseDir, "otbenv.profile")
    if (!file.exists(envScript)) {
      env2 <- file.path(baseDir, "bin", "otbenv.profile")
      envScript <- if (file.exists(env2)) env2 else NA_character_
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
  
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    if (!quiet) message("::: Found otbcli, but no valid launcher next to it (not a usable standalone install).")
    return(FALSE)
  }
  
  df <- do.call(rbind, rows)
  
  # 4) de-duplicate by binDir
  df <- df[!duplicated(df$binDir), , drop = FALSE]
  rownames(df) <- NULL
  
  df
}

# Footer
# - NEW workflow (C): Linux search validates launcher presence (critical).
# - searchOTBX() contract aligned with linkOTB_linux(): binDir/baseDir/otbCmd/envScript/installation_type.
# - Old workflow pieces removed: no side effects, no PATH changes, no mixed Windows logic.
