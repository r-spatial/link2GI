# Windows-specific OTB locator
#' @keywords internal
.linkOTB_windows <- function(bin_OTB,
                             root_OTB,
                             type_OTB,
                             searchLocation,
                             ver_select,
                             quiet,
                             returnPaths = TRUE) {
  
  if (!isTRUE(returnPaths)) return(invisible(NULL))
  
  if (is.null(searchLocation)) searchLocation <- "C:/"
  
  # Manual override
  if (!is.null(bin_OTB) && nzchar(bin_OTB)) {
    binDir  <- normalizePath(bin_OTB, winslash = "\\", mustWork = FALSE)
    baseDir <- if (!is.null(root_OTB)) normalizePath(root_OTB, winslash = "\\", mustWork = FALSE)
    else normalizePath(file.path(binDir, ".."), winslash = "\\", mustWork = FALSE)
    
    wrappers <- c(
      file.path(binDir, "otbcli.bat"),
      file.path(binDir, "otbcli.exe"),
      file.path(binDir, "otbcli")
    )
    wrappers <- wrappers[file.exists(wrappers)]
    otbCmd <- if (length(wrappers)) wrappers[1] else NA_character_
    
    envCandidates <- c(
      file.path(baseDir, "otbenv.ps1"),
      file.path(baseDir, "otbenv.bat"),
      file.path(baseDir, "bin", "otbenv.ps1"),
      file.path(baseDir, "bin", "otbenv.bat")
    )
    envScript <- envCandidates[file.exists(envCandidates)][1]
    if (!length(envScript)) envScript <- NA_character_
    
    return(list(
      exist = !is.na(otbCmd),
      pathOTB = binDir,
      otbCmd = otbCmd,
      otbRoot = baseDir,
      envScript = envScript,
      version = data.frame(
        binDir = binDir,
        baseDir = baseDir,
        otbCmd = otbCmd,
        envScript = envScript,
        installation_type = "OTB",
        stringsAsFactors = FALSE
      )
    ))
  }
  
  hits <- searchOTBW(searchLocation, quiet = quiet)
  if (!nrow(hits)) return(list(exist = FALSE))
  
  idx <- if (nrow(hits) == 1) 1 else getrowotbVer(hits$binDir)
  
  list(
    exist = TRUE,
    pathOTB = hits$binDir[idx],
    otbCmd = hits$otbCmd[idx],
    otbRoot = hits$baseDir[idx],
    envScript = hits$envScript[idx],
    version = hits
  )
}


# ------------------------------------------------------------------
# FOOTER — Migration / Workflow Information
# ------------------------------------------------------------------
# File:     R/otb_link_windows.R
# Workflow: C (new consolidated OTB workflow)
#
# Role:
#   - Windows-only OTB discovery
#   - Supports OTB >= 9.x standalone bundles
#
# Legacy:
#   - Fully replaces Windows logic in old linkOTB()
#
# ------------------------------------------------------------------
