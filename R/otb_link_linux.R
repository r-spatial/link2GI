# Linux-specific OTB locator
.linkOTB_linux <- function(bin_OTB,
                           root_OTB,
                           type_OTB,
                           searchLocation,
                           ver_select,
                           quiet) {
  
  if (is.null(searchLocation)) {
    searchLocation <- c("~", "/opt", "/usr/local", "/usr")
  }
  
  # Manual override
  if (!is.null(bin_OTB) && nzchar(bin_OTB)) {
    binDir  <- normalizePath(bin_OTB, mustWork = FALSE)
    baseDir <- if (!is.null(root_OTB)) normalizePath(root_OTB, mustWork = FALSE)
    else normalizePath(file.path(binDir, ".."), mustWork = FALSE)
    
    otbCmd <- file.path(binDir, "otbcli")
    envScript <- file.path(baseDir, "otbenv.profile")
    if (!file.exists(envScript)) envScript <- NA_character_
    
    return(list(
      exist = file.exists(otbCmd),
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
  
  # Autodetect
  hits <- searchOTBX(searchLocation, quiet = quiet)
  if (identical(hits, FALSE) || !nrow(hits)) {
    return(list(exist = FALSE))
  }
  
  idx <- if (nrow(hits) == 1) 1 else getrowotbVer(hits$binDir)
  
  binDir  <- normalizePath(hits$binDir[idx], mustWork = FALSE)
  baseDir <- normalizePath(hits$baseDir[idx], mustWork = FALSE)
  otbCmd  <- hits$otbCmd[idx]
  envScript <- hits$envScript[idx]
  
  list(
    exist = file.exists(otbCmd),
    pathOTB = binDir,
    otbCmd = otbCmd,
    otbRoot = baseDir,
    envScript = envScript,
    version = hits
  )
}

# ------------------------------------------------------------------
# FOOTER — Migration / Workflow Information
# ------------------------------------------------------------------
# File:     R/otb_link_linux.R
# Workflow: C (new consolidated OTB workflow)
#
# Role:
#   - Linux-only OTB discovery
#   - No environment modification
#
# Legacy:
#   - Replaces Linux logic previously embedded in linkOTB()
#
# ------------------------------------------------------------------
