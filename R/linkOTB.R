#' Locate and describe 'Orfeo ToolBox' (OTB) API bindings
#'
#' Locate a valid \href{https://www.orfeo-toolbox.org/}{Orfeo ToolBox} installation
#' and return the relevant paths and metadata.
#'
#' @details
#' The function searches for an OTB command line installation and selects one
#' version if multiple installations are found. Historically, \code{linkOTB()}
#' also prepared the current R session by adding OTB paths to the environment.
#'
#' In the current implementation, \code{linkOTB()} is intended to be
#' \emph{non-invasive} on all platforms: it returns paths and metadata, including
#' the inferred OTB root directory and a best-effort path to the OTB environment
#' initialisation script (\code{otbenv.profile} on Linux, \code{otbenv.bat} on
#' Windows), but does not modify \code{PATH} or other environment variables.
#'
#' It looks for the \code{otbcli.bat} file on Windows. If the file is found in a
#' \code{bin} folder it is assumed to be a valid OTB binary installation.
#'
#' @param bin_OTB String. Path to where the OTB binaries are located.
#' @param root_OTB String. Full path to the OTB root folder containing the
#'   installation. If provided, it is also used to resolve the environment
#'   initialisation script on Windows.
#' @param ver_select Logical or numeric. Default is \code{FALSE}. If there is
#'   more than one OTB installation and \code{ver_select = TRUE}, the user can
#'   interactively select the preferred OTB version. If \code{ver_select = FALSE},
#'   the latest version is automatically selected. If numeric \code{> 0}, the
#'   corresponding row index of the detected installations is selected.
#' @param searchLocation String. Hard drive letter (Windows) or mounting point
#'   (Linux). Default for Windows is \code{C:/}, default for Linux is
#'   \code{/usr/bin/}.
#' @param type_OTB String. Optional installation type filter (e.g. OSGeo4W/QGIS).
#' @param quiet Logical. Switch for suppressing messages. Default is \code{TRUE}.
#' @param returnPaths Logical. If set to \code{FALSE}, nothing is returned.
#'   If \code{TRUE}, the selected OTB paths and detected versions are returned.
#'
#' @note
#' You may also set the path manually. Using an 'OSGeo4W64'
#' \url{https://trac.osgeo.org/osgeo4w/} installation it is typically
#' \code{C:/OSGeo4W64/bin/}.
#'
#' @return
#' A list with at least:
#' \itemize{
#'   \item \code{exist}: logical, whether a valid installation was found
#'   \item \code{pathOTB}: path to the selected OTB \code{bin} directory
#'   \item \code{otbCmd}: best-effort base command as detected
#'   \item \code{version}: data.frame of detected installations
#'   \item \code{otbRoot}: inferred OTB root directory (one level above \code{bin})
#'   \item \code{envScript}: best-effort path to \code{otbenv.profile} /
#'     \code{otbenv.bat} if found, otherwise \code{NA_character_}
#' }
#'
#' @author Chris Reudenbach
#' @export
#' @examples
#' \dontrun{
#' # call if you do not have any idea if and where OTB is installed
#' otb <- linkOTB()
#' if (otb$exist) {
#'   print(otb)
#' }
#' }
linkOTB <- function(bin_OTB = NULL, root_OTB = NULL, type_OTB = NULL,
                    searchLocation = NULL, ver_select = FALSE, quiet = TRUE,
                    returnPaths = TRUE) {
  
  sys <- Sys.info()[["sysname"]]
  
  # ---- defaults ----
  if (is.null(searchLocation)) {
    searchLocation <- if (sys == "Windows") "C:/" else "/usr/bin/"
  }
  
  # ---- allow manual override (preferred if user knows exact paths) ----
  if (!is.null(bin_OTB) && nzchar(bin_OTB)) {
    
    if (sys == "Windows") {
      pathOTB <- normalizePath(bin_OTB, winslash = "\\", mustWork = FALSE)
      otbRoot <- if (!is.null(root_OTB) && nzchar(root_OTB)) {
        normalizePath(root_OTB, winslash = "\\", mustWork = FALSE)
      } else {
        normalizePath(file.path(pathOTB, ".."), winslash = "\\", mustWork = FALSE)
      }
      
      # pick a usable command
      cand_cmd <- c(
        file.path(pathOTB, "otbcli.bat"),
        file.path(pathOTB, "otbcli.exe"),
        file.path(pathOTB, "otbcli")
      )
      cand_cmd <- cand_cmd[file.exists(cand_cmd)]
      otbCmd <- if (length(cand_cmd)) cand_cmd[[1]] else NA_character_
      
      # standalone OTB >=9 uses otbenv.ps1
      cand_env <- c(
        file.path(otbRoot, "otbenv.ps1"),
        file.path(otbRoot, "otbenv.bat"),
        file.path(otbRoot, "bin", "otbenv.ps1"),
        file.path(otbRoot, "bin", "otbenv.bat"),
        file.path(otbRoot, "bin", "o4w_env.bat"),
        file.path(otbRoot, "OSGeo4W.bat")
      )
      cand_env <- cand_env[file.exists(cand_env)]
      envScript <- if (length(cand_env)) cand_env[[1]] else NA_character_
      
      otb <- list(
        exist = isTRUE(file.exists(pathOTB)) && !is.na(otbCmd) && file.exists(otbCmd),
        pathOTB = pathOTB,
        otbCmd = otbCmd,
        version = data.frame(
          binDir = pathOTB,
          baseDir = otbRoot,
          otbCmd = otbCmd,
          envScript = envScript,
          installation_type = "OTB",
          stringsAsFactors = FALSE
        ),
        otbRoot = otbRoot,
        envScript = envScript
      )
      
      if (isTRUE(returnPaths)) return(otb)
      return(invisible(NULL))
      
    } else {
      # non-Windows: keep behavior minimal
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
      
      otb <- list(
        exist = isTRUE(file.exists(pathOTB)) && !is.na(otbCmd) && file.exists(otbCmd),
        pathOTB = pathOTB,
        otbCmd = otbCmd,
        version = data.frame(
          binDir = pathOTB,
          baseDir = otbRoot,
          otbCmd = otbCmd,
          envScript = envScript,
          installation_type = "OTB",
          stringsAsFactors = FALSE
        ),
        otbRoot = otbRoot,
        envScript = envScript
      )
      
      if (isTRUE(returnPaths)) return(otb)
      return(invisible(NULL))
    }
  }
  
  # ---- autodetect ----
  params_OTB <- findOTB(searchLocation = searchLocation, quiet = quiet)
  
  if (identical(params_OTB, FALSE) || is.null(params_OTB) ||
      (is.list(params_OTB) && length(params_OTB) == 1 && identical(params_OTB[[1]], FALSE))) {
    otb <- list(exist = FALSE)
    if (isTRUE(returnPaths)) return(otb)
    return(invisible(NULL))
  }
  
  if (!is.data.frame(params_OTB) || nrow(params_OTB) == 0) {
    otb <- list(exist = FALSE)
    if (isTRUE(returnPaths)) return(otb)
    return(invisible(NULL))
  }
  
  # optional installation-type filter
  if (!is.null(type_OTB) && nzchar(type_OTB) && ("installation_type" %in% names(params_OTB))) {
    params2 <- params_OTB[params_OTB$installation_type %in% type_OTB, , drop = FALSE]
    if (nrow(params2) > 0) params_OTB <- params2
  }
  
  # select installation
  if (nrow(params_OTB) == 1) {
    idx <- 1
  } else if (is.numeric(ver_select) && ver_select > 0) {
    idx <- ver_select
  } else if (isTRUE(ver_select)) {
    if (!quiet) {
      cat("You have more than one valid OTB version\n")
      print(params_OTB, right = FALSE, row.names = TRUE)
    }
    idx <- as.numeric(readline(prompt = "Please choose one:  "))
  } else {
    idx <- getrowotbVer(params_OTB$binDir)
  }
  
  if (is.na(idx) || idx < 1 || idx > nrow(params_OTB)) idx <- 1
  
  pathOTB <- params_OTB$binDir[[idx]]
  pathOTB <- if (sys == "Windows") {
    normalizePath(pathOTB, winslash = "\\", mustWork = FALSE)
  } else {
    normalizePath(pathOTB, mustWork = FALSE)
  }
  
  # derive root
  if (sys == "Windows") {
    otbRoot <- if ("baseDir" %in% names(params_OTB) && nzchar(params_OTB$baseDir[[idx]])) {
      normalizePath(params_OTB$baseDir[[idx]], winslash = "\\", mustWork = FALSE)
    } else {
      normalizePath(file.path(pathOTB, ".."), winslash = "\\", mustWork = FALSE)
    }
    
    # command
    cand_cmd <- c(
      if ("otbCmd" %in% names(params_OTB)) params_OTB$otbCmd[[idx]] else NA_character_,
      file.path(pathOTB, "otbcli.bat"),
      file.path(pathOTB, "otbcli.exe"),
      file.path(pathOTB, "otbcli")
    )
    cand_cmd <- unique(cand_cmd[!is.na(cand_cmd) & nzchar(cand_cmd)])
    cand_cmd <- cand_cmd[file.exists(cand_cmd)]
    otbCmd <- if (length(cand_cmd)) cand_cmd[[1]] else NA_character_
    
    # env script: prefer what searchOTBW already detected (params_OTB$envScript)
    envScript <- NA_character_
    if ("envScript" %in% names(params_OTB)) {
      es <- params_OTB$envScript[[idx]]
      if (!is.na(es) && nzchar(es) && file.exists(es)) {
        envScript <- normalizePath(es, winslash = "\\", mustWork = FALSE)
      }
    }
    
    # fallback only if not provided by searchOTBW
    if (is.na(envScript)) {
      cand_env <- c(
        file.path(otbRoot, "otbenv.ps1"),
        file.path(otbRoot, "otbenv.bat"),
        file.path(otbRoot, "bin", "otbenv.ps1"),
        file.path(otbRoot, "bin", "otbenv.bat"),
        file.path(otbRoot, "bin", "o4w_env.bat"),
        file.path(otbRoot, "OSGeo4W.bat")
      )
      cand_env <- cand_env[file.exists(cand_env)]
      envScript <- if (length(cand_env)) cand_env[[1]] else NA_character_
    }
    
  } else {
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
    
    # env script: prefer what searchOTBX returned if available, else fallback
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
  }
  
  otb <- list(
    pathOTB = pathOTB,
    otbCmd = otbCmd,
    version = params_OTB,
    exist = isTRUE(file.exists(pathOTB)) && !is.na(otbCmd) && file.exists(otbCmd),
    otbRoot = otbRoot,
    envScript = envScript
  )
  
  if (isTRUE(returnPaths)) return(otb)
  invisible(NULL)
}
