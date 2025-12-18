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
#'
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
  
  # ---- helper (local) ----
  otb_root_from_bin <- function(binDir) {
    binDir <- normalizePath(binDir, mustWork = FALSE)
    normalizePath(file.path(binDir, ".."), mustWork = FALSE)
  }
  
  # ---- defaults ----
  if (is.null(searchLocation)) {
    if (Sys.info()[["sysname"]] == "Windows") {
      searchLocation <- "C:/"
    } else {
      searchLocation <- "/usr/bin/"
    }
  }
  
  params_OTB <- findOTB(searchLocation = searchLocation, quiet = quiet)
  
  if (params_OTB[[1]][1] != FALSE) {
    
    if (Sys.info()[["sysname"]] != "Windows") {
      
      if (nrow(params_OTB) == 1) {
        pathOTB <- params_OTB$binDir[[1]]
        otbCmd  <- params_OTB$otbCmd[[1]]
        
      } else if (nrow(params_OTB) > 1 && is.numeric(ver_select) && ver_select > 0) {
        
        if (!quiet) {
          cat("You have more than one valid OTB version\n")
          print(params_OTB, right = FALSE, row.names = TRUE)
        }
        
        if (is.null(type_OTB)) {
          pathOTB <- params_OTB$binDir[[ver_select]]
          otbCmd  <- params_OTB$otbCmd[[ver_select]]
        }
        
      } else if (nrow(params_OTB) > 1 && (!ver_select)) {
        
        if (!quiet) {
          cat("You have more than one valid OTB version\n")
          print(params_OTB, right = FALSE, row.names = TRUE)
        }
        
        ver <- getrowotbVer(params_OTB$binDir)
        pathOTB <- params_OTB$binDir[[ver]]
        otbCmd  <- params_OTB$otbCmd[[ver]]
        
        if (!quiet) cat("\nSelect: ", ver)
        
      } else if (nrow(params_OTB) > 1 && ver_select) {
        
        cat("You have more than one valid OTB version\n")
        print(params_OTB, right = FALSE, row.names = TRUE)
        
        if (is.null(type_OTB)) {
          ver <- as.numeric(readline(prompt = "Please choose one:  "))
          pathOTB <- params_OTB$binDir[[ver]]
          otbCmd  <- params_OTB$otbCmd[[ver]]
        }
      }
      
    } else {
      # Windows branch: DO NOT call setenvOTB() here (no env mutation)
      if (nrow(params_OTB) == 1) {
        pathOTB <- if ("binDir" %in% names(params_OTB)) params_OTB$binDir[[1]] else params_OTB[[1]][[1]]
        otbCmd  <- if ("otbCmd" %in% names(params_OTB)) params_OTB$otbCmd[[1]] else NA_character_
        
      } else if (nrow(params_OTB) > 1 && ver_select) {
        
        if (!quiet) {
          cat("You have more than one valid OTB version\n")
          print(params_OTB, right = FALSE, row.names = TRUE)
        }
        
        if (is.null(type_OTB)) {
          ver <- as.numeric(readline(prompt = "Please choose one:  "))
          pathOTB <- params_OTB$binDir[[ver]]
          otbCmd  <- params_OTB$otbCmd[[ver]]
        }
        
      } else if (nrow(params_OTB) > 1 && is.numeric(ver_select) && ver_select > 0) {
        
        if (!quiet) {
          cat("You have more than one valid OTB version\n")
          print(params_OTB, right = FALSE, row.names = TRUE)
        }
        
        if (is.null(type_OTB)) {
          pathOTB <- params_OTB$binDir[[ver_select]]
          otbCmd  <- params_OTB$otbCmd[[ver_select]]
        }
        
      } else if (nrow(params_OTB) > 1 && (!ver_select)) {
        
        if (!quiet) {
          cat("You have more than one valid OTB version\n")
          print(params_OTB, right = FALSE, row.names = TRUE)
        }
        
        ver <- getrowotbVer(params_OTB$binDir)
        pathOTB <- params_OTB$binDir[[ver]]
        otbCmd  <- params_OTB$otbCmd[[ver]]
        
        if (!quiet) cat("\nSelect: ", ver)
      }
    }
    
    # ---- build return object ----
    otb <- list()
    otb$pathOTB <- pathOTB
    otb$otbCmd  <- otbCmd
    otb$version <- params_OTB
    otb$exist   <- TRUE
    
    # ---- derive OTB root + env script WITHOUT mutating env ----
    sys <- Sys.info()[["sysname"]]
    
    if (sys == "Windows") {
      
      otb$otbRoot <- if (!is.null(root_OTB)) {
        normalizePath(root_OTB, winslash = "\\", mustWork = FALSE)
      } else {
        normalizePath(file.path(pathOTB, ".."), winslash = "\\", mustWork = FALSE)
      }
      
      cand <- c(
        file.path(otb$otbRoot, "otbenv.bat"),
        file.path(otb$otbRoot, "bin", "otbenv.bat"),
        file.path(otb$otbRoot, "bin", "o4w_env.bat"),
        file.path(otb$otbRoot, "OSGeo4W.bat")
      )
      cand <- cand[file.exists(cand)]
      otb$envScript <- if (length(cand)) cand[[1]] else NA_character_
      
    } else {
      
      otb$otbRoot <- otb_root_from_bin(pathOTB)
      
      cand <- c(
        file.path(otb$otbRoot, "otbenv.profile"),
        file.path(otb$otbRoot, "bin", "otbenv.profile")
      )
      cand <- cand[file.exists(cand)]
      otb$envScript <- if (length(cand)) cand[[1]] else NA_character_
    }
    
  } else {
    otb <- list()
    otb$exist <- FALSE
    returnPaths <- TRUE
  }
  
  if (isTRUE(returnPaths)) return(otb)
  invisible(NULL)
}
