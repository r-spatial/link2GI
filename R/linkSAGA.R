#' Locate and set up SAGA GIS bindings
#'
#' @description
#' Detects available SAGA GIS installations and configures the environment for
#' calling the SAGA command line interface (`saga_cmd` / `saga_cmd.exe`).
#' Optionally prepares variables commonly used by wrappers such as \pkg{RSAGA}.
#'
#' The function relies on [findSAGA()] to discover installations (unless a
#' predefined installation table is provided via `default_SAGA`).
#'
#' @details
#' The selection strategy is:
#' \itemize{
#'   \item If exactly one installation is found, it is used.
#'   \item If multiple are found and `ver_select` is numeric, the indexed installation is used.
#'   \item If multiple are found and `ver_select = TRUE`, an interactive selection is requested.
#'   \item If multiple are found and `ver_select = FALSE`, the "latest" entry is chosen
#'         (currently: last row; improve later by a numeric version compare).
#' }
#'
#' On Windows, SAGA uses different module folder conventions depending on version
#' (e.g., `tools/` vs `modules/`). This function tries to derive the correct module
#' directory based on the detected version.
#'
#' @param default_SAGA Optional. Either `NULL` (default; triggers [findSAGA()]) or a
#'   `data.frame`-like object describing installations (at least a first column with
#'   bin directory paths; a second column may provide module paths on Unix).
#' @param searchLocation Character. Search root or hint passed to [findSAGA()].
#'   Use `"default"` to search standard locations.
#' @param ver_select Logical or numeric. If numeric, select the indexed installation.
#'   If `TRUE`, interactive selection. If `FALSE`, automatic selection.
#' @param quiet Logical. Suppress messages.
#' @param returnPaths Logical. If `TRUE`, return the selection list. If `FALSE`, only
#'   perform environment setup (side effects).
#'
#' @return
#' A list with elements:
#' \describe{
#'   \item{sagaPath}{Selected SAGA bin directory.}
#'   \item{sagaModPath}{Selected SAGA module directory (may be empty on Unix in this legacy logic).}
#'   \item{sagaCmd}{Full path to the `saga_cmd` executable.}
#'   \item{installed}{The detected installation table.}
#'   \item{exist}{Logical; `TRUE` if a valid installation was selected.}
#' }
#'
#' @keywords internal
#' @export



linkSAGA <- function(default_SAGA = NULL,
                     searchLocation = "default",
                     ver_select = FALSE,
                     quiet = TRUE,
                     returnPaths = TRUE) {
  
  # normalize ver_select
  if (identical(ver_select, "T")) ver_select <- TRUE
  if (identical(ver_select, "F")) ver_select <- FALSE
  
  sys <- Sys.info()[["sysname"]]
  scmd <- if (identical(sys, "Windows")) "saga_cmd.exe" else "saga_cmd"
  removePattern <- if (identical(sys, "Windows")) "\\\\$" else "/$"
  sep <- if (identical(sys, "Windows")) "\\" else "/"
  
  saga <- list(exist = FALSE)
  
  # find installations if none provided
  if (is.null(default_SAGA)) {
    default_SAGA <- as.data.frame(findSAGA(searchLocation = searchLocation, quiet = quiet))
  } else {
    default_SAGA <- as.data.frame(default_SAGA)
  }
  
  # robust found check
  if (!is.data.frame(default_SAGA) || nrow(default_SAGA) < 1) {
    if (returnPaths) return(saga)
    return(invisible(NULL))
  }
  if (isFALSE(default_SAGA[[1]][1])) {
    if (returnPaths) return(saga)
    return(invisible(NULL))
  }
  
  n <- nrow(default_SAGA)
  
  # select index
  sel <- 1L
  
  if (n == 1) {
    sel <- 1L
    
  } else if (is.numeric(ver_select) && ver_select >= 1 && ver_select <= n) {
    sel <- as.integer(ver_select)
    if (!quiet) {
      message("Multiple SAGA installations detected; selected index: ", sel)
    }
    
  } else if (isTRUE(ver_select)) {
    if (!quiet) {
      cat("You have installed more than one SAGA GIS version\n")
      print(default_SAGA)
      cat("\n")
    }
    v <- suppressWarnings(as.integer(readline(prompt = "Choose version (row number): ")))
    if (!is.na(v) && v >= 1 && v <= n) {
      sel <- v
    } else {
      stop("Invalid SAGA version selection.")
    }
    
  } else {
    # automatic (legacy): pick last row
    # (later: replace by numeric_version compare if you store versions in the table)
    sel <- n
    if (!quiet) {
      message("Multiple SAGA installations detected; auto-selected index: ", sel)
    }
  }
  
  sagaBin <- as.character(default_SAGA[[1]][sel])
  sagaBin <- gsub(removePattern, "", sagaBin)
  
  sagaCmd <- paste0(sagaBin, sep, scmd)
  sagaPath <- sagaBin
  
  # module path logic (Windows only; keep your existing intent, but robust)
  sagaModPath <- ""
  if (identical(sys, "Windows")) {
    
    # numeric version compare if getSagaVer exists and returns something parseable
    vchar <- try(getSagaVer(sagaPath), silent = TRUE)
    vnum <- suppressWarnings(if (!inherits(vchar, "try-error")) numeric_version(vchar) else NA)
    
    if (!is.na(vnum) && vnum >= numeric_version("3.0.0")) {
      sagaModPath <- paste0(sagaBin, sep, "tools")
    } else {
      sagaModPath <- paste0(sagaBin, sep, "modules")
    }
  } else {
    # your old code effectively empties this on non-Windows
    sagaModPath <- ""
  }
  
  sagaModPath <- gsub(removePattern, "", sagaModPath)
  
  # env vars
  Sys.setenv(SAGA_MLB = sagaModPath)
  add2Path(sagaPath)
  
  saga$sagaPath <- sagaPath
  saga$sagaModPath <- sagaModPath
  saga$sagaCmd <- sagaCmd
  saga$installed <- default_SAGA
  saga$exist <- TRUE
  
  if (returnPaths) return(saga)
  invisible(NULL)
}
