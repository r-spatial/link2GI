# R/otb_link.R
# Public API: linkOTB()  (OS dispatcher, no side effects)

#' Locate and describe Orfeo ToolBox (OTB) API bindings
#'
#' Public wrapper that dispatches to OS-specific implementations.
#' No PATH mutation, no environment setup here.
#'
#' @param bin_OTB Optional. Path to the OTB `bin/` directory.
#' @param root_OTB Optional. Path to the OTB installation root directory.
#' @param type_OTB Optional installation type filter (if available from discovery).
#' @param searchLocation Optional search location for autodetect (e.g. mountpoint).
#' @param ver_select Selection logic: FALSE = newest, TRUE = interactive, numeric = row index.
#' @param quiet Logical. If FALSE, print selection tables and messages.
#' @param returnPaths Logical. If TRUE, return the gili descriptor.
#' @return A gili list describing the selected OTB installation.
#' @export
linkOTB <- function(bin_OTB = NULL, root_OTB = NULL, type_OTB = NULL,
                    searchLocation = NULL, ver_select = FALSE, quiet = TRUE,
                    returnPaths = TRUE) {
  
  sys <- Sys.info()[["sysname"]]
  
  if (identical(sys, "Windows")) {
    gili <- .linkOTB_windows(
      bin_OTB = bin_OTB,
      root_OTB = root_OTB,
      type_OTB = type_OTB,
      searchLocation = searchLocation,
      ver_select = ver_select,
      quiet = quiet,
      returnPaths = returnPaths
    )
  } else {
    gili <- linkOTB_linux(
      bin_OTB = bin_OTB,
      root_OTB = root_OTB,
      type_OTB = type_OTB,
      searchLocation = searchLocation,
      ver_select = ver_select,
      quiet = quiet,
      returnPaths = returnPaths
    )
  }
  
  # Hard contract (Workflow C): on non-Windows exist=TRUE requires a valid launcher
  if (!identical(sys, "Windows") && isTRUE(gili$exist)) {
    if (is.null(gili$launcher) || is.na(gili$launcher) || !nzchar(gili$launcher) || !file.exists(gili$launcher)) {
      gili$exist <- FALSE
    }
  }
  
  gili
}

# Footer
# - Neu-Workflow (C): OS-Dispatch nur hier.
# - Keine Legacy-Logik, keine side effects.
