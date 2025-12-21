#' Locate and describe an Orfeo Toolbox (OTB) installation
#'
#' Dispatcher that selects the platform-specific OTB locator.
#' This function is non-invasive: it does NOT modify PATH or ENV.
#'
#' @export
linkOTB <- function(bin_OTB = NULL,
                    root_OTB = NULL,
                    type_OTB = NULL,
                    searchLocation = NULL,
                    ver_select = FALSE,
                    quiet = TRUE,
                    returnPaths = TRUE) {
  
  sys <- Sys.info()[["sysname"]]
  
  if (identical(sys, "Windows")) {
    otb <- .linkOTB_windows(
      bin_OTB = bin_OTB,
      root_OTB = root_OTB,
      type_OTB = type_OTB,
      searchLocation = searchLocation,
      ver_select = ver_select,
      quiet = quiet,
      returnPaths = TRUE
    )
  } else {
    otb <- linkOTB_linux(
      bin_OTB = bin_OTB,
      root_OTB = root_OTB,
      type_OTB = type_OTB,
      searchLocation = searchLocation,
      ver_select = ver_select,
      quiet = quiet,
      returnPaths = TRUE
    )
  }
  
  # Workflow C contract: on non-Windows exist=TRUE requires a valid launcher
  if (!identical(sys, "Windows") && isTRUE(otb$exist)) {
    if (is.null(otb$launcher) || is.na(otb$launcher) || !nzchar(otb$launcher) || !file.exists(otb$launcher)) {
      otb$exist <- FALSE
    }
  }
  
  if (!isTRUE(returnPaths)) return(invisible(NULL))
  otb
}
