#' Run an OTB application (new workflow C)
#'
#' Executes an Orfeo ToolBox application via the launcher/wrapper described by
#' `gili` (typically returned by [linkOTB()]). This wrapper is non-invasive: it
#' does not permanently modify PATH or the user environment.
#'
#' The command is provided as a list in "link2GI style":
#' - `otbCmdList[[1]]` is the application name (e.g., `"DimensionalityReduction"`)
#' - named elements are OTB parameter keys (without leading `-`)
#'
#' Parameter values can be:
#' - a scalar character/numeric (converted to character)
#' - `NA` / `NA_character_` to omit the parameter
#' - for pixel-typed outputs: a character vector of length 2
#'   `c("<path>", "<pixel_type>")` (e.g. `c("out.tif","float")`)
#'
#' @param otbCmdList List. OTB command list. The first element must be the
#'   algorithm name; remaining named elements are parameter keys/values.
#' @param gili List. OTB installation descriptor as returned by [linkOTB()].
#'   If `NULL`, [linkOTB()] is called.
#' @param retRaster Logical. If `TRUE`, return a `terra::SpatRaster` for the
#'   primary raster output (when detectable). If `FALSE`, return the output
#'   path(s) (character) or a status code depending on implementation.
#' @param retCommand Logical. If `TRUE`, do not execute; return the exact CLI
#'   command string that would be run.
#' @param quiet Logical. If `TRUE`, suppress console output from OTB (best-effort).
#' @return Depending on `retCommand` / `retRaster`, returns either a command
#'   string, a `terra::SpatRaster`, or a character vector/status describing the
#'   produced output.
#' @export
 runOTB <- function(otbCmdList,
                   gili = NULL,
                   retRaster = TRUE,
                   retCommand = FALSE,
                   quiet = TRUE) {
  
  stopifnot(is.list(otbCmdList), length(otbCmdList) > 0)
  

  
  if (is.null(gili)) gili <- linkOTB()
  if (!isTRUE(gili$exist)) stop("No valid OTB installation found.")
  
  algo <- otbCmdList[[1]]
  otbCmdList[[1]] <- NULL
  
  args <- unlist(
    lapply(names(otbCmdList), function(k) {
      v <- otbCmdList[[k]]
      if (is.null(v) || all(is.na(v))) return(NULL)
      c(paste0("-", k), as.character(v))
    }),
    use.names = FALSE
  )
  
  if (isTRUE(retCommand)) {
    # show what would be executed (best-effort)
    sys <- Sys.info()[["sysname"]]
    if (identical(sys, "Windows")) {
      return(paste(c(gili$otbCmd, algo, args), collapse = " "))
    } else {
      launcher <- gili$launcher
      if (is.null(launcher) || is.na(launcher) || !nzchar(launcher)) launcher <- gili$otbCmd
      return(paste(c(launcher, algo, args), collapse = " "))
    }
  }
  
  sys <- Sys.info()[["sysname"]]
  
  if (identical(sys, "Windows")) {
    # Windows: keep simple direct call; isolation via runOTB_isolated() (separate)
    status <- system2(
      gili$otbCmd,
      c(algo, args),
      stdout = if (quiet) TRUE else FALSE,
      stderr = if (quiet) TRUE else FALSE
    )
    return(invisible(status))
  }
  
  # Linux/macOS: launcher + explicit env
  otb_root  <- .otb_root_from_gili(gili)
  env_named <- .otb_env_linux(otb_root)
  
  out <- .otb_run_launcher_linux(
    gili   = gili,
    args   = c(algo, args),
    env    = env_named,
    stdout = FALSE,  # capture (per .otb_run_launcher_linux logic)
    stderr = FALSE
  )
  
  invisible(out)
}


#' Execute an OTB application in an isolated OTB environment (mainly Windows)
#'
#' - Windows: dot-sources `otbenv.ps1` (preferred) or calls `otbenv.bat`, then runs
#'   `otbcli` within the same shell session.
#' - Linux/macOS: delegates to [runOTB()] (launcher + explicit env already used).
#'
#' @param otbCmdList Non-empty list. `otbCmdList[[1]]` is the OTB application name.
#'   Named entries are parameter keys without leading dashes.
#' @param gili Optional list from [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @param retCommand Logical. If `TRUE`, returns the exact shell command that would
#'   be executed instead of running it.
#' @param quiet Logical. If `TRUE`, suppresses stdout/stderr (best-effort).
#'
#' @return If `retCommand=TRUE`, a character scalar command line. Otherwise an
#'   invisible status code.
#'
#' @export
runOTB_isolated <- function(otbCmdList,
                            gili = NULL,
                            retCommand = FALSE,
                            quiet = TRUE) {
  
  stopifnot(is.list(otbCmdList), length(otbCmdList) > 0)
  otbCmdList_0 <- otbCmdList
  
  if (is.null(gili)) gili <- linkOTB()
  if (!isTRUE(gili$exist)) stop("No valid OTB installation found.")
  
  algo <- otbCmdList[[1]]
  otbCmdList[[1]] <- NULL
  
  args <- unlist(
    lapply(names(otbCmdList), function(k) {
      v <- otbCmdList[[k]]
      if (is.null(v) || all(is.na(v))) return(NULL)
      c(paste0("-", k), as.character(v))
    }),
    use.names = FALSE
  )
  
  sys <- Sys.info()[["sysname"]]
  
  # Linux/macOS: workflow C already isolates via launcher + explicit env in runOTB()
  
  if (!identical(sys, "Windows")) {
    return(runOTB(
      otbCmdList = otbCmdList_0,
      gili = gili,
      retCommand = retCommand,
      quiet = quiet
    ))
  }
  
  # Windows: require envScript for true isolation
  envScript <- gili$envScript
  if (is.null(envScript) || is.na(envScript) || !nzchar(envScript) || !file.exists(envScript)) {
    stop("runOTB_isolated(): gili$envScript not found. Provide an OTB standalone bundle with otbenv.ps1/.bat or set gili$envScript.", call. = FALSE)
  }
  if (is.null(gili$otbCmd) || is.na(gili$otbCmd) || !nzchar(gili$otbCmd) || !file.exists(gili$otbCmd)) {
    stop("runOTB_isolated(): gili$otbCmd not found.", call. = FALSE)
  }
  
  envScript <- normalizePath(envScript, winslash = "\\", mustWork = FALSE)
  otbCmd    <- normalizePath(gili$otbCmd, winslash = "\\", mustWork = FALSE)
  
  ext <- tolower(tools::file_ext(envScript))
  
  if (isTRUE(retCommand)) {
    if (identical(ext, "ps1")) {
      ps <- paste0(
        "& { . ", shQuote(envScript),
        "; & ", shQuote(otbCmd), " ", shQuote(algo),
        if (length(args)) paste0(" ", paste(vapply(args, shQuote, character(1)), collapse = " ")) else "",
        " }"
      )
      return(paste(c("powershell.exe", "-NoProfile -ExecutionPolicy Bypass -Command", shQuote(ps)), collapse = " "))
    }
    
    if (identical(ext, "bat")) {
      cmdline <- paste0(
        "call ", shQuote(envScript),
        " && ", shQuote(otbCmd), " ", shQuote(algo),
        if (length(args)) paste0(" ", paste(vapply(args, shQuote, character(1)), collapse = " ")) else ""
      )
      return(paste(c("cmd.exe", "/c", shQuote(cmdline)), collapse = " "))
    }
    
    stop("runOTB_isolated(): Unsupported envScript extension: ", ext, call. = FALSE)
  }
  
  if (identical(ext, "ps1")) {
    ps <- paste0(
      "& { . ", shQuote(envScript),
      "; & ", shQuote(otbCmd), " ", shQuote(algo),
      if (length(args)) paste0(" ", paste(vapply(args, shQuote, character(1)), collapse = " ")) else "",
      " }"
    )
    
    status <- system2(
      "powershell.exe",
      c("-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", ps),
      stdout = if (quiet) TRUE else FALSE,
      stderr = if (quiet) TRUE else FALSE
    )
    return(invisible(status))
  }
  
  if (identical(ext, "bat")) {
    cmdline <- paste0(
      "call ", shQuote(envScript),
      " && ", shQuote(otbCmd), " ", shQuote(algo),
      if (length(args)) paste0(" ", paste(vapply(args, shQuote, character(1)), collapse = " ")) else ""
    )
    
    status <- system2(
      "cmd.exe",
      c("/c", cmdline),
      stdout = if (quiet) TRUE else FALSE,
      stderr = if (quiet) TRUE else FALSE
    )
    return(invisible(status))
  }
  
  stop("runOTB_isolated(): Unsupported envScript extension: ", ext, call. = FALSE)
}

# ------------------------------------------------------------------
# FOOTER — Migration / Workflow Information
# ------------------------------------------------------------------
# File:     R/otb_run.R
# Workflow: C (new consolidated OTB workflow)
#
# Role:
#   - Unified execution logic
#   - Linux uses launcher + explicit env (matches otb_capabilities/spec stack)
#   - Windows: runOTB() keeps direct otbcli call; runOTB_isolated() provides env-script isolation
# ------------------------------------------------------------------
