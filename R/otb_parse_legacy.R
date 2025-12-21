# R/otb_parse_legacy.R
#
# Public (LEGACY names, kept for API stability):
# - parseOTBAlgorithms()
# - parseOTBFunction()
#
# NEW workflow (C) underneath:
#' Linux/macOS: lists plugin libs \verb{otbapp_*.so}, \verb{otbapp_*.dylib}, \verb{otbapp_*.dll}
#' Windows: lists wrappers \verb{otbcli_<Algo>.ps1}, \verb{otbcli_<Algo>.bat}, \verb{otbcli_<Algo>.exe}#
# Policy:
# - keep return shapes compatible with old link2GI usage where feasible
# - do NOT do env mutation
# - do NOT shell out to "otbcli -print_applications"

# ------------------------ exported: parseOTBAlgorithms ------------------------

#' Retrieve available OTB applications
#'
#' Linux/macOS: lists `otbapp_*.\{so,dylib,dll\}` under OTB_APPLICATION_PATH derived
#' from OTB root.
#'
#' Windows: lists wrappers `otbcli_<Algo>.\{ps1,bat,exe\}` in `gili$pathOTB` (binDir).
#' #'
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @return Character vector of application names.
#' @export
parseOTBAlgorithms <- function(gili = NULL) {
  
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) {
    stop("No valid OTB installation found (gili$exist is FALSE).")
  }
  
  sys <- Sys.info()[["sysname"]]
  
  if (identical(sys, "Windows")) {
    
    # bin directory
    bin <- NULL
    if (!is.null(gili$binDir) && nzchar(gili$binDir)) bin <- gili$binDir
    if (is.null(bin) && !is.null(gili$pathOTB) && nzchar(gili$pathOTB)) bin <- gili$pathOTB
    if (is.null(bin)) stop("gili does not provide a valid OTB bin directory (binDir/pathOTB).")
    
    bin <- utils::shortPathName(bin)
    
    f <- list.files(
      path        = bin,
      pattern     = "^otbcli_.+\\.(ps1|bat|exe)$",
      full.names  = FALSE,
      ignore.case = TRUE
    )
    if (!length(f)) return(character(0))
    
    base  <- sub("\\.(ps1|bat|exe)$", "", f, ignore.case = TRUE)
    algos <- sub("^otbcli_", "", base, ignore.case = TRUE)
    algos <- algos[nzchar(algos)]
    
    return(sort(unique(algos)))
  }
  
  # Linux/macOS: derive from application plugin libraries
  otb_root  <- .otb_root_from_gili(gili)
  env_named <- .otb_env_linux(otb_root)
  
  app_path <- env_named[["OTB_APPLICATION_PATH"]]
  if (is.null(app_path) || !nzchar(app_path)) stop("OTB_APPLICATION_PATH could not be derived.")
  app_dirs <- strsplit(app_path, ":", fixed = TRUE)[[1]]
  app_dirs <- app_dirs[dir.exists(app_dirs)]
  if (!length(app_dirs)) stop("OTB_APPLICATION_PATH resolves to no existing directories.")
  
  files <- unlist(lapply(app_dirs, function(d) {
    list.files(d, pattern = "^otbapp_.*\\.(so|dylib|dll)$", full.names = FALSE)
  }), use.names = FALSE)
  
  if (!length(files)) return(character(0))
  
  algos <- sub("^otbapp_", "", files)
  algos <- sub("\\.(so|dylib|dll)$", "", algos, ignore.case = TRUE)
  
  sort(unique(algos))
}

# ------------------------ exported: parseOTBFunction ------------------------

#' Retrieve the argument list from an OTB application
#'
#' Legacy convenience wrapper that returns a list containing:
#' - first element: algo name
#' - named entries: parameter defaults (if any) and "mandatory" markers
#' - `$help`: per-parameter help text (if available)
#'
#' Under the hood this uses the NEW introspection API:
#' [otb_capabilities()] and [otb_args_spec()].
#'
#' @param algo Character. OTB application name (see [parseOTBAlgorithms()]).
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @return List (legacy format).
#' @export
parseOTBFunction <- function(algo = NULL, gili = NULL) {
  
  if (is.null(algo) || !nzchar(algo)) stop("`algo` must be a non-empty character string.")
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) stop("No valid OTB installation found (gili$exist is FALSE).")
  
  # NEW API (source of truth)
  caps <- link2GI::otb_capabilities(algo, gili, include_param_help = TRUE)
  spec <- link2GI::otb_args_spec(algo, gili)
  
  # tolerate both: spec is DF, or list(spec$params is DF)
  spec_df <- NULL
  if (is.data.frame(spec)) {
    spec_df <- spec
  } else if (is.list(spec) && !is.null(spec$params) && is.data.frame(spec$params)) {
    spec_df <- spec$params
  }
  
  if (is.null(spec_df) || !nrow(spec_df)) {
    stop(
      "OTB help output could not be parsed (no parameters found).\n",
      "First lines of output:\n",
      paste(utils::head(caps$text, 60), collapse = "\n")
    )
  }
  
  # Build legacy param list:
  # - mandatory -> "mandatory"
  # - optional with default -> default string
  # - progress default forced to "false" if missing
  param <- list()
  for (i in seq_len(nrow(spec_df))) {
    k <- spec_df$key[i]
    
    if (isTRUE(spec_df$mandatory[i])) {
      param[[k]] <- "mandatory"
      next
    }
    
    d <- spec_df$default[i]
    if (!is.na(d) && nzchar(d)) {
      param[[k]] <- d
    } else if (identical(k, "progress")) {
      param[["progress"]] <- "false"
    }
  }
  
  # Assemble legacy ocmd: first element algo (unnamed) without R.utils dependency
  ocmd <- c(list(algo), param)
  
  # Per-parameter help (already captured by otb_capabilities)
  helpList <- list()
  if (is.list(caps$param_help) && length(caps$param_help)) {
    helpList <- caps$param_help
  } else if ("progress" %in% names(ocmd)) {
    # historical special-casing
    helpList[["progress"]] <- "Report progress: It must be 0, 1, false or true"
  }
  
  ocmd$help <- helpList
  ocmd
}

# ------------------------------------------------------------------
# FOOTER — Migration / Workflow Information
# ------------------------------------------------------------------
# File:     R/otb_parse_legacy.R
# Workflow: C (new consolidated OTB workflow)
#
# Role:
#   - parseOTBAlgorithms(): legacy export; enumerates apps without executing OTB:
#       * Windows: wrapper enumeration (otbcli_<Algo>.*)
#       * Linux/macOS: plugin enumeration (otbapp_*.so|dylib|dll) via .otb_env_linux()
#   - parseOTBFunction(): legacy export; rebuilds old list shape from:
#       * otb_capabilities() + otb_args_spec() (new introspection core)
#   - No env mutation; no reliance on otbcli -print_applications.
#
# ------------------------------------------------------------------
