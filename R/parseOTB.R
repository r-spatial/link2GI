
# ------------------------ exported: parseOTBAlgorithms ------------------------

#' Retrieve available OTB applications
#'
#' On Linux, do NOT rely on `-print_applications` (not supported in your CLI layout).
#' Instead, list `otbapp_*.so` (or .dll/.dylib) under OTB_APPLICATION_PATH.
#' On Windows, list `otbcli_<Algo>.bat/.exe` wrappers in `bin`.
#'
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @return Character vector of application names.
#' @export
#' @examples
#' \dontrun{
#' otb <- link2GI::linkOTB()
#' if (otb$exist) parseOTBAlgorithms(otb)
#' }
parseOTBAlgorithms <- function(gili = NULL) {
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) {
    stop("No valid OTB installation found (gili$exist is FALSE).")
  }
  
  sys <- Sys.info()[["sysname"]]
  
  if (sys == "Windows") {
    bin <- utils::shortPathName(gili$pathOTB)
    
    # list candidates: otbcli_<Algo>[.bat|.exe|<noext>]
    f <- list.files(
      path = bin,
      pattern = "^otbcli_.+",
      full.names = FALSE,
      ignore.case = TRUE
    )
    
    if (!length(f)) return(character(0))
    
    # keep only wrappers we accept
    ok <- grepl("^otbcli_.+\\.(bat|exe)$", f, ignore.case = TRUE) |
      grepl("^otbcli_.+[^.]$", f, ignore.case = TRUE) # no extension
    
    f <- f[ok]
    if (!length(f)) return(character(0))
    
    # strip extension and "otbcli_"
    base <- sub("\\.(bat|exe)$", "", f, ignore.case = TRUE)
    algos <- sub("^otbcli_", "", base, ignore.case = TRUE)
    
    # drop empties
    algos <- algos[nzchar(algos)]
    
    return(sort(unique(algos)))
  }
  
  # Linux: derive module names from otbapp_*.so/.dylib/.dll in OTB_APPLICATION_PATH
  otb_root  <- .otb_root_from_gili(gili)
  env_named <- .otb_env_linux(otb_root)
  
  app_dirs <- strsplit(env_named[["OTB_APPLICATION_PATH"]], ":", fixed = TRUE)[[1]]
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
#' Queries `-help` output and returns a parameter list prefilled with detected defaults.
#'
#' @param algo Character. OTB application name (see [parseOTBAlgorithms()]).
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @return A list whose first element is the `algo` name, followed by parameters,
#'   plus a `$help` element with per-parameter help text.
#' @export
#' @examples
#' \dontrun{
#' otb <- link2GI::linkOTB()
#' if (otb$exist) parseOTBFunction("ComputeImagesStatistics", otb)
#' }
parseOTBFunction <- function(algo = NULL, gili = NULL) {
  if (is.null(algo) || !nzchar(algo)) stop("`algo` must be a non-empty character string.")
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) stop("No valid OTB installation found (gili$exist is FALSE).")
  
  # 1) Use robust capabilities parser (works for Linux via launcher, Windows via wrappers)
  caps <- link2GI::otb_capabilities(algo, gili, include_param_help = TRUE)
  spec <- link2GI::otb_args_spec(algo, gili)
  
  if (!is.data.frame(spec) || !nrow(spec)) {
    stop("OTB help output could not be parsed (no parameters found).\n",
         "First lines of output:\n",
         paste(utils::head(caps$text, 60), collapse = "\n"))
  }
  
  # 2) Build parameter list with defaults where available.
  # Keep historical behavior:
  # - mandatory -> "mandatory"
  # - progress -> "false" if missing
  param <- list()
  for (i in seq_len(nrow(spec))) {
    k <- spec$key[i]
    
    if (isTRUE(spec$mandatory[i])) {
      param[[k]] <- "mandatory"
      next
    }
    
    d <- spec$default[i]
    if (!is.na(d) && nzchar(d)) {
      param[[k]] <- d
    } else if (identical(k, "progress")) {
      param[["progress"]] <- "false"
    } else {
      # keep unset (historical: it was often absent unless default/mandatory/progress)
      # do nothing
    }
  }
  
  # 3) Assemble ocmd: first element algo
  ocmd <- R.utils::insert(param, 1, algo)
  
  # 4) Per-parameter help (already captured)
  helpList <- list()
  if (is.list(caps$param_help) && length(caps$param_help)) {
    helpList <- caps$param_help
  } else {
    # preserve old special-casing
    if ("progress" %in% names(ocmd)) {
      helpList[["progress"]] <- "Report progress: It must be 0, 1, false or true"
    }
  }
  
  ocmd$help <- helpList
  ocmd
}


# ------------------------ exported: runOTB ------------------------

# ------------------------ exported: runOTB ------------------------

#' Execute an OTB application
#'
#' Builds a CLI call from a parameter list created by [parseOTBFunction()] and executes it.
#' On Windows, supports OTB 9.x standalone PowerShell wrappers (.ps1) as well as legacy .bat/.exe.
#'
#' @param otbCmdList List as returned by [parseOTBFunction()], with parameters set.
#'   Must contain the application name either as first unnamed element, or in `.algo` / `algo`.
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @param retRaster Logical. If `TRUE`, attempt to read raster/vector/XML output.
#' @param retCommand Logical. If `TRUE`, return only a printable command string.
#' @param quiet Logical. If `FALSE`, stream stdout/stderr to console.
#' @return Depending on `retRaster` and output type: a `terra::SpatRaster`,
#'   an `xml2::xml_document`, an `sf` object, or `invisible(NULL)`.
#' @export
runOTB <- function(otbCmdList = NULL, gili = NULL,
                   retRaster = TRUE, retCommand = FALSE, quiet = TRUE) {
  
  .scalar_path <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.character(x) && length(x) >= 1) return(x[1])
    x
  }
  
  if (is.null(otbCmdList) || !is.list(otbCmdList) || !length(otbCmdList)) {
    stop("`otbCmdList` must be a non-empty list.", call. = FALSE)
  }
  
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) {
    stop("No valid OTB installation found (gili$exist is FALSE).", call. = FALSE)
  }
  
  # ---- robust algo detection ----
  nms <- names(otbCmdList)
  algo <- NULL
  
  # 1) preferred explicit fields
  if (!is.null(otbCmdList[[".algo"]])) algo <- otbCmdList[[".algo"]]
  if (is.null(algo) && !is.null(otbCmdList[["algo"]])) algo <- otbCmdList[["algo"]]
  
  # 2) legacy: first unnamed element (prints as `$` in str())
  if (is.null(algo)) {
    if (length(nms) >= 1 && (is.null(nms[1]) || !nzchar(nms[1]))) {
      algo <- otbCmdList[[1]]
      otbCmdList[[1]] <- NULL
    }
  }
  
  algo <- as.character(algo)[1]
  if (is.na(algo) || !nzchar(algo)) {
    stop("Could not determine OTB application name (algo). Expected `.algo`/`algo` or first unnamed list element.", call. = FALSE)
  }
  
  # remove help if present (parseOTBFunction often leaves it)
  if (!is.null(otbCmdList$help)) otbCmdList$help <- NULL
  
  # map known input aliases safely
  if (!is.null(otbCmdList[["input_in"]]) && is.null(otbCmdList[["in"]])) {
    otbCmdList[["in"]] <- otbCmdList[["input_in"]]
    otbCmdList[["input_in"]] <- NULL
  }
  if (!is.null(otbCmdList[["input_il"]]) && is.null(otbCmdList[["il"]])) {
    otbCmdList[["il"]] <- otbCmdList[["input_il"]]
    otbCmdList[["input_il"]] <- NULL
  }
  
  # output path for read-back (scalar path only)
  outn_path <- NULL
  if (!is.null(otbCmdList[["out.xml"]])) outn_path <- .scalar_path(otbCmdList[["out.xml"]])
  if (!is.null(otbCmdList[["out"]]))     outn_path <- .scalar_path(otbCmdList[["out"]])
  if (!is.null(otbCmdList[["io.out"]]))  outn_path <- .scalar_path(otbCmdList[["io.out"]])
  
  # normalize first token for known path params (keep token vectors like c(path,"float")!)
  absify <- function(x) {
    if (is.character(x) && length(x) >= 1 && nzchar(x[1])) {
      x[1] <- normalizePath(x[1], mustWork = FALSE)
    }
    x
  }
  for (k in c("in","il","out","out.xml","io.out")) {
    if (!is.null(otbCmdList[[k]])) otbCmdList[[k]] <- absify(otbCmdList[[k]])
  }
  
  # refresh outn_path after absify
  if (!is.null(otbCmdList[["out.xml"]])) outn_path <- .scalar_path(otbCmdList[["out.xml"]])
  if (!is.null(otbCmdList[["out"]]))     outn_path <- .scalar_path(otbCmdList[["out"]])
  if (!is.null(otbCmdList[["io.out"]]))  outn_path <- .scalar_path(otbCmdList[["io.out"]])
  
  # build args (token-safe)
  args <- character(0)
  for (k in names(otbCmdList)) {
    v <- otbCmdList[[k]]
    if (is.null(v)) next
    
    if (is.logical(v)) v <- if (isTRUE(v)) "1" else "0"
    v <- as.character(v)
    v <- v[nzchar(v)]
    if (!length(v)) next
    
    if (identical(k, "progress")) {
      args <- c(args, paste0("-progress=", v[1]))
    } else {
      args <- c(args, paste0("-", k), v)
    }
  }
  
  if (isTRUE(retCommand)) {
    return(paste0("[OTB] ", algo, " ", paste(args, collapse = " ")))
  }
  
  sys <- Sys.info()[["sysname"]]
  status <- 0L
  
  # stdout/stderr routing
  so <- if (isTRUE(quiet)) TRUE else ""
  se <- if (isTRUE(quiet)) TRUE else ""
  
  if (sys == "Windows") {
    
    # prefer binDir if present (your gili has it), else fall back to pathOTB
    bin <- NULL
    if (!is.null(gili$binDir) && nzchar(gili$binDir)) bin <- gili$binDir
    if (is.null(bin) && !is.null(gili$pathOTB) && nzchar(gili$pathOTB)) bin <- gili$pathOTB
    if (is.null(bin)) stop("gili does not provide a valid OTB bin directory (binDir/pathOTB).", call. = FALSE)
    
    bin <- normalizePath(bin, winslash = "\\", mustWork = FALSE)
    
    w_ps1 <- file.path(bin, paste0("otbcli_", algo, ".ps1"))
    w_bat <- file.path(bin, paste0("otbcli_", algo, ".bat"))
    w_exe <- file.path(bin, paste0("otbcli_", algo, ".exe"))
    
    if (file.exists(w_ps1)) {
      # run wrapper directly; wrapper typically takes plain args without keys? -> NO: we pass args as built (with -k v tokens)
      status <- system2(
        "powershell.exe",
        args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(w_ps1), args),
        stdout = so, stderr = se
      )
    } else if (file.exists(w_bat)) {
      status <- system2(
        "cmd.exe",
        args = c("/c", shQuote(w_bat), args),
        stdout = so, stderr = se
      )
    } else if (file.exists(w_exe)) {
      status <- system2(
        w_exe,
        args = args,
        stdout = so, stderr = se
      )
    } else {
      stop(
        "Missing OTB CLI wrapper for application '", algo, "'.\n",
        "Searched in: ", bin, "\n",
        "Expected one of:\n  ", basename(w_ps1), "\n  ", basename(w_bat), "\n  ", basename(w_exe),
        call. = FALSE
      )
    }
    
  } else {
    # keep your existing launcher path (assumes .otb_run_launcher exists)
    status <- .otb_run_launcher(gili, args = c(algo, args), stdout = so, stderr = se)
  }
  
  if (!isTRUE(retRaster)) return(invisible(NULL))
  if (is.null(outn_path) || !nzchar(outn_path)) return(invisible(NULL))
  
  if (!identical(as.integer(status), 0L)) {
    stop("OTB execution failed (exit status ", status, "). Output not produced: ", outn_path, call. = FALSE)
  }
  if (!file.exists(outn_path)) {
    stop("OTB returned exit status 0 but output file does not exist: ", outn_path, call. = FALSE)
  }
  
  ext <- tolower(tools::file_ext(outn_path))
  if (ext %in% c("tif", "tiff", "vrt")) return(terra::rast(outn_path))
  if (ext == "xml") return(xml2::read_xml(outn_path))
  if (!is.null(otbCmdList[["mode"]]) && identical(otbCmdList[["mode"]], "vector")) {
    return(sf::st_read(outn_path, quiet = TRUE))
  }
  
  invisible(NULL)
}


#' Run Orfeo Toolbox applications in an isolated environment session
#'
#' Executes an OTB command while ensuring that the OTB runtime environment is
#' initialized **in the same shell session** as the application call.
#'
#' This is mainly needed for **OTB standalone bundles** where the environment
#' must be activated via an `otbenv` script:
#' - **Windows OTB >= 9.x**: `otbenv.ps1` must be **dot-sourced** in PowerShell
#'   in the same session as the call.
#' - **Windows legacy**: `otbenv.bat` / `otbenv.cmd` can be chained in `cmd.exe`.
#' - **Linux/macOS**: `otbenv.profile` is sourced in a `bash -lc` session.
#'
#' The function uses [runOTB()] to build a printable command line (with
#' `retCommand = TRUE`) but executes it in a separate, isolated process with the
#' correct environment initialization.
#'
#' @param otbCmdList Named list of OTB parameters (as used by [runOTB()]).
#'   Must not be `NULL`. Typical keys include `in`, `out`, `io.out`, `out.xml`,
#'   and algorithm-specific parameters.
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is
#'   called internally.
#' @param retRaster Logical; if `TRUE` (default) and an output file path can be
#'   inferred, reads the produced output back into R (raster/XML/vector as
#'   appropriate).
#' @param retCommand Logical; if `TRUE`, returns only the command string built by
#'   [runOTB()] and does not execute anything.
#' @param quiet Logical; if `TRUE` (default), suppress stdout/stderr from the
#'   system call. Set to `FALSE` for debugging.
#'
#' @return
#' If `retCommand = TRUE`, a character string containing the command line.
#'
#' Otherwise returns one of:
#' - A `terra::SpatRaster` if the inferred output is `tif/tiff/vrt` and
#'   `retRaster = TRUE`.
#' - An `xml2::xml_document` if the inferred output is `xml` and
#'   `retRaster = TRUE`.
#' - An `sf` object if `otbCmdList$mode == "vector"` and `retRaster = TRUE`.
#' - Otherwise, returns the (invisible) result from the underlying `system2()`
#'   call (typically exit status / captured output depending on platform).
#'
#' @details
#' Output path inference checks, in order: `out.xml`, `out`, then `io.out`.
#' If the output does not exist after execution, the function errors.
#'
#' Note: This function assumes that `gili$envScript` points to a valid
#' environment initializer: `otbenv.profile` (Linux/macOS) or
#' `otbenv.ps1`/`otbenv.bat` (Windows).
#'
#' @examples
#' \dontrun{
#' library(link2GI)
#'
#' otb <- linkOTB()
#' cmd <- list(
#'   in  = "input.tif",
#'   out = "pca.tif",
#'   method = "pca",
#'   nbcomp = "3"
#' )
#'
#' # Print the command only
#' runOTB_isolated(cmd, otb, retCommand = TRUE)
#'
#' # Execute and read output raster
#' r <- runOTB_isolated(cmd, otb, retRaster = TRUE, quiet = FALSE)
#' }
#'
#' @export
runOTB_isolated <- function(otbCmdList = NULL, gili = NULL,
                            retRaster = TRUE, retCommand = FALSE,
                            quiet = TRUE) {
  stopifnot(!is.null(otbCmdList))
  if (is.null(gili)) gili <- link2GI::linkOTB()
  
  # 1) use existing runOTB to build the printable command, but do NOT execute it there
  command <- runOTB(otbCmdList = otbCmdList, gili = gili, retCommand = TRUE)
  
  if (isTRUE(retCommand)) return(command)
  
  sys <- Sys.info()[["sysname"]]
  
  # 2) derive env script from gili (must exist)
  envScript <- gili$envScript
  if (is.null(envScript) || is.na(envScript) || !file.exists(envScript)) {
    stop("OTB env script not found. Expected otbenv.profile (Linux) or otbenv.ps1/.bat (Windows).")
  }
  
  # 3) execute isolated (same shell/session as env init script)
  if (sys == "Windows") {
    
    envScript_n <- normalizePath(envScript, winslash = "\\", mustWork = TRUE)
    
    if (grepl("\\.ps1$", envScript_n, ignore.case = TRUE)) {
      # Standalone OTB >= 9.x: PowerShell env init must be dot-sourced in the SAME session
      ps_cmd <- paste0('& { . "', envScript_n, '"; ', command, ' }')
      
      out <- system2(
        "powershell.exe",
        args   = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", ps_cmd),
        stdout = if (quiet) TRUE else FALSE,
        stderr = if (quiet) TRUE else FALSE
      )
      
    } else {
      # Legacy: .bat/.cmd can be chained within cmd.exe
      envScript_q <- shQuote(envScript_n)
      full <- paste0(envScript_q, " && ", command)
      
      out <- system2(
        "cmd.exe",
        args   = c("/c", full),
        stdout = if (quiet) TRUE else FALSE,
        stderr = if (quiet) TRUE else FALSE
      )
    }
    
  } else {
    # Linux/mac: run in bash login shell with local env setup
    envScript_q <- shQuote(normalizePath(envScript, mustWork = TRUE))
    full <- sprintf(". %s; %s", envScript_q, command)
    
    out <- system2(
      "bash",
      args   = c("-lc", full),
      stdout = if (quiet) TRUE else FALSE,
      stderr = if (quiet) TRUE else FALSE
    )
  }
  
  # 4) return like runOTB (post-read)
  outn <- NULL
  if (!is.null(otbCmdList$out.xml))  outn <- otbCmdList$out.xml
  if (!is.null(otbCmdList$out))      outn <- otbCmdList$out
  if (!is.null(otbCmdList$`io.out`)) outn <- otbCmdList$`io.out`
  
  if (!isTRUE(retRaster)) return(invisible(out))
  if (is.null(outn)) return(invisible(out))
  
  # If OTB failed, file won't exist
  if (!file.exists(outn)) {
    stop("OTB did not produce expected output: ", outn,
         "\n(Enable quiet=FALSE to inspect stderr/stdout.)")
  }
  
  ext <- tolower(tools::file_ext(outn))
  if (ext %in% c("tif", "tiff", "vrt")) return(terra::rast(outn))
  if (ext == "xml") return(xml2::read_xml(outn))
  
  # vector mode (read as sf)
  if (!is.null(otbCmdList$mode) && identical(otbCmdList$mode, "vector")) {
    return(sf::st_read(outn, quiet = TRUE))
  }
  
  invisible(out)
}
