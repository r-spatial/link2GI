# ------------------------ internal helpers ------------------------

#' @keywords internal
.otb_root_from_gili <- function(gili) {
  stopifnot(isTRUE(gili$exist))
  if (!is.null(gili$otbRoot) && nzchar(gili$otbRoot)) {
    return(normalizePath(gili$otbRoot, mustWork = TRUE))
  }
  p <- normalizePath(gili$pathOTB, mustWork = TRUE)  # .../bin/
  normalizePath(file.path(p, ".."), mustWork = TRUE) # .../
}

#' @keywords internal
.otb_env_linux <- function(otb_root) {
  otb_root <- normalizePath(otb_root, mustWork = TRUE)
  
  app_paths <- c(
    file.path(otb_root, "lib",   "otb", "applications"),
    file.path(otb_root, "lib64", "otb", "applications")
  )
  app_paths <- app_paths[dir.exists(app_paths)]
  if (!length(app_paths)) stop("No OTB applications dir found under: ", otb_root)
  
  lib_paths <- c(file.path(otb_root, "lib"), file.path(otb_root, "lib64"))
  lib_paths <- lib_paths[dir.exists(lib_paths)]
  if (!length(lib_paths)) stop("No OTB lib dir found under: ", otb_root)
  
  env_named <- c(
    OTB_INSTALL_DIR      = otb_root,
    CMAKE_PREFIX_PATH    = otb_root,
    OTB_APPLICATION_PATH = paste(app_paths, collapse = ":"),
    GDAL_DATA            = file.path(otb_root, "share", "gdal"),
    PROJ_LIB             = file.path(otb_root, "share", "proj"),
    GDAL_DRIVER_PATH     = "disable",
    LC_NUMERIC           = "C",
    PATH                 = paste(c(file.path(otb_root, "bin"), Sys.getenv("PATH")), collapse = ":"),
    PYTHONPATH           = paste(c(file.path(otb_root, "lib", "otb", "python"),
                                   Sys.getenv("PYTHONPATH")), collapse = ":"),
    LD_LIBRARY_PATH      = paste(c(lib_paths, Sys.getenv("LD_LIBRARY_PATH")), collapse = ":")
  )
  
  env_named <- env_named[nzchar(env_named)]
  env_named
}

#' @keywords internal
.otb_launcher_path_linux <- function(otb_root) {
  otb_root <- normalizePath(otb_root, mustWork = TRUE)
  launcher <- file.path(otb_root, "bin", "otbApplicationLauncherCommandLine")
  if (!file.exists(launcher)) {
    stop("Missing OTB launcher: ", launcher,
         "\n(OTB install seems incomplete or otbRoot wrong)")
  }
  launcher
}

#' @keywords internal
#' @keywords internal
.otb_run_launcher <- function(gili, args, stdout = TRUE, stderr = TRUE) {
  sys <- Sys.info()[["sysname"]]
  stopifnot(isTRUE(gili$exist))
  
  if (sys == "Windows") {
    stop("Internal error: .otb_run_launcher() should not be called on Windows.")
  }
  
  otb_root <- .otb_root_from_gili(gili)
  
  # prefer the real launcher; fallback to otbcli if needed
  launcher <- file.path(otb_root, "bin", "otbApplicationLauncherCommandLine")
  if (!file.exists(launcher)) {
    launcher <- file.path(otb_root, "bin", "otbcli")
    if (!file.exists(launcher)) stop("No OTB launcher/cli found under: ", otb_root)
  }
  
  env_named <- .otb_env_linux(otb_root)
  stopifnot(is.character(env_named), length(names(env_named)) == length(env_named))
  
  # build: VAR='value' VAR2='value2' <launcher> <args...>
  env_part <- paste0(names(env_named), "=", vapply(unname(env_named), shQuote, character(1)))
  cmd_part <- c(shQuote(launcher), vapply(args, shQuote, character(1)))
  cmd <- paste(c(env_part, cmd_part), collapse = " ")
  
  # run via bash to ensure quoting survives shell wrappers
  system2("bash", c("-lc", cmd), stdout = stdout, stderr = stderr)
}


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
  if (is.null(gili) || !isTRUE(gili$exist)) stop("No valid OTB installation found (gili$exist is FALSE).")
  
  sys <- Sys.info()[["sysname"]]
  
  if (sys == "Windows") {
    path_OTB <- utils::shortPathName(gili$pathOTB)
    f <- list.files(path_OTB, pattern = "^otbcli_.*(\\.bat|\\.exe)?$", full.names = FALSE, ignore.case = TRUE)
    f <- f[grepl("^otbcli_", f, ignore.case = TRUE)]
    f <- sub("\\.bat$", "", f, ignore.case = TRUE)
    f <- sub("\\.exe$", "", f, ignore.case = TRUE)
    algos <- sub("^otbcli_", "", f, ignore.case = TRUE)
    return(sort(unique(algos)))
  }
  
  # Linux: derive module names from otbapp_*.so in applications dirs
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
  
  sys <- Sys.info()[["sysname"]]
  
  # full help output
  if (sys == "Windows") {
    bin <- utils::shortPathName(gili$pathOTB)
    exe <- file.path(bin, paste0("otbcli_", algo, ".bat"))
    if (!file.exists(exe)) {
      exe2 <- file.path(bin, paste0("otbcli_", algo))
      if (!file.exists(exe2)) stop("Missing OTB CLI wrapper: ", exe)
      exe <- exe2
    }
    txt <- system2(exe, c("-help"), stdout = TRUE, stderr = TRUE)
  } else {
    txt <- .otb_run_launcher(gili, args = c(algo, "-help"), stdout = TRUE, stderr = TRUE)
  }
  
  arg_lines <- txt[grepl("^\\s*-", txt)]
  if (!length(arg_lines)) {
    stop("OTB help output could not be parsed (no argument lines found).\n",
         "First lines of output:\n",
         paste(utils::head(txt, 60), collapse = "\n"))
  }
  
  # normalize & split
  args <- gsub("\t", " ", arg_lines, fixed = TRUE)
  args <- gsub(" +", "   ", args)
  args <- strsplit(args, "   ", fixed = TRUE)
  
  param <- list()
  
  for (row in args) {
    field2 <- if (length(row) >= 2) row[2] else ""
    field4 <- if (length(row) >= 4) row[4] else ""
    
    line_all <- paste(row, collapse = " ")
    drop <- grepl("(OTB-Team)|(-help)|(otbcli_)|(-inxml)", line_all)
    
    default <- ""
    if (!drop) {
      if (grepl("default value is", field4, fixed = TRUE)) {
        tmp <- strsplit(field4, "default value is ", fixed = TRUE)[[1]]
        if (length(tmp) >= 2) default <- strsplit(tmp[2], ")", fixed = TRUE)[[1]][1]
      } else if (grepl("(mandatory)", field4)) {
        default <- "mandatory"
      } else if (identical(field4, "Report progress ")) {
        default <- "false"
      } else {
        default <- ""  
      }
    }
    
    if (!drop && nzchar(default) && nzchar(field2)) {
      arg <- field2
      if (identical(arg, "-in")) arg <- "-input_in"
      if (identical(arg, "-il")) arg <- "-input_il"
      if (nchar(arg) >= 2) param[[substr(arg, 2, nchar(arg))]] <- default
    }
  }
  
  ocmd <- R.utils::insert(param, 1, algo)
  
  # per-parameter help
  helpList <- list()
  t <- ocmd
  t[[1]] <- NULL
  
  for (arg in names(t)) {
    if (identical(arg, "progress")) {
      helpList[["progress"]] <- "Report progress: It must be 0, 1, false or true"
      next
    }
    
    if (sys == "Windows") {
      bin <- utils::shortPathName(gili$pathOTB)
      exe <- file.path(bin, paste0("otbcli_", algo, ".bat"))
      if (!file.exists(exe)) exe <- file.path(bin, paste0("otbcli_", algo))
      out <- system2(exe, c("-help", arg), stdout = TRUE, stderr = TRUE)
    } else {
      out <- .otb_run_launcher(gili, args = c(algo, "-help", arg), stdout = TRUE, stderr = TRUE)
    }
    
    out <- unique(out)
    dropi <- c(grep("\\w*no version information available\\w*", out), grep("^\\s*$", out))
    if (length(dropi)) out <- out[-dropi]
    helpList[[arg]] <- out
  }
  
  ocmd$help <- helpList
  ocmd
}

# ------------------------ exported: runOTB ------------------------

#' Execute an OTB application
#'
#' Builds a CLI call from a parameter list created by [parseOTBFunction()] and executes it.
#' On Linux, execution uses the OTB launcher with a call-local environment (no global env mutation).
#'
#' @param otbCmdList List as returned by [parseOTBFunction()], with parameters set.
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @param retRaster Logical. If `TRUE`, attempt to read raster/vector/XML output.
#' @param retCommand Logical. If `TRUE`, return only a printable command string.
#' @param quiet Logical. If `FALSE`, do not suppress stdout/stderr.
#' @return Depending on `retRaster` and output type: a `terra::SpatRaster`,
#'   an `xml2::xml_document`, an `sf` object, or `invisible(NULL)`.
#' @export
#' @examples
#' \dontrun{
#' otb <- link2GI::linkOTB()
#' if (otb$exist) {
#'   cmd <- parseOTBFunction("ComputeImagesStatistics", otb)
#'   cmd[["input_il"]] <- "/path/to/image.tif"
#'   cmd[["out.xml"]]  <- tempfile(fileext = ".xml")
#'   runOTB(cmd, otb, retRaster = TRUE, quiet = FALSE)
#' }
#' }
runOTB <- function(otbCmdList = NULL, gili = NULL,
                   retRaster = TRUE, retCommand = FALSE, quiet = TRUE) {
  
  if (is.null(otbCmdList) || !length(otbCmdList))
    stop("`otbCmdList` must be a non-empty list.")
  
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (!isTRUE(gili$exist)) stop("No valid OTB installation found (gili$exist is FALSE).")
  
  algo <- unlist(otbCmdList[1], use.names = FALSE)
  otbCmdList[1] <- NULL
  otbCmdList$help <- NULL
  
  # map input keys safely
  if (!is.null(otbCmdList[["input_in"]]) && is.null(otbCmdList[["in"]])) {
    otbCmdList[["in"]] <- otbCmdList[["input_in"]]
    otbCmdList[["input_in"]] <- NULL
  }
  if (!is.null(otbCmdList[["input_il"]]) && is.null(otbCmdList[["il"]])) {
    otbCmdList[["il"]] <- otbCmdList[["input_il"]]
    otbCmdList[["input_il"]] <- NULL
  }
  
  # output path (for reading back)
  outn <- NULL
  if (!is.null(otbCmdList[["out.xml"]])) outn <- otbCmdList[["out.xml"]]
  if (!is.null(otbCmdList[["out"]]))     outn <- otbCmdList[["out"]]
  if (!is.null(otbCmdList[["io.out"]]))  outn <- otbCmdList[["io.out"]]
  
  absify <- function(x) if (is.character(x) && length(x) == 1) normalizePath(x, mustWork = FALSE) else x
  if (!is.null(otbCmdList[["in"]]))      otbCmdList[["in"]]      <- absify(otbCmdList[["in"]])
  if (!is.null(otbCmdList[["il"]]))      otbCmdList[["il"]]      <- absify(otbCmdList[["il"]])
  if (!is.null(otbCmdList[["out"]]))     otbCmdList[["out"]]     <- absify(otbCmdList[["out"]])
  if (!is.null(otbCmdList[["out.xml"]])) otbCmdList[["out.xml"]] <- absify(otbCmdList[["out.xml"]])
  if (!is.null(otbCmdList[["io.out"]]))  otbCmdList[["io.out"]]  <- absify(otbCmdList[["io.out"]])
  
  keys <- names(otbCmdList)
  vals <- unname(otbCmdList)
  
  vals <- lapply(vals, function(v) if (length(v) > 1) paste(v, collapse = " ") else v)
  vals <- as.character(vals)
  
  args <- character(0)
  for (i in seq_along(keys)) {
    k <- keys[[i]]
    v <- vals[[i]]
    if (is.na(v) || !nzchar(v)) next
    
    if (identical(k, "progress")) {
      args <- c(args, paste0("-progress=", v))   # <-- wichtig
    } else {
      args <- c(args, paste0("-", k), v)
    }
  }
  
  
  if (isTRUE(retCommand)) {
    return(paste0("[OTB] ", algo, " ", paste(args, collapse = " ")))
  }
  
  sys <- Sys.info()[["sysname"]]
  
  if (sys == "Windows") {
    bin <- utils::shortPathName(gili$pathOTB)
    exe <- file.path(bin, paste0("otbcli_", algo, ".bat"))
    if (!file.exists(exe)) exe <- file.path(bin, paste0("otbcli_", algo))
    if (!file.exists(exe)) stop("Missing OTB CLI wrapper: ", exe)
    system2(exe, args, stdout = !quiet, stderr = !quiet)
  } else {
    .otb_run_launcher(gili, args = c(algo, args), stdout = !quiet, stderr = !quiet)
  }
  
  if (!retRaster || is.null(outn) || !nzchar(outn)) return(invisible(NULL))
  
  ext <- tolower(tools::file_ext(outn))
  if (ext %in% c("tif", "tiff", "vrt")) return(terra::rast(outn))
  if (ext == "xml") return(xml2::read_xml(outn))
  if (!is.null(otbCmdList[["mode"]]) && identical(otbCmdList[["mode"]], "vector")) {
    return(sf::st_read(outn, quiet = TRUE))
  }
  
  invisible(NULL)
}
