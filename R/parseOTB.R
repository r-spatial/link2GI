
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
  
  arg_lines <- txt[grepl("^\\s*(MISSING\\s+)?-", txt)]
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
  
  .scalar_path <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.character(x) && length(x) >= 1) return(x[1])
    x
  }
  
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
  
  # output path (for reading back) - keep full token vector, but compute scalar path early
  outn <- NULL
  if (!is.null(otbCmdList[["out.xml"]])) outn <- otbCmdList[["out.xml"]]
  if (!is.null(otbCmdList[["out"]]))     outn <- otbCmdList[["out"]]
  if (!is.null(otbCmdList[["io.out"]]))  outn <- otbCmdList[["io.out"]]
  
  outn_path <- .scalar_path(outn)  # DEFINE EARLY, always
  
  # normalize paths (only first token if vector like c(path,"float"))
  absify <- function(x) {
    if (is.character(x) && length(x) >= 1) {
      x[1] <- normalizePath(x[1], mustWork = FALSE)
    }
    x
  }
  
  if (!is.null(otbCmdList[["in"]]))      otbCmdList[["in"]]      <- absify(otbCmdList[["in"]])
  if (!is.null(otbCmdList[["il"]]))      otbCmdList[["il"]]      <- absify(otbCmdList[["il"]])
  if (!is.null(otbCmdList[["out"]]))     otbCmdList[["out"]]     <- absify(otbCmdList[["out"]])
  if (!is.null(otbCmdList[["out.xml"]])) otbCmdList[["out.xml"]] <- absify(otbCmdList[["out.xml"]])
  if (!is.null(otbCmdList[["io.out"]]))  otbCmdList[["io.out"]]  <- absify(otbCmdList[["io.out"]])
  
  # outn_path may have changed due to absify
  if (!is.null(otbCmdList[["out.xml"]])) outn_path <- .scalar_path(otbCmdList[["out.xml"]])
  if (!is.null(otbCmdList[["out"]]))     outn_path <- .scalar_path(otbCmdList[["out"]])
  if (!is.null(otbCmdList[["io.out"]]))  outn_path <- .scalar_path(otbCmdList[["io.out"]])
  
  # build args (token-safe; do NOT collapse vectors like c(path,"float"))
  args <- character(0)
  keys <- names(otbCmdList)
  
  for (k in keys) {
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
  
  if (sys == "Windows") {
    bin <- utils::shortPathName(gili$pathOTB)
    exe <- file.path(bin, paste0("otbcli_", algo, ".bat"))
    if (!file.exists(exe)) exe <- file.path(bin, paste0("otbcli_", algo))
    if (!file.exists(exe)) stop("Missing OTB CLI wrapper: ", exe)
    
    status <- system2(exe, args, stdout = !quiet, stderr = !quiet)
  } else {
    status <- .otb_run_launcher(gili, args = c(algo, args), stdout = !quiet, stderr = !quiet)
  }
  
  # return nothing if not requested
  if (!isTRUE(retRaster)) return(invisible(NULL))
  
  # if no output specified, nothing to read
  if (is.null(outn_path) || !nzchar(outn_path)) return(invisible(NULL))
  
  # fail explicitly if OTB failed
  if (!identical(status, 0L)) {
    stop("OTB execution failed (exit status ", status, "). Output not produced: ", outn_path)
  }
  
  # fail explicitly if output missing
  if (!file.exists(outn_path)) {
    stop("OTB returned exit status 0 but output file does not exist: ", outn_path)
  }
  
  ext <- tolower(tools::file_ext(outn_path))
  
  if (ext %in% c("tif", "tiff", "vrt")) return(terra::rast(outn_path))
  if (ext == "xml") return(xml2::read_xml(outn_path))
  if (!is.null(otbCmdList[["mode"]]) && identical(otbCmdList[["mode"]], "vector")) {
    return(sf::st_read(outn_path, quiet = TRUE))
  }
  
  invisible(NULL)
}

