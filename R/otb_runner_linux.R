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
.otb_run_launcher <- function(gili, args, stdout = TRUE, stderr = TRUE, capture = FALSE) {
  
  # Resolve launcher path (prefer explicit root; tolerate bin/)
  .otb_launcher_path <- function(gili) {
    root <- if (!is.null(gili$otbRoot) && nzchar(gili$otbRoot)) gili$otbRoot else gili$pathOTB
    root <- sub("/+$", "", root)
    if (grepl("/bin$", root)) root <- dirname(root)
    file.path(root, "bin", "otbApplicationLauncherCommandLine")
  }
  
  launcher <- .otb_launcher_path(gili)
  
  # Build env from root (unchanged logic)
  root <- if (!is.null(gili$otbRoot) && nzchar(gili$otbRoot)) gili$otbRoot else gili$pathOTB
  root <- sub("/+$", "", root)
  if (grepl("/bin$", root)) root <- dirname(root)
  bin  <- file.path(root, "bin")
  
  env_vec <- c(
    paste0("OTB_INSTALL_DIR=", root),
    paste0("CMAKE_PREFIX_PATH=", root),
    paste0("OTB_APPLICATION_PATH=", file.path(root, "lib/otb/applications")),
    paste0("GDAL_DATA=", file.path(root, "share/gdal")),
    paste0("PROJ_LIB=", file.path(root, "share/proj")),
    "GDAL_DRIVER_PATH=disable",
    "LC_NUMERIC=C",
    paste0("PATH=", bin, ":", Sys.getenv("PATH")),
    paste0("PYTHONPATH=", file.path(root, "lib/otb/python"), ":", Sys.getenv("PYTHONPATH")),
    paste0("LD_LIBRARY_PATH=", file.path(root, "lib"), ":", Sys.getenv("LD_LIBRARY_PATH"))
  )
  
  if (isTRUE(capture)) {
    out <- system2(
      command = launcher,
      args    = args,
      env     = env_vec,
      stdout  = TRUE,   # capture
      stderr  = TRUE
    )
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    return(list(status = as.integer(status), output = out))
  }
  
  # NOTE: no "" redirections; TRUE=capture/suppress, FALSE=inherit console
  status <- system2(
    command = launcher,
    args    = args,
    env     = env_vec,
    stdout  = if (stdout) FALSE else TRUE,
    stderr  = if (stderr) FALSE else TRUE
  )
  
  as.integer(status)
}



#' @keywords internal
.otb_help_text <- function(gili, algo, param = NULL) {
  
  help_args <- c(algo, "-help")
  if (!is.null(param) && length(param) > 0) {
    help_args <- c(help_args, as.character(param))
  }
  
  rr <- .otb_run_launcher(gili, args = help_args, capture = TRUE)
  
  txt <- rr$output
  has_params_block <- any(grepl("^Parameters\\s*:", txt))
  
  # accept non-zero exit code for -help if output is parseable
  if (!identical(rr$status, 0L) && !has_params_block && is.null(param)) {
    stop("OTB help call failed (exit status ", rr$status, ").\n",
         paste(txt, collapse = "\n"))
  }
  
  txt
}

