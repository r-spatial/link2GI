#' @importFrom stats setNames
NULL


# R/gdalRun.R
# ------------------------------------------------------------------------------
# GDAL CLI runner + fingerprint + help cache + minimal skeleton + NDJSON logging
# ------------------------------------------------------------------------------
# Design goals:
# - Use the *linked* GDAL binaries (from linkGDAL/findGDAL) deterministically.
# - No implicit PATH reliance (per-call env).
# - Provide a GDAL "fingerprint" and NDJSON run logs for reproducibility.
# - Provide a minimal, valid skeleton generated from "--help" (no fake semantics).
#
# Dependencies: base R only (optional jsonlite if installed; falls back to safe writer)
# ------------------------------------------------------------------------------

# ---- small helpers ------------------------------------------------------------

# helper: null-coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

# helper: normalize a "gdal link" list to a context with guaranteed fields
gdal_normalize_ctx <- function(gdal) {
  if (!is.list(gdal)) stop("gdal must be a list/context.", call. = FALSE)
  
  # env: always a named character vector
  env <- gdal$env
  if (is.null(env)) {
    env <- character(0)
  } else if (is.environment(env)) {
    env <- as.list(env, all.names = TRUE)
  }
  
  if (is.list(env)) {
    # flatten list -> named character vector (first element only)
    env <- vapply(names(env), function(nm) {
      x <- env[[nm]]
      if (length(x) == 0 || is.null(x)) NA_character_ else as.character(x[[1]])
    }, FUN.VALUE = character(1))
    names(env) <- names(env) %||% character(0)
  } else {
    env <- as.character(env)
    if (is.null(names(env))) names(env) <- character(length(env))
  }
  
  if (length(env) == 0 || is.null(names(env))) {
    env <- setNames(character(0), character(0))
  }
  
  gdal$env <- env
  
  # also guarantee a place for binDir/path if your downstream expects it
  gdal$path <- gdal$path %||% gdal$binDir %||% NA_character_
  gdal
}

# safe getter for env keys
.gdal_env_get <- function(env, key, default = NA_character_) {
  if (is.null(env)) return(default)
  nms <- names(env)
  if (is.null(nms) || !(key %in% nms)) return(default)
  v <- env[[key]]
  if (is.null(v) || length(v) == 0) default else as.character(v[[1]])
}


# ---- PATCH the two callers (minimal edits) ----

gdal_fingerprint <- function(gdal) {
  gdal <- gdal_normalize_ctx(gdal)
  
  list(
    path      = gdal$path %||% NA_character_,
    gdal_data = gdal_env_get(gdal, "GDAL_DATA", NA_character_),
    # add whatever you already fingerprint (version, exe paths, etc.)
    env_keys  = names(gdal$env)
  )
}

gdal_skeleton <- function(gdal, tool) {
  gdal <- gdal_normalize_ctx(gdal)
  
  gdald <- gdal_env_get(gdal, "GDAL_DATA", NA_character_)
  # ... keep your current skeleton logic; just do NOT index gdal$env blindly.
  # Example placeholder:
  list(
    tool = tool,
    gdal_data = gdald,
    args = list()
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

.gdal_norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)

.gdal_is_windows <- function() identical(Sys.info()[["sysname"]], "Windows")

.gdal_bin_ext <- function() if (.gdal_is_windows()) ".exe" else ""

.gdal_quote <- function(x) {
  # system2 handles quoting mostly, but for logs we want stable representation
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

.gdal_hash32 <- function(x) {
  # small stable id for context + cache keys
  raw <- charToRaw(paste0(x, collapse = "\n"))
  # base::crc32 exists only via tools::crc32? no. Implement simple adler32-like
  a <- 1L; b <- 0L
  for (r in raw) { a <- (a + as.integer(r)) %% 65521L; b <- (b + a) %% 65521L }
  sprintf("%04x%04x", b, a)
}

.gdal_mkdirp <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

# NDJSON: prefer jsonlite if available; otherwise write a conservative JSON line
.gdal_json_line <- function(x) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
  }
  # Fallback: minimal JSON builder (strings + numeric + logical + NULL + lists)
  esc <- function(s) {
    s <- gsub("\\\\", "\\\\\\\\", s)
    s <- gsub("\"", "\\\\\"", s)
    s <- gsub("\r", "\\\\r", s, fixed = TRUE)
    s <- gsub("\n", "\\\\n", s, fixed = TRUE)
    s <- gsub("\t", "\\\\t", s, fixed = TRUE)
    s
  }
  atom <- function(v) {
    if (is.null(v)) return("null")
    if (length(v) == 0) return("[]")
    if (is.logical(v)) return(ifelse(is.na(v), "null", ifelse(v, "true", "false")))
    if (is.numeric(v)) return(ifelse(is.na(v), "null", format(v, scientific = FALSE, trim = TRUE)))
    if (is.character(v)) return(paste0("\"", esc(v), "\""))
    if (inherits(v, "POSIXt")) return(paste0("\"", esc(format(v, "%Y-%m-%dT%H:%M:%S%z")), "\""))
    paste0("\"", esc(as.character(v)), "\"")
  }
  build <- function(obj) {
    if (is.null(obj)) return("null")
    if (is.atomic(obj) && length(obj) == 1) return(atom(obj))
    if (is.atomic(obj) && length(obj) > 1) return(paste0("[", paste(vapply(obj, atom, ""), collapse = ","), "]"))
    if (is.list(obj)) {
      nms <- names(obj) %||% rep("", length(obj))
      if (is.null(names(obj)) || all(nms == "")) {
        return(paste0("[", paste(vapply(obj, build, ""), collapse = ","), "]"))
      }
      parts <- mapply(function(k, v) paste0("\"", esc(k), "\":", build(v)),
                      k = nms, v = obj, SIMPLIFY = TRUE, USE.NAMES = FALSE)
      return(paste0("{", paste(parts, collapse = ","), "}"))
    }
    atom(as.character(obj))
  }
  build(x)
}

gdal_append_ndjson <- function(file, record) {
  stopifnot(is.character(file), length(file) == 1)
  .gdal_mkdirp(dirname(file))
  line <- .gdal_json_line(record)
  con <- file(file, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(line, con = con, sep = "\n", useBytes = TRUE)
  invisible(file)
}

# ---- 1) context ---------------------------------------------------------------

#' Create a GDAL CLI context from linked binaries
#'
#' @description
#' Resolves executable paths for GDAL utilities (e.g., gdalinfo, gdalwarp, ogr2ogr)
#' under a specific binDir and prepares a per-call environment (PATH + optional vars).
#'
#' @param binDir Character. Directory containing GDAL executables.
#' @param env Named character vector. Additional env vars (e.g., GDAL_DATA, PROJ_LIB).
#' @param log_file Character. NDJSON log file path (default: "gdal-run.ndjson" in tempdir()).
#' @param help_cache_dir Character. Directory for cached help texts (default: "gdal-help-cache" in tempdir()).
#'
#' @return A list (class "gdal_context") with resolved executables and env.
#' @export
gdal_context <- function(binDir,
                         env = character(),
                         log_file = file.path(tempdir(), "gdal-run.ndjson"),
                         help_cache_dir = file.path(tempdir(), "gdal-help-cache")) {
  stopifnot(is.character(binDir), length(binDir) == 1)
  binDir <- .gdal_norm(binDir)
  
  # minimal set; you can extend ad-hoc without breaking anything
  cmds <- c("gdalinfo", "gdalwarp", "gdal_translate", "gdalbuildvrt",
            "gdalsrsinfo", "ogr2ogr", "ogrinfo", "gdaladdo", "gdal_calc")
  ext <- .gdal_bin_ext()
  
  exe <- setNames(vector("character", length(cmds)), cmds)
  for (k in cmds) {
    p <- file.path(binDir, paste0(k, ext))
    exe[[k]] <- if (file.exists(p)) p else NA_character_
  }
  
  # per-call PATH: prepend binDir, do not mutate global PATH
  current_path <- Sys.getenv("PATH", unset = "")
  sep <- if (.gdal_is_windows()) ";" else ":"
  path_new <- paste(c(binDir, current_path), collapse = sep)
  
  env2 <- env
  env2[["PATH"]] <- path_new
  
  if (is.null(names(env2))) names(env2) <- rep("", length(env2))
  if (!("GDAL_DATA" %in% names(env2))) env2[["GDAL_DATA"]] <- NA_character_
  if (!("PROJ_LIB"  %in% names(env2))) env2[["PROJ_LIB"]]  <- NA_character_
  
  # context id: stable over binDir+present exes+env keys
  id <- .gdal_hash32(c(binDir, paste(names(exe), exe, sep = "="), paste(names(env2), env2, sep = "=")))
  
  structure(list(
    id = id,
    binDir = binDir,
    exe = exe,
    env = env2,
    log_file = log_file,
    help_cache_dir = help_cache_dir
  ), class = "gdal_context")
}

#' Create a GDAL context from linkGDAL()
#'
#' @description
#' Convenience helper that derives a GDAL CLI context from the result of
#' [linkGDAL()], ensuring that the linked GDAL binaries are used.
#'
#' @param gdal_link Result of [linkGDAL()].
#' @param env Named character vector of environment variables.
#' @param log_file NDJSON log file path.
#' @param help_cache_dir Directory for cached GDAL help output.
#'
#' @return An object of class `gdal_context`.
#' @export
gdal_context_from_link <- function(gdal_link,
                                   env = character(),
                                   log_file = file.path(tempdir(), "gdal-run.ndjson"),
                                   help_cache_dir = file.path(tempdir(), "gdal-help-cache")) {
  stopifnot(is.list(gdal_link))
  binDir <- gdal_link$path %||% gdal_link$binDir %||% gdal_link$gdalPath
  if (is.null(binDir) || !nzchar(binDir)) stop("Cannot derive binDir from linkGDAL() result.")
  gdal_context(binDir = binDir, env = env, log_file = log_file, help_cache_dir = help_cache_dir)
}

# ---- 2) runner + logging ------------------------------------------------------

#' Run a GDAL utility deterministically
#'
#' @param gdal A gdal_context.
#' @param cmd Character. Utility name (e.g., "gdalwarp") or a full path.
#' @param args Character vector of CLI args (tokens).
#' @param wd Working directory (optional).
#' @param timeout Timeout seconds (best-effort; no hard kill in base R).
#' @param echo Logical. If TRUE, print the resolved command line.
#' @param capture Logical. If TRUE, capture stdout/stderr.
#' @param log Logical. If TRUE, append an NDJSON record to gdal$log_file.
#'
#' @return A list with status/stdout/stderr/call/meta.
#' @export
run_gdal <- function(gdal,
                     cmd,
                     args = character(),
                     wd = NULL,
                     timeout = NULL,
                     echo = FALSE,
                     capture = TRUE,
                     log = TRUE) {
  stopifnot(inherits(gdal, "gdal_context"))
  stopifnot(is.character(cmd), length(cmd) == 1)
  stopifnot(is.character(args))
  
  # resolve executable
  exe_resolved <- cmd
  if (!file.exists(exe_resolved)) {
    if (!is.null(gdal$exe[[cmd]]) && !is.na(gdal$exe[[cmd]])) {
      exe_resolved <- gdal$exe[[cmd]]
    } else {
      # try binDir + cmd(+ext)
      ext <- .gdal_bin_ext()
      cand <- file.path(gdal$binDir, paste0(cmd, ext))
      if (file.exists(cand)) exe_resolved <- cand
    }
  }
  if (!file.exists(exe_resolved)) {
    stop("GDAL executable not found for cmd='", cmd, "' (binDir=", gdal$binDir, ")")
  }
  
  start_time <- Sys.time()
  owd <- getwd()
  if (!is.null(wd)) setwd(wd)
  on.exit(if (!is.null(wd)) setwd(owd), add = TRUE)
  
  if (echo) {
    message("GDAL CMD: ", shQuote(exe_resolved), " ", paste(shQuote(args), collapse = " "))
  }
  
  stdout <- character()
  stderr <- character()
  status <- NA_integer_
  
  if (capture) {
    # capture stdout; stderr via tempfile
    errf <- tempfile("gdal-stderr-")
    on.exit(unlink(errf), add = TRUE)
    stdout <- tryCatch(
      system2(exe_resolved, args = args, stdout = TRUE, stderr = errf, env = gdal$env),
      error = function(e) { structure(character(), status = 1L) }
    )
    status <- attr(stdout, "status") %||% 0L
    if (file.exists(errf)) {
      stderr <- readLines(errf, warn = FALSE, encoding = "UTF-8")
    }
  } else {
    status <- suppressWarnings(system2(exe_resolved, args = args, stdout = "", stderr = "", env = gdal$env))
    stdout <- character()
    stderr <- character()
  }
  
  end_time <- Sys.time()
  
  rec <- list(
    ts_start = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
    ts_end   = format(end_time,   "%Y-%m-%dT%H:%M:%S%z"),
    duration_s = as.numeric(difftime(end_time, start_time, units = "secs")),
    host = list(
      sysname = Sys.info()[["sysname"]],
      release = Sys.info()[["release"]],
      machine = Sys.info()[["machine"]]
    ),
    wd = .gdal_norm(getwd()),
    context_id = gdal$id,
    exe_resolved = .gdal_norm(exe_resolved),
    args = .gdal_quote(args),
    env = list(
      PATH      = .gdal_env_get(gdal$env, "PATH", ""),
      GDAL_DATA = .gdal_env_get(gdal$env, "GDAL_DATA", NA_character_),
      PROJ_LIB  = .gdal_env_get(gdal$env, "PROJ_LIB",  NA_character_)
    ),
    exit_code = as.integer(status),
    stdout = if (length(stdout)) paste(stdout, collapse = "\n") else "",
    stderr = if (length(stderr)) paste(stderr, collapse = "\n") else ""
  )
  
  if (isTRUE(log)) {
    gdal_append_ndjson(gdal$log_file, rec)
  }
  
  list(
    status = as.integer(status),
    stdout = stdout,
    stderr = stderr,
    call = list(exe = exe_resolved, args = args),
    meta = rec
  )
}

# ---- 3) fingerprint + help cache + skeleton ----------------------------------

#' Collect a GDAL capability fingerprint for a context
#'
#' @param gdal A gdal_context.
#' @param log Logical. If TRUE, append fingerprint record to NDJSON (type="fingerprint").
#'
#' @return A list with version strings and formats.
#' @export
gdal_fingerprint <- function(gdal, log = TRUE) {
  stopifnot(inherits(gdal, "gdal_context"))
  
  get_out <- function(cmd, args) {
    r <- tryCatch(run_gdal(gdal, cmd = cmd, args = args, capture = TRUE, log = FALSE),
                  error = function(e) list(status = 1L, stdout = character(), stderr = e$message))
    list(status = r$status, out = paste(r$stdout, collapse = "\n"), err = paste(r$stderr, collapse = "\n"))
  }
  
  v_gdalinfo <- get_out("gdalinfo", c("--version"))
  fm_r <- get_out("gdalinfo", c("--formats"))
  fm_v <- get_out("ogrinfo",  c("--formats"))
  axis <- get_out("gdalsrsinfo", c("EPSG:4326"))
  
  fp <- list(
    type = "fingerprint",
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    context_id = gdal$id,
    binDir = gdal$binDir,
    gdalinfo_version = v_gdalinfo$out,
    gdal_formats = fm_r$out,
    ogr_formats  = fm_v$out,
    axis_probe_epsg4326 = axis$out
  )
  
  if (isTRUE(log)) {
    gdal_append_ndjson(gdal$log_file, fp)
  }
  fp
}

#' Get and cache "--help" output for a GDAL command
#'
#' @param gdal A gdal_context.
#' @param cmd Utility name, e.g. "gdalwarp".
#'
#' @return Character scalar (full help text).
#' @export
gdal_help <- function(gdal, cmd) {
  stopifnot(inherits(gdal, "gdal_context"))
  stopifnot(is.character(cmd), length(cmd) == 1)
  
  .gdal_mkdirp(gdal$help_cache_dir)
  key <- .gdal_hash32(c(gdal$id, cmd))
  f <- file.path(gdal$help_cache_dir, paste0(cmd, "_", key, ".help.txt"))
  
  if (file.exists(f)) {
    return(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  
  r <- run_gdal(gdal, cmd = cmd, args = c("--help"), capture = TRUE, log = FALSE)
  txt <- paste(c(r$stdout, r$stderr), collapse = "\n")
  writeLines(txt, con = f, useBytes = TRUE)
  txt
}

#' Build a minimal CLI skeleton from "--help"
#'
#' @description
#' Extracts the first "Usage:" line (best-effort) and a flat list of flags.
#' Does NOT attempt semantic validation.
#'
#' @param gdal A gdal_context.
#' @param cmd Utility name, e.g. "gdalwarp".
#'
#' @return List with exe, usage, flags, args_template.
#' @export
gdal_skeleton <- function(gdal, cmd) {
  stopifnot(inherits(gdal, "gdal_context"))
  h <- gdal_help(gdal, cmd)
  lines <- unlist(strsplit(h, "\n", fixed = TRUE), use.names = FALSE)
  
  # usage line
  uidx <- grep("^\\s*(Usage:|usage:)", lines)
  usage <- if (length(uidx)) trimws(lines[uidx[1]]) else NA_character_
  
  # flat flags: tokens like "-t_srs", "--config", "-of", "-tr"
  # (keep both single- and double-dash)
  flags <- unique(unlist(regmatches(lines, gregexpr("(?<!\\S)(--?[A-Za-z0-9][A-Za-z0-9._-]*)(?!\\S)",
                                                    lines, perl = TRUE)), use.names = FALSE))
  flags <- flags[nzchar(flags)]
  
  exe <- gdal$exe[[cmd]]
  if (is.null(exe) || is.na(exe)) exe <- cmd
  
  # minimal template: input/output placeholders commonly used
  args_template <- c("<ARGS...>", "<INPUT...>", "<OUTPUT...>")
  
  list(
    cmd = cmd,
    exe = exe,
    usage = usage,
    flags = flags,
    args_template = args_template
  )
}

#' Assemble args from a skeleton and a named list
#'
#' @param skel Result of gdal_skeleton().
#' @param opts Named list. Names map to flags without leading dash, e.g. list(t_srs="EPSG:25832").
#' @param positional Character. Additional positional args (e.g. input/output files).
#'
#' @return Character vector of CLI args.
#' @export
gdal_build_args <- function(skel, opts = list(), positional = character()) {
  stopifnot(is.list(skel), !is.null(skel$cmd))
  stopifnot(is.list(opts))
  stopifnot(is.character(positional))
  
  # map names -> "-name" (single dash) by default; allow caller to pass full flags as names
  mkflag <- function(nm) {
    if (startsWith(nm, "-")) nm else paste0("-", nm)
  }
  
  out <- character()
  if (length(opts)) {
    for (nm in names(opts)) {
      flag <- mkflag(nm)
      val <- opts[[nm]]
      if (isTRUE(val) && (is.logical(val) || identical(val, ""))) {
        out <- c(out, flag)
      } else if (isFALSE(val) || is.null(val) || (length(val) == 0)) {
        # skip
      } else if (length(val) == 1) {
        out <- c(out, flag, as.character(val))
      } else {
        out <- c(out, flag, as.character(val))
      }
    }
  }
  c(out, positional)
}
