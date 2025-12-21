# ============================================================
# OTB introspection helpers (link2GI)  -- FIXED VERSION
# ============================================================
# Fixes vs your draft:
# 1) API consistency: otb_args_spec(algo, gili=...) NOT otb_args_spec(params_df)
#    (your tutorial called otb_args_spec(algo, otb) and hit "unused argument").
# 2) Robustness: tolerate OTB "-help" returning exit status 1 (OTB does this).
# 3) Parser: stricter block extraction + safer regex + avoids returning empty data.frame()
#    without columns (always returns a typed df with expected columns).
# 4) Command list building: ensure first element is algo and list is named properly.
# 5) Optional defaults: keep "progress=false" convention, but DO NOT invent defaults for others.
#
# Placement:
#   R/otb_introspection.R
#
# Requires:
#   - link2GI::linkOTB()
#   - your Linux runner: .otb_run_launcher(gili, args, ...) -> character vector (may have attr("status"))
# ============================================================

#' @importFrom stats setNames
NULL

# ------------------------ internal: normalize empty df ------------------------

#' @keywords internal
.otb_empty_params_df <- function() {
  data.frame(
    key = character(0),
    missing = logical(0),
    mandatory = logical(0),
    type = character(0),
    has_pixel = logical(0),         # NEW
    default = character(0),
    pixel_default = character(0),   # NEW
    desc = character(0),
    stringsAsFactors = FALSE
  )
}
# ------------------------ internal: parse Parameters block ------------------------

#' Parse the "Parameters:" block from OTB -help output
#'
#' Internal helper to extract parameter definitions from the standard
#' "Parameters:" section of OTB CLI help output.
#'
#' The parser handles the following OTB conventions:
#' \itemize{
#'   \item mandatory parameters marked by the prefix \code{MISSING}
#'   \item mandatory parameters marked by \code{(mandatory)} in the description
#'   \item output parameters with pixel type specification \code{[pixel]}
#'   \item separation of parameter defaults from pixel-type defaults
#' }
#'
#' @param txt Character vector returned by
#'   \code{.otb_run_launcher(gili, c(algo, "-help"), ...)}.
#'
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{key}{Parameter key without leading \code{-}.}
#'   \item{missing}{Logical. TRUE if prefixed with \code{MISSING}.}
#'   \item{mandatory}{Logical. TRUE if marked \code{(mandatory)}.}
#'   \item{type}{Type string extracted from angle brackets.}
#'   \item{has_pixel}{Logical. TRUE if parameter has a \code{[pixel]} qualifier.}
#'   \item{default}{Parameter default value (if any).}
#'   \item{pixel_default}{Default pixel type (only for \code{[pixel]} outputs).}
#'   \item{desc}{Full human-readable description line.}
#' }
#'
#' @keywords internal
.otb_parse_parameters_block <- function(txt) {
  
  # empty / invalid input → empty typed data.frame
  if (!is.character(txt) || !length(txt)) {
    return(data.frame(
      key = character(0),
      missing = logical(0),
      mandatory = logical(0),
      type = character(0),
      has_pixel = logical(0),
      default = character(0),
      pixel_default = character(0),
      desc = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # locate "Parameters:" block
  i0 <- grep("^\\s*Parameters\\s*:\\s*$", txt)
  if (!length(i0)) {
    i0 <- grep("^\\s*Parameters\\s*:", txt)
    if (!length(i0)) {
      return(data.frame(
        key = character(0),
        missing = logical(0),
        mandatory = logical(0),
        type = character(0),
        has_pixel = logical(0),
        default = character(0),
        pixel_default = character(0),
        desc = character(0),
        stringsAsFactors = FALSE
      ))
    }
  }
  i0 <- i0[1]
  
  # block end at next section header or EOF
  i1 <- grep("^\\s*(Examples\\s*:|Authors\\s*:|Limitations\\s*:|See also\\s*:)", txt)
  i1 <- i1[i1 > i0]
  i1 <- if (length(i1)) i1[1] - 1 else length(txt)
  
  if (i0 + 1 > i1) {
    return(data.frame(
      key = character(0),
      missing = logical(0),
      mandatory = logical(0),
      type = character(0),
      has_pixel = logical(0),
      default = character(0),
      pixel_default = character(0),
      desc = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  block <- txt[(i0 + 1):i1]
  block <- block[nzchar(trimws(block))]
  if (!length(block)) {
    return(data.frame(
      key = character(0),
      missing = logical(0),
      mandatory = logical(0),
      type = character(0),
      has_pixel = logical(0),
      default = character(0),
      pixel_default = character(0),
      desc = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # expected parameter line pattern:
  # MISSING -out <string> [pixel] description (default value is float)
  pat <- "^\\s*(MISSING\\s+)?(-[A-Za-z0-9_.-]+)\\s+<([^>]+)>\\s*(\\[[^\\]]+\\])?\\s+(.+?)\\s*$"
  
  m  <- regexec(pat, block, perl = TRUE)
  mm <- regmatches(block, m)
  mm <- mm[lengths(mm) > 0]
  if (!length(mm)) {
    return(data.frame(
      key = character(0),
      missing = logical(0),
      mandatory = logical(0),
      type = character(0),
      has_pixel = logical(0),
      default = character(0),
      pixel_default = character(0),
      desc = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  rows <- lapply(mm, function(x) {
    
    missing <- !is.na(x[2]) && nzchar(x[2])
    key     <- sub("^-", "", x[3])
    type    <- trimws(x[4])
    bracket <- if (!is.na(x[5])) trimws(x[5]) else ""
    rest    <- trimws(x[6])
    
    has_pixel <- identical(bracket, "[pixel]")
    mandatory <- grepl("\\(mandatory\\)", rest)
    
    # parse default value
    def <- NA_character_
    if (grepl("default value is", rest, fixed = TRUE)) {
      def <- sub(".*default value is\\s*", "", rest)
      def <- sub("\\).*", "", def)
      def <- trimws(def)
      if (!nzchar(def)) def <- NA_character_
    }
    
    # separate pixel-type default from parameter default
    pixel_def <- NA_character_
    if (isTRUE(has_pixel) && !is.na(def)) {
      pixel_def <- def
      def <- NA_character_
    }
    
    data.frame(
      key = key,
      missing = missing,
      mandatory = mandatory,
      type = type,
      has_pixel = has_pixel,
      default = def,
      pixel_default = pixel_def,
      desc = rest,
      stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, rows)
  rownames(df) <- NULL
  df
}


# ------------------------ exported: otb_capabilities ------------------------
#' Retrieve OTB application capabilities (help text + parsed parameters)
#'
#' @param algo Character. OTB application name (e.g. "BandMathX").
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @param include_param_help Logical. If `TRUE`, calls `-help <param>` for each key.
#'
#' @return List with elements:
#' \describe{
#'   \item{text}{Character vector of the `-help` output.}
#'   \item{params}{Parsed parameter table (data.frame / tibble depending on your parser).}
#'   \item{param_help}{Named list of per-parameter help texts (or `NULL`).}
#' }
#' @export
otb_capabilities <- function(algo, gili = NULL, include_param_help = FALSE) {
  if (!is.character(algo) || length(algo) != 1L || !nzchar(algo)) {
    stop("`algo` must be a non-empty character scalar.")
  }
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) {
    stop("No valid OTB installation found (gili$exist is FALSE).")
  }
  
  sys <- Sys.info()[["sysname"]]
  
  .resolve_otb_cli <- function(bin, algo) {
    # gili$pathOTB should be binDir; keep your shortPathName usage
    bin <- utils::shortPathName(bin)
    
    # Accept modern OTB 9.x standalone (.ps1) and legacy (.bat/.exe / no-ext)
    cands <- c(
      file.path(bin, paste0("otbcli_", algo, ".ps1")),
      file.path(bin, paste0("otbcli_", algo, ".bat")),
      file.path(bin, paste0("otbcli_", algo, ".exe")),
      file.path(bin, paste0("otbcli_", algo)) # very old / edge cases
    )
    hit <- cands[file.exists(cands)][1]
    if (is.na(hit) || !nzchar(hit)) return(NA_character_)
    hit
  }
  
  .run_otb_help_windows <- function(cli, args) {
    # args is c("-help") or c("-help", k)
    if (grepl("\\.ps1$", cli, ignore.case = TRUE)) {
      # Run PS1 via PowerShell, avoiding policy issues
      system2(
        "powershell.exe",
        c(
          "-NoProfile",
          "-ExecutionPolicy", "Bypass",
          "-Command",
          paste0("& ", shQuote(cli), " ", paste(vapply(args, shQuote, character(1)), collapse = " "))
        ),
        stdout = TRUE,
        stderr = TRUE
      )
    } else {
      # .bat/.exe/no-ext should work via system2 directly
      system2(cli, args, stdout = TRUE, stderr = TRUE)
    }
  }
  
  if (sys == "Windows") {
    bin <- gili$pathOTB
    cli <- .resolve_otb_cli(bin, algo)
    if (is.na(cli)) {
      stop(
        "Missing OTB CLI wrapper for '", algo, "' in: ", utils::shortPathName(bin), "\n",
        "Expected one of: otbcli_", algo, ".ps1 / .bat / .exe",
        call. = FALSE
      )
    }
    txt <- .run_otb_help_windows(cli, c("-help"))
  } else {
    txt <- .otb_help_text(gili, algo)
  }
  
  params <- .otb_parse_parameters_block(txt)
  
  help_list <- NULL
  if (isTRUE(include_param_help) && nrow(params)) {
    help_list <- setNames(vector("list", nrow(params)), params$key)
    
    if (sys == "Windows") {
      cli <- .resolve_otb_cli(gili$pathOTB, algo)
      if (is.na(cli)) {
        stop(
          "Missing OTB CLI wrapper for '", algo, "' in: ", utils::shortPathName(gili$pathOTB),
          call. = FALSE
        )
      }
    }
    
    for (k in params$key) {
      if (sys == "Windows") {
        out <- .run_otb_help_windows(cli, c("-help", k))
      } else {
        out <- .otb_help_text(gili, algo, param = k)
      }
      out <- unique(out)
      out <- out[nzchar(trimws(out))]
      help_list[[k]] <- out
    }
  }
  
  list(text = txt, params = params, param_help = help_list)
}



# ------------------------ exported: otb_args_spec ------------------------

#' Return normalized OTB parameter spec for an application
#'
#' @param algo Character. OTB application name.
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @return data.frame with columns: key, type, mandatory, has_pixel, pixel_default, has_default, default, class, desc
#' @export
otb_args_spec <- function(algo, gili = NULL) {
  caps <- otb_capabilities(algo, gili, include_param_help = FALSE)
  df <- caps$params
  
  if (!is.data.frame(df) || !nrow(df)) {
    return(data.frame(
      key = character(0),
      type = character(0),
      mandatory = logical(0),
      has_pixel = logical(0),
      pixel_default = character(0),
      has_default = logical(0),
      default = character(0),
      class = character(0),
      desc = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  mandatory <- as.logical(df$mandatory) | as.logical(df$missing)
  
  default <- df$default
  default[default == "" | default == "<NA>"] <- NA_character_
  
  if ("progress" %in% df$key) {
    i <- which(df$key == "progress")[1]
    if (is.na(default[i])) default[i] <- "false"
  }
  
  has_default <- !is.na(default)
  
  class <- ifelse(mandatory, "mandatory",
                  ifelse(has_default, "optional_with_default", "optional"))
  
  out <- data.frame(
    key = df$key,
    type = df$type,
    mandatory = mandatory,
    has_pixel = df$has_pixel,
    pixel_default = df$pixel_default,
    has_default = has_default,
    default = default,
    class = class,
    desc = df$desc,
    stringsAsFactors = FALSE
  )
  
  rownames(out) <- NULL
  out
}


  
 


#' OTB-required parameter keys for an application
#'
#' Returns the parameter keys that are strictly required according to the
#' OTB CLI help output. A parameter is considered required if it is marked
#' as \code{MISSING} in the "Parameters:" block or contains \code{(mandatory)}
#' in its description.
#'
#' This function does \strong{not} apply any additional package policy such as
#' enforcing file-based outputs. For that, use \code{\link{otb_required_with_output}}.
#'
#' @param algo Character scalar. OTB application name (e.g. \code{"BandMathX"}).
#' @param gili Optional list returned by \code{\link[link2GI:linkOTB]{link2GI::linkOTB}}.
#'   If \code{NULL}, \code{link2GI::linkOTB()} is called internally.
#'
#' @return Character vector of required parameter keys (without leading \code{-}).
#'   Returns an empty character vector if no parameters could be parsed.
#'
#' @export
#' @examples
#' \dontrun{
#' otb <- link2GI::linkOTB()
#' if (isTRUE(otb$exist)) {
#'   otb_required("DimensionalityReduction", otb)
#' }
#' }
otb_required <- function(algo, gili = NULL) {
  spec <- otb_args_spec(algo, gili)
  if (!is.data.frame(spec) || !nrow(spec)) return(character(0))
  spec$key[which(spec$mandatory)]
}



#' Required parameter keys under link2GI output policy
#'
#' Extends \code{\link{otb_required}} by optionally enforcing the presence of at least
#' one file-based output parameter key in the returned set. This reflects the package
#' policy "always write outputs to disk" even if OTB marks output parameters as optional.
#'
#' The function does \emph{not} guess output pixel types or create temporary file names.
#' It only returns keys.
#'
#' @param algo Character scalar. OTB application name (e.g. \code{"BandMathX"}).
#' @param gili Optional list returned by \code{\link[link2GI:linkOTB]{link2GI::linkOTB}}.
#'   If \code{NULL}, \code{link2GI::linkOTB()} is called internally.
#' @param enforce_output Logical. If \code{TRUE} (default), include the first matching
#'   file output key found in the application's parameter set.
#'
#' @return Character vector of required keys (without leading \code{-}).
#'
#' @export
#' @examples
#' \dontrun{
#' otb <- link2GI::linkOTB()
#' if (isTRUE(otb$exist)) {
#'   otb_required_with_output("DimensionalityReduction", otb)
#' }
#' }
otb_required_with_output <- function(algo, gili = NULL, enforce_output = TRUE) {
  spec <- otb_args_spec(algo, gili)
  if (!is.data.frame(spec) || !nrow(spec)) return(character(0))
  
  req <- spec$key[which(spec$mandatory)]
  
  if (isTRUE(enforce_output)) {
    out_candidates <- c("out", "io.out", "out.xml", "outxml", "mode.vector.out")
    out_present <- intersect(out_candidates, spec$key)
    if (length(out_present)) req <- union(req, out_present[1])
  }
  
  req
}





# ------------------------ exported: otb_optional ------------------------

#' Get optional parameters for an OTB application (keys + defaults optional)
#'
#' @param algo Character. OTB application name.
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @param with_defaults Logical. If TRUE, fill parsed defaults where available; else NA.
#' @return Named list.
#' @export
otb_optional <- function(algo, gili = NULL, with_defaults = TRUE) {
  spec <- otb_args_spec(algo, gili)
  
  opt <- spec[!spec$mandatory, , drop = FALSE]
  out <- setNames(vector("list", nrow(opt)), opt$key)
  
  if (!nrow(opt)) return(out)
  
  for (i in seq_len(nrow(opt))) {
    k <- opt$key[i]
    d <- opt$default[i]
    out[[k]] <- if (isTRUE(with_defaults) && !is.na(d)) d else NA_character_
  }
  out
}

# ------------------------ exported: otb_build_cmd ------------------------

#' Build a runOTB-compatible command template from OTB application metadata
#'
#' Constructs a command list in the format expected by \code{\link{runOTB}}:
#' the first element is the application name, followed by named parameters
#' (without leading \code{-}).
#'
#' Mandatory parameters reported by OTB are always included and initialized
#' as \code{NA_character_} to force explicit user input.
#'
#' Optional parameters can be included either with parsed defaults (when available)
#' or as \code{NA_character_}.
#'
#' By default, this function also enforces presence of at least one file-based
#' output parameter in the template (see \code{require_output}), reflecting the
#' package policy to always write outputs to disk.
#'
#' @param algo Character scalar. Name of the OTB application.
#' @param gili Optional list returned by \code{\link{linkOTB}}. If \code{NULL},
#'   \code{linkOTB()} is called internally.
#' @param include_optional Character. One of \code{"none"}, \code{"defaults"},
#'   \code{"all_na"}:
#'   \describe{
#'     \item{\code{"none"}}{Do not include optional parameters.}
#'     \item{\code{"defaults"}}{Include optional parameters and prefill parsed defaults where available.}
#'     \item{\code{"all_na"}}{Include optional parameters but set all to \code{NA_character_}.}
#'   }
#' @param require_output Logical. If \code{TRUE} (default), enforce that at least one
#'   file-based output parameter key is present in the returned command template
#'   (e.g. \code{out}, \code{io.out}, \code{mode.vector.out}). This reflects the
#'   \pkg{link2GI} workflow policy to always write outputs to disk, even if OTB
#'   marks some output parameters as optional.
#'
#' @return A list suitable for \code{\link{runOTB}}.
#'
#' @seealso \code{\link{otb_capabilities}}, \code{\link{otb_args_spec}},
#'   \code{\link{otb_required}}, \code{\link{otb_optional}}, \code{\link{otb_set_out}}
#'
#' @examples
#' \dontrun{
#' otb <- link2GI::linkOTB()
#' cmd <- otb_build_cmd("BandMathX", otb, include_optional = "defaults")
#' cmd[["il"]]  <- c("a.tif", "b.tif")
#' cmd <- otb_set_out(cmd, otb, path = "out.tif")
#' cmd[["exp"]] <- "im1b1 + im2b1"
#' runOTB(cmd, otb, quiet = FALSE)
#' }
#' @export
otb_build_cmd <- function(algo, gili = NULL,
                          include_optional = c("none", "defaults", "all_na"),
                          require_output = TRUE) {
  include_optional <- match.arg(include_optional)
  
  spec <- otb_args_spec(algo, gili)
  
  cmd <- list(algo)
  
  # 1) OTB-mandatories immer drin, immer NA
  req <- spec$key[spec$mandatory]
  for (k in req) cmd[[k]] <- NA_character_
  
  # 2) Workflow-Policy: erzwinge mindestens einen Dateiausgabe-Key
  if (isTRUE(require_output)) {
    out_candidates <- c("out", "io.out", "out.xml", "outxml", "mode.vector.out")
    out_present <- intersect(out_candidates, spec$key)
    if (length(out_present)) {
      k <- out_present[1]
      if (is.null(cmd[[k]])) cmd[[k]] <- NA_character_
    }
  }
  
  # 3) Optional parameter inclusion
  if (include_optional == "none") return(cmd)
  
  opt <- spec[!spec$mandatory, , drop = FALSE]
  if (!nrow(opt)) return(cmd)
  
  for (i in seq_len(nrow(opt))) {
    k <- opt$key[i]
    if (!is.null(cmd[[k]])) next  # don't overwrite required/policy keys
    
    if (include_optional == "defaults") {
      # only true defaults (NOT pixel defaults) come through spec$default
      d <- opt$default[i]
      cmd[[k]] <- if (!is.na(d)) d else NA_character_
    } else {
      cmd[[k]] <- NA_character_
    }
  }
  
  cmd
}

# ------------------------ optional exported: otb_show ------------------------

#' Print a quick console view of an OTB application's parameters
#'
#' @param algo Character. OTB application name.
#' @param gili Optional list returned by [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @return Invisibly list(caps=..., spec=...)
#' @export
otb_show <- function(algo, gili = NULL) {
  caps <- otb_capabilities(algo, gili, include_param_help = FALSE)
  spec <- otb_args_spec(algo, gili)
  
  cat("\n[OTB] ", algo, "\n", sep = "")
  cat("Help lines: ", length(caps$text), "\n", sep = "")
  cat("Parameters parsed: ", nrow(spec), "\n\n", sep = "")
  
  if (nrow(spec)) {
    print(spec, row.names = FALSE)
  } else {
    cat("(No parameters parsed from help output.)\n")
  }
  
  invisible(list(caps = caps, spec = spec))
}


#' Set an OTB output parameter safely (file-based output)
#'
#' Assign a file-based output parameter (e.g. \code{out}, \code{io.out},
#' \code{mode.vector.out}) in a runOTB command list created by
#' \code{\link{otb_build_cmd}}.
#'
#' This helper enforces the package policy of explicit on-disk outputs:
#' it never creates temporary paths and never fills unrelated parameters
#' such as \code{outinv}, \code{outmatrix}, or \code{bv} with pixel types.
#'
#' Some OTB outputs are typed as \code{[pixel]} in the help output. For such keys,
#' OTB accepts either:
#' \itemize{
#'   \item a single path (OTB uses its internal default), or
#'   \item \code{c(path, pixel_type)} (explicit pixel type).
#' }
#'
#' This function will only append \code{pixel_type} for keys that are detected
#' as \code{[pixel]} outputs.
#'
#' @param cmd List. A command template created by \code{\link{otb_build_cmd}}.
#' @param gili Optional list returned by \code{\link{linkOTB}}. If \code{NULL},
#'   \code{linkOTB()} is called.
#' @param key Character scalar. Output parameter key to set (default: \code{"out"}).
#' @param path Character scalar. Output file path.
#' @param pixel_type Optional character scalar. Pixel type (e.g. \code{"float"},
#'   \code{"uint16"}). Only applied if \code{key} is a \code{[pixel]} output.
#'   If \code{NULL}, the function uses the parsed default if available, otherwise
#'   falls back to \code{"float"}.
#' @param overwrite Logical. If \code{FALSE}, error if \code{path} already exists.
#' @param create_dir Logical. If \code{TRUE}, create the parent directory of
#'   \code{path} if missing.
#'
#' @return The modified \code{cmd} list.
#' @export
#' @examples
#' \dontrun{
#' otb <- link2GI::linkOTB()
#' cmd <- link2GI::otb_build_cmd("DimensionalityReduction", otb, include_optional="defaults")
#' cmd[["in"]] <- "in.tif"
#' cmd <- link2GI::otb_set_out(cmd, otb, key="out", path="pca.tif")
#' link2GI::runOTB(cmd, otb, quiet=FALSE)
#' }
otb_set_out <- function(cmd, gili = NULL, key = "out", path,
                        pixel_type = NULL,
                        overwrite = TRUE,
                        create_dir = TRUE) {
  if (!is.list(cmd) || !length(cmd)) stop("`cmd` must be a non-empty list.")
  if (!is.character(key) || length(key) != 1 || !nzchar(key)) stop("`key` must be a non-empty character scalar.")
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) stop("`path` must be a non-empty character scalar.")
  
  algo <- cmd[[1]]
  if (!is.character(algo) || length(algo) != 1 || !nzchar(algo)) {
    stop("`cmd[[1]]` must be the OTB algorithm name (character scalar).")
  }
  
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) stop("No valid OTB installation found (gili$exist is FALSE).")
  
  if (!(key %in% names(cmd))) {
    stop("[otb_set_out] Output key not present in command: ", key)
  }
  
  out_path <- normalizePath(path, mustWork = FALSE)
  
  dirp <- dirname(out_path)
  if (isTRUE(create_dir) && !dir.exists(dirp)) {
    dir.create(dirp, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(dirp)) stop("Output directory does not exist: ", dirp)
  
  if (!isTRUE(overwrite) && file.exists(out_path)) {
    stop("Output file exists and overwrite=FALSE: ", out_path)
  }
  
  # Detect whether this key is an image output that supports [pixel]
  # (uses your exported: otb_args_spec(algo, gili))
  spec <- link2GI::otb_args_spec(algo, gili)
  
  i <- which(spec$key == key)
  if (!length(i)) stop("[otb_set_out] Output key not present in spec: ", key)
  i <- i[1]
  
  # Your otb_args_spec() must provide these two columns:
  # - has_pixel (logical)
  # - pixel_default (character/NA)
  has_pixel <- isTRUE(spec$has_pixel[i])
  
  if (has_pixel) {
    pt <- pixel_type
    if (is.null(pt) || !nzchar(pt)) {
      pt <- spec$pixel_default[i]
      if (is.na(pt) || !nzchar(pt)) pt <- "float"
    }
    cmd[[key]] <- c(out_path, pt)
  } else {
    cmd[[key]] <- out_path
  }
  
  cmd
}



