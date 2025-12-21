# R/otb_parse.R
# Public API: otb_capabilities(), otb_args_spec(), otb_required*(), otb_optional(),
#             otb_build_cmd(), otb_set_out(), otb_show()

# ------------------------ internal: empty df ---------------------------------

#' @noRd
.otb_empty_params_df <- function() {
  data.frame(
    key = character(0),
    missing = logical(0),
    mandatory = logical(0),
    type = character(0),
    has_pixel = logical(0),
    default = character(0),
    pixel_default = character(0),
    desc = character(0),
    stringsAsFactors = FALSE
  )
}

# ------------------------ internal: parse Parameters block --------------------

#' @noRd
.otb_parse_parameters_block <- function(txt) {
  
  if (!is.character(txt) || !length(txt)) return(.otb_empty_params_df())
  
  i0 <- grep("^\\s*Parameters\\s*:\\s*$", txt)
  if (!length(i0)) i0 <- grep("^\\s*Parameters\\s*:", txt)
  if (!length(i0)) return(.otb_empty_params_df())
  i0 <- i0[1]
  
  i1 <- grep("^\\s*(Examples\\s*:|Authors\\s*:|Limitations\\s*:|See also\\s*:)", txt)
  i1 <- i1[i1 > i0]
  i1 <- if (length(i1)) i1[1] - 1 else length(txt)
  
  if (i0 + 1 > i1) return(.otb_empty_params_df())
  
  block <- txt[(i0 + 1):i1]
  block <- block[nzchar(trimws(block))]
  if (!length(block)) return(.otb_empty_params_df())
  
  pat <- "^\\s*(MISSING\\s+)?(-[A-Za-z0-9_.-]+)\\s+<([^>]+)>\\s*(\\[[^\\]]+\\])?\\s+(.+?)\\s*$"
  
  m  <- regexec(pat, block, perl = TRUE)
  mm <- regmatches(block, m)
  mm <- mm[lengths(mm) > 0]
  if (!length(mm)) return(.otb_empty_params_df())
  
  rows <- lapply(mm, function(x) {
    
    missing <- !is.na(x[2]) && nzchar(x[2])
    key     <- sub("^-", "", x[3])
    type    <- trimws(x[4])
    bracket <- if (!is.na(x[5])) trimws(x[5]) else ""
    rest    <- trimws(x[6])
    
    has_pixel <- identical(bracket, "[pixel]")
    mandatory <- grepl("\\(mandatory\\)", rest)
    
    def <- NA_character_
    if (grepl("default value is", rest, fixed = TRUE)) {
      def <- sub(".*default value is\\s*", "", rest)
      def <- sub("\\).*", "", def)
      def <- trimws(def)
      if (!nzchar(def)) def <- NA_character_
    }
    
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

# ------------------------ exported: capabilities ------------------------------

#' Retrieve OTB application capabilities (help text + parsed parameters)
#'
#' Calls the OTB application with `-help` and parses the `Parameters:` block
#' into a parameter table.
#'
#' @param algo Character scalar. OTB application name.
#' @param gili Optional list from [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @param include_param_help Logical. If `TRUE`, additionally queries
#'   `-help <param>` for each parameter and returns these blocks as a named list.
#'
#' @return A list with components:
#' - `text`: character vector of help lines.
#' - `params`: data.frame parsed from the `Parameters:` block.
#' - `param_help`: `NULL` or named list of character vectors (per-parameter help).
#'
#' @rdname otb_api
#' @export
otb_capabilities <- function(algo, gili = NULL, include_param_help = FALSE)  {
  
  if (!is.character(algo) || length(algo) != 1L || !nzchar(algo)) {
    stop("`algo` must be a non-empty character scalar.")
  }
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) {
    stop("No valid OTB installation found (gili$exist is FALSE).")
  }
  
  sys <- Sys.info()[["sysname"]]
  
  if (identical(sys, "Windows")) {
    stop("Windows implementation not loaded in Linux-only phase.")
  }
  
  txt <- .otb_help_text(gili, algo)
  params <- .otb_parse_parameters_block(txt)
  
  help_list <- NULL
  if (isTRUE(include_param_help) && nrow(params)) {
    help_list <- stats::setNames(vector("list", nrow(params)), params$key)
    for (k in params$key) {
      out <- .otb_help_text(gili, algo, param = k)
      out <- unique(out)
      out <- out[nzchar(trimws(out))]
      help_list[[k]] <- out
    }
  }
  
  list(text = txt, params = params, param_help = help_list)
}

# ------------------------ exported: args spec --------------------------------

#' Return normalized OTB parameter spec for an application
#'
#' Normalizes the parsed parameter table (from [otb_capabilities()]) into a
#' stable schema used for command building.
#'
#' @param algo Character scalar. OTB application name.
#' @param gili Optional list from [linkOTB()]. If `NULL`, [linkOTB()] is called.
#'
#' @return A data.frame with (at least) the columns:
#' `key`, `type`, `mandatory`, `has_pixel`, `pixel_default`, `has_default`,
#' `default`, `class`, `desc`.
#'
#' @rdname otb_api
#' @export
otb_args_spec <- function(algo, gili = NULL){
  
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

# ------------------------ exported: required/optional/build/set_out/show ------

#' List mandatory parameter keys
#'
#' Convenience accessor based on [otb_args_spec()].
#'
#' @param algo Character scalar. OTB application name.
#' @param gili Optional list from [linkOTB()].
#'
#' @return Character vector of mandatory parameter keys.
#'
#' @rdname otb_api
#' @export
otb_required <- function(algo, gili = NULL)  {
  spec <- otb_args_spec(algo, gili)
  if (!is.data.frame(spec) || !nrow(spec)) return(character(0))
  spec$key[which(spec$mandatory)]
}

#' List mandatory parameter keys, optionally enforcing an output parameter
#'
#' Like [otb_required()], but can ensure that a best-effort output key is included.
#'
#' @param algo Character scalar. OTB application name.
#' @param gili Optional list from [linkOTB()].
#' @param enforce_output Logical. If `TRUE`, attempts to add an output key from a
#'   small set of common output parameter names (e.g. `"out"`, `"io.out"`).
#'
#' @return Character vector of required parameter keys (including output if enforced).
#'
#' @rdname otb_api
#' @export
otb_required_with_output <- function(algo, gili = NULL, enforce_output = TRUE){
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

#' List optional parameter keys and defaults
#'
#' Convenience accessor based on [otb_args_spec()]. Returns a named list of
#' optional parameters with `NA_character_` placeholders or default values.
#'
#' @param algo Character scalar. OTB application name.
#' @param gili Optional list from [linkOTB()].
#' @param with_defaults Logical. If `TRUE`, populate optional parameters with
#'   their default where available; otherwise use `NA_character_`.
#'
#' @return Named list of optional parameters.
#'
#' @rdname otb_api
#' @export
otb_optional <- function(algo, gili = NULL, with_defaults = TRUE){
  spec <- otb_args_spec(algo, gili)
  
  opt <- spec[!spec$mandatory, , drop = FALSE]
  out <- stats::setNames(vector("list", nrow(opt)), opt$key)
  
  if (!nrow(opt)) return(out)
  
  for (i in seq_len(nrow(opt))) {
    k <- opt$key[i]
    d <- opt$default[i]
    out[[k]] <- if (isTRUE(with_defaults) && !is.na(d)) d else NA_character_
  }
  out
}


#' Build a command template for an OTB application
#'
#' Creates a command list suitable for [runOTB()], with mandatory parameters
#' always present as `NA_character_` placeholders. Optional parameters can be
#' omitted, filled with defaults, or included as `NA_character_`.
#'
#' @param algo Character scalar. OTB application name.
#' @param gili Optional list from [linkOTB()].
#' @param include_optional One of `"none"`, `"defaults"`, `"all_na"`.
#' @param require_output Logical. If `TRUE`, ensures that a best-effort output key
#'   placeholder exists if the application exposes one of the common output keys.
#'
#' @return A command list with `cmd[[1]] == algo` and named entries for parameters.
#'
#' @rdname otb_api
#' @export
otb_build_cmd <- function(algo, gili = NULL,
                          include_optional = c("none", "defaults", "all_na"),
                          require_output = TRUE)  {
  include_optional <- match.arg(include_optional)
  
  spec <- otb_args_spec(algo, gili)
  cmd  <- list(algo)
  
  # --- mandatory keys (always present as NA placeholders) ---------------------
  req <- spec$key[isTRUE(spec$mandatory)]
  for (k in req) cmd[[k]] <- NA_character_
  
  # --- ensure an output key placeholder exists (best-effort) -----------------
  if (isTRUE(require_output)) {
    out_candidates <- c("out", "io.out", "out.xml", "outxml", "mode.vector.out")
    out_present <- intersect(out_candidates, spec$key)
    if (length(out_present)) {
      k <- out_present[1]
      if (is.null(cmd[[k]])) cmd[[k]] <- NA_character_
    }
  }
  
  if (include_optional == "none") return(cmd)
  
  opt <- spec[!isTRUE(spec$mandatory), , drop = FALSE]
  if (!nrow(opt)) return(cmd)
  
  # --- method-aware filtering ------------------------------------------------
  # If the app exposes a "method" key and also has method.<name>.* subkeys,
  # only include the subkeys for the selected method (default or user-set).
  method_key_present <- "method" %in% spec$key
  has_method_subkeys <- any(grepl("^method\\.[^.]+\\.", spec$key))
  
  selected_method <- NA_character_
  if (method_key_present) {
    # default from spec if present, otherwise keep NA (then we won't filter)
    mrow <- spec[spec$key == "method", , drop = FALSE]
    if (nrow(mrow) == 1L) {
      if (!is.na(mrow$default) && nzchar(mrow$default)) {
        selected_method <- as.character(mrow$default)
      }
    }
  }
  
  allowed_method_prefix <- if (!is.na(selected_method) && nzchar(selected_method)) {
    paste0("method.", selected_method, ".")
  } else {
    NA_character_
  }
  
  for (i in seq_len(nrow(opt))) {
    k <- opt$key[i]
    if (!is.null(cmd[[k]])) next
    
    # Skip method.* subtrees that do not match the selected method
    if (method_key_present && has_method_subkeys && grepl("^method\\.", k)) {
      if (identical(k, "method")) {
        # keep "method" itself (will get default/NA below)
      } else {
        if (is.na(allowed_method_prefix) || !startsWith(k, allowed_method_prefix)) {
          next
        }
      }
    }
    
    if (include_optional == "defaults") {
      d <- opt$default[i]
      cmd[[k]] <- if (!is.na(d)) as.character(d) else NA_character_
    } else {
      cmd[[k]] <- NA_character_
    }
  }
  
  cmd
}


#' Show OTB help/spec summary for an application
#'
#' Prints a compact overview (help line count, parameter count) and the full
#' normalized parameter spec table.
#'
#' @param algo Character scalar. OTB application name.
#' @param gili Optional list from [linkOTB()].
#'
#' @return Invisibly returns a list with components `caps` and `spec`.
#'
#' @rdname otb_api
#' @export
otb_show <- function(algo, gili = NULL){
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

#' Set an output parameter path in a command list
#'
#' Updates `cmd[[key]]` to a normalized output path. If the parameter is pixel-typed
#' (`[pixel]` in OTB help), the value is set as `c(path, pixel_type)`.
#'
#' This function performs basic checks on directory existence and overwrite policy.
#'
#' @param cmd Command list as produced by [otb_build_cmd()].
#' @param gili Optional list from [linkOTB()]. If `NULL`, [linkOTB()] is called.
#' @param key Character scalar. Output parameter key to set (default `"out"`).
#' @param path Character scalar. Output file path.
#' @param pixel_type Optional character scalar pixel type (e.g. `"float"`). Only used
#'   if the output parameter is pixel-typed; if `NULL`, uses the pixel default from spec
#'   or `"float"` as fallback.
#' @param overwrite Logical. If `FALSE`, error if the file already exists.
#' @param create_dir Logical. If `TRUE`, create the output directory if missing.
#'
#' @return The modified command list.
#'
#' @rdname otb_api
#' @export
otb_set_out <- function(cmd, gili = NULL, key = "out", path,
                        pixel_type = NULL,
                        overwrite = TRUE,
                        create_dir = TRUE)  {
  
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
  
  spec <- otb_args_spec(algo, gili)
  
  i <- which(spec$key == key)
  if (!length(i)) stop("[otb_set_out] Output key not present in spec: ", key)
  i <- i[1]
  
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

# Footer
# - Neu-Workflow (C): Linux-only Phase -> Windows stop() in capabilities.
# - Alle Parser/Spec-Funktionen hängen an .otb_help_text() (launcher+env).
# - Keine Abhängigkeit von parseOTBFunction/parseOTBAlgorithms nötig.
