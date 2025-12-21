# R/otb_version_select.R
# Internal helper: getrowotbVer()
#
# NOTE: you already call getrowotbVer(params_OTB$binDir) in linkOTB_linux().
# If it exists elsewhere and is stable, keep it. If not, drop-in this one.

#' Select "newest" OTB installation row index
#'
#' Heuristic: parse a version-like token from binDir/baseDir path. If none can be
#' parsed, fall back to the last row.
#'
#' @param paths Character vector of binDir paths.
#' @return Integer row index (1..length(paths))
#' @keywords internal
getrowotbVer <- function(paths) {
  
  if (!is.character(paths) || !length(paths)) return(1L)
  
  # Extract the longest digit/dot sequence, e.g. "9.1.1" or "911" etc.
  extract_ver <- function(p) {
    p <- basename(normalizePath(p, mustWork = FALSE))
    m <- regmatches(p, regexpr("[0-9]+(\\.[0-9]+)+", p, perl = TRUE))
    if (length(m) && nzchar(m)) return(m)
    
    m2 <- regmatches(p, regexpr("[0-9]{3,}", p, perl = TRUE))
    if (length(m2) && nzchar(m2)) return(m2)
    
    NA_character_
  }
  
  vers <- vapply(paths, extract_ver, character(1))
  
  # Convert to comparable numeric tuple (pad to 4 parts)
  to_tuple <- function(v) {
    if (is.na(v)) return(c(-Inf, -Inf, -Inf, -Inf))
    if (grepl("\\.", v)) {
      parts <- as.numeric(strsplit(v, "\\.", fixed = FALSE)[[1]])
    } else {
      # "911" -> 9.1.1 (best-effort)
      chars <- strsplit(v, "", fixed = TRUE)[[1]]
      parts <- suppressWarnings(as.numeric(chars))
    }
    parts <- parts[!is.na(parts)]
    parts <- c(parts, rep(0, 4 - length(parts)))[1:4]
    parts
  }
  
  mat <- t(vapply(vers, to_tuple, numeric(4)))
  
  # order by tuple
  o <- do.call(order, as.data.frame(mat))
  idx <- o[length(o)]
  if (is.na(idx) || idx < 1L) idx <- 1L
  idx
}

# Footer
# - NEW workflow (C): makes linkOTB_linux() deterministic when multiple installs exist.
# - No interactive selection here (that stays in linkOTB_* via ver_select).
