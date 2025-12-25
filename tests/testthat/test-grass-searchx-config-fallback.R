searchGRASSX <- function(MP = "default", quiet = TRUE) {
  
  .msg <- function(...) if (!isTRUE(quiet)) message(...)
  
  # ---- FIX: define version variable for all branches ----
  ver_char <- NA_character_
  
  # 0) Hint handling
  hint_dir <- NULL
  if (!is.null(MP) && !identical(MP, "default")) {
    if (.link2gi_file_exists(MP) && !.link2gi_dir_exists(MP)) hint_dir <- dirname(MP)
    if (.link2gi_dir_exists(MP)) hint_dir <- MP
  }
  
  # 1) Locate grass executable
  grass_exe <- unname(.link2gi_which("grass")[1])
  if (!nzchar(grass_exe) && !is.null(hint_dir)) {
    cand <- file.path(hint_dir, "grass")
    if (.link2gi_file_exists(cand)) grass_exe <- cand
  }
  
  if (!nzchar(grass_exe)) {
    .msg("searchGRASSX(): 'grass' not found on PATH.")
    return(FALSE)
  }
  
  # 2) Ask GRASS for GISBASE (preferred)
  gisbase_out <- try(.link2gi_sys2(grass_exe, c("--config", "path"), stdout = TRUE, stderr = TRUE), silent = TRUE)
  
  if (!inherits(gisbase_out, "try-error") && length(gisbase_out) > 0) {
    gisbase <- trimws(gisbase_out[1])
    if (nzchar(gisbase) && .link2gi_dir_exists(gisbase)) {
      
      # Version (best-effort)
      ver_out <- try(.link2gi_sys2(grass_exe, "--version", stdout = TRUE, stderr = TRUE), silent = TRUE)
      if (!inherits(ver_out, "try-error") && length(ver_out) > 0) {
        m <- regmatches(ver_out[1], regexpr("[0-9]+(\\.[0-9]+)+", ver_out[1]))
        if (length(m) > 0) ver_char <- m
      }
      
      return(data.frame(
        instDir = gisbase,
        version = ver_char,
        installation_type = basename(grass_exe),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # 3) Fallback candidates (last resort)
  roots <- c("/usr/lib", "/usr/local/lib", "/opt")
  cand <- unlist(lapply(roots, function(r) .link2gi_glob(file.path(r, "grass*"))), use.names = FALSE)
  cand <- cand[nzchar(cand)]
  cand <- cand[.link2gi_dir_exists(cand)]
  
  ok <- vapply(cand, function(p) {
    .link2gi_dir_exists(file.path(p, "etc")) &&
      (.link2gi_file_exists(file.path(p, "etc", "VERSIONNUMBER")) || .link2gi_dir_exists(file.path(p, "scripts")))
  }, logical(1))
  cand <- cand[ok]
  
  if (length(cand) == 0) {
    .msg("searchGRASSX(): could not resolve GISBASE via '--config path' and no fallback candidates found.")
    return(FALSE)
  }
  
  suf <- suppressWarnings(as.integer(gsub(".*grass", "", basename(cand), ignore.case = TRUE)))
  ord <- order(ifelse(is.na(suf), -Inf, suf), decreasing = TRUE)
  cand <- cand[ord]
  
  # ---- FIX: ver_char exists here (NA if unknown) ----
  data.frame(
    instDir = cand[1],
    version = ver_char,
    installation_type = basename(grass_exe),
    stringsAsFactors = FALSE
  )
}
