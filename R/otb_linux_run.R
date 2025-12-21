# R/otb_linux_run.R
# Internal implementation: .otb_run_launcher_linux() + shared helper .otb_help_text()

#' @keywords internal
.otb_run_launcher_linux <- function(gili, args,
                                    env = NULL,
                                    stdout = FALSE, stderr = FALSE) {
  
  launcher <- gili$launcher
  if (is.null(launcher) || is.na(launcher) || !file.exists(launcher)) {
    stop("OTB launcher not found (gili$launcher).", call. = FALSE)
  }
  
  # env -> NAME=VALUE vector
  env_vec <- NULL
  if (!is.null(env)) {
    if (is.list(env)) env_vec <- unlist(env, use.names = TRUE)
    if (is.character(env)) env_vec <- env
    if (!is.null(env_vec)) {
      if (!is.null(names(env_vec)) && any(nzchar(names(env_vec)))) {
        env_vec <- paste0(names(env_vec), "=", as.character(env_vec))
      }
      env_vec <- env_vec[nzchar(env_vec)]
    }
  }
  
  # system2 semantics:
  # - stdout=TRUE  -> capture output as character vector
  # - stdout=FALSE -> print to console
  cap_out <- if (isTRUE(stdout)) TRUE else FALSE
  cap_err <- if (isTRUE(stderr)) TRUE else FALSE
  
  out <- suppressWarnings(system2(
    command = launcher,
    args    = args,
    env     = env_vec,
    stdout  = cap_out,
    stderr  = cap_err
  ))
  
  # If we captured output, tolerate non-zero exit status (help often returns 1)
  if ((isTRUE(stdout) || isTRUE(stderr)) && is.numeric(out) && length(out) == 1L) {
    # In rare cases system2 may still return status; don't warn here.
    return(invisible(out))
  }
  
  out
}

#' @keywords internal
.otb_help_text <- function(gili, algo, param = NULL) {
  
  otb_root  <- .otb_root_from_gili(gili)
  env_named <- .otb_env_linux(otb_root)
  
  help_args <- c(algo, "-help")
  if (!is.null(param) && length(param) > 0) {
    help_args <- c(help_args, as.character(param))
  }
  
  # capture help output
  txt <- .otb_run_launcher_linux(
    gili,
    args   = help_args,
    env    = env_named,
    stdout = TRUE,    # TRUE -> capture (system2 semantics)
    stderr = TRUE
  )
  
  
  # tolerate non-zero exit for -help if "Parameters:" appears
  if (!is.character(txt) || !length(txt)) {
    stop("OTB help output is empty for: ", algo, call. = FALSE)
  }
  
  txt
}

# Footer
# - Neu-Workflow (C): Linux runner ist strikt launcher-basiert + explicit env.
# - Legacy capture= Parameter entfernt (keine Dual-Semantik mehr).
# - help calls laufen mit .otb_env_linux() (Fix für libOTBCommandLine missing / LD_LIBRARY_PATH).

