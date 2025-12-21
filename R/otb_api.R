# R/otb_api.R
# CRAN / roxygen glue for the OTB public API exported from R/otb_parse.R
# - Single family topic: ?otb_api
# - Each exported function in R/otb_parse.R should add: @rdname otb_api
# - No @inheritParams (avoids self-referential topic failures during document())

#' Orfeo ToolBox (OTB) helpers: introspection and command construction
#'
#' Public helpers to introspect OTB applications (by parsing `-help` output)
#' and to build/modify command lists consumable by [runOTB()].
#'
#' @details
#' The functions in this family are **non-invasive**: they do not mutate `PATH`
#' or global environment variables. They rely on a valid OTB descriptor as
#' returned by [linkOTB()]. On Linux/macOS the implementation expects a working
#' launcher (`gili$launcher`) and uses an explicit environment map internally.
#'
#' The common command representation is a list where:
#' - `cmd[[1]]` is the application name (character scalar), e.g. `"DimensionalityReduction"`.
#' - subsequent named entries represent CLI parameters **without** a leading dash,
#'   e.g. `cmd[["in"]]`, `cmd[["out"]]`, `cmd[["method"]]`.
#' - values are character scalars, `NA_character_` (placeholder), or for pixel-typed
#'   output parameters a character vector `c(path, pixel_type)`.
#'
#' @section Introspection:
#' - [otb_capabilities()] returns the raw help text and a parsed parameter table.
#' - [otb_args_spec()] normalizes the parsed table into a stable schema used by
#'   the helper functions below.
#'
#' @section Command helpers:
#' - [otb_required()], [otb_required_with_output()], [otb_optional()]
#' - [otb_build_cmd()] creates a template command list using the spec.
#' - [otb_set_out()] sets/validates an output parameter path (optionally pixel-typed).
#' - [otb_show()] prints a compact overview for interactive use.
#'
#' @seealso
#' [linkOTB()], [runOTB()], [runOTB_isolated()]
#'
#' @name otb_api
NULL
