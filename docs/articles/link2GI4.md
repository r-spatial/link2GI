# OTB Wrapper in link2GI

``` r
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)
```

The Orfeo Toolbox (OTB) provides a collection of command-line
applications for remote sensing and image processing. `link2GI` offers
an R wrapper around these applications with a focus on
**reproducibility, transparency, and robustness across OTB versions**.

Historically, `link2GI` exposed OTB applications through a heuristic
list-based wrapper (“legacy API”). This is retained for backward
compatibility. In addition, `link2GI` now provides a **new
introspection-based API** that derives parameter metadata from OTB’s own
CLI help output and avoids implicit assumptions.

## Two APIs: legacy vs. introspection

### Legacy API (`parseOTBFunction()` / `runOTB()`)

The legacy API parses the output of:

    otbcli <algorithm> -help

and converts it into a modifiable R list representing the command-line
call.

Characteristics:

- simple and permissive
- minimal upfront constraints
- convenient for older scripts

Limitations:

- required parameters and defaults are inferred heuristically
- sensitive to changes in OTB help formatting
- less suitable for strict, version-stable workflows

### Introspection-based API (recommended)

The new API treats the OTB CLI help output as the **single source of
truth** for parameters and their status (mandatory vs. optional) and
supports a strict “write-to-disk” workflow.

Core functions:

- [`otb_capabilities()`](https://r-spatial.github.io/link2GI/reference/otb_capabilities.md):
  retrieve CLI help text (source of truth)
- [`otb_args_spec()`](https://r-spatial.github.io/link2GI/reference/otb_args_spec.md):
  normalized parameter spec table parsed from `-help`
- [`otb_build_cmd()`](https://r-spatial.github.io/link2GI/reference/otb_build_cmd.md):
  create a
  [`runOTB()`](https://r-spatial.github.io/link2GI/reference/runOTB.md)-compatible
  command template (valid keys only)
- [`otb_set_out()`](https://r-spatial.github.io/link2GI/reference/otb_set_out.md):
  set file-based outputs safely (including `[pixel]` outputs)

------------------------------------------------------------------------

## Typical modern workflow

The following example demonstrates a complete workflow:

1.  link an installed OTB
2.  read and display the raw `-help` output (source of truth)
3.  inspect the parsed parameter table (`spec`)
4.  build a command template from *valid keys*
5.  set input and explicit on-disk output
6.  run via
    [`runOTB()`](https://r-spatial.github.io/link2GI/reference/runOTB.md)
    and plot the result

### Example: `DimensionalityReduction` (PCA) — fully transparent

This example avoids “guessing” parameter names. The only manual step is
choosing the correct parameter key for “number of PCA components” by
reading the `spec` table printed from your local OTB build.

``` r
library(link2GI)
library(terra)
library(mlr3spatial)

# 0) Link OTB
otb <- link2GI::linkOTB(searchLocation = "~/apps/otb911/")

# 1) Choose algorithm
algo <- "DimensionalityReduction"

# 2) Read the OTB help text (source of truth)
caps <- link2GI::otb_capabilities(algo, otb, include_param_help = FALSE)
cat(paste(head(caps$text, 60), collapse = "\n"), "\n")

# 3) Get the parsed parameter table (still transparent)
spec <- link2GI::otb_args_spec(algo, otb)
print(spec[, c("key", "class", "mandatory", "default")], row.names = FALSE)

# 4) Identify the PCA component-count key by inspecting 'spec'
#    (no fallback list, no heuristics)
pca_related <- spec[grepl("^method\\.pca\\.|^nbcomp$|outdim|comp", spec$key), ]
print(pca_related[, c("key", "mandatory", "default", "desc")], row.names = FALSE)

# Pick the correct key from the printed table (example for OTB 9.1.1: "nbcomp")
nb_key <- "nbcomp"

# 5) Build the command template (only valid keys)
cmd <- link2GI::otb_build_cmd(
  algo,
  otb,
  include_optional = "defaults",
  require_output = TRUE
)

# 6) Input + output (explicit on-disk path, not tempdir)
infile <- system.file("extdata", "leipzig_raster.tif", package = "mlr3spatial")

out_dir <- file.path(getwd(), "otb_tutorial_out")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_pca <- file.path(out_dir, "leipzig_pca.tif")

cmd[["in"]] <- infile
cmd <- link2GI::otb_set_out(cmd, otb, key = "out", path = out_pca)

# 7) PCA settings (explicit)
cmd[["method"]] <- "pca"
cmd[[nb_key]]   <- "9"

# 8) Show the exact CLI that will be executed
cat(link2GI::runOTB(cmd, otb, retCommand = TRUE, retRaster = FALSE), "\n")

# 9) Run + plot
pca_rast <- link2GI::runOTB(cmd, otb, retRaster = TRUE, quiet = FALSE)
plot(pca_rast[[1]], main = "PCA output (band 1)")
```

## Legacy workflow (still supported)

The legacy workflow builds a command list by parsing `-help` output and
then executes it.

``` r
library(link2GI)
library(terra)

otb <- link2GI::linkOTB()

algo <- "EdgeExtraction"
cmd  <- parseOTBFunction(algo = algo, gili = otb)

# legacy uses input_in / input_il mappings

cmd$input_in <- system.file("ex/elev.tif", package = "terra")

# set required parameters (example values; check cmd$help)

cmd$filter  <- "touzi"
cmd$channel <- 1

# explicit output path (recommended even in legacy)

cmd$out <- tempfile(fileext = ".tif")

res <- runOTB(cmd, gili = otb, quiet = FALSE)
plot(res)
```

## What changed and why

Older versions of the wrapper tried to be convenient by guessing
defaults and required parameters. That is fragile across OTB releases.

The introspection-based API:

- trusts OTB metadata instead of heuristics
- separates capability discovery from execution
- enforces explicit file outputs (reproducible artifacts)
- makes failures explicit rather than silent

## Compatibility notes

- The introspection-based API is intended for OTB installations where
  the CLI `-help` output provides a stable, parseable `Parameters:`
  section.
- If your OTB version does not expose a parseable parameter block, the
  introspection parser may return an empty spec; in that case the legacy
  API may still work (best-effort).

## Migration hints

- Existing scripts that use
  [`parseOTBFunction()`](https://r-spatial.github.io/link2GI/reference/parseOTBFunction.md)
  and
  [`runOTB()`](https://r-spatial.github.io/link2GI/reference/runOTB.md)
  should continue to work.
- For new projects, prefer
  [`otb_capabilities()`](https://r-spatial.github.io/link2GI/reference/otb_capabilities.md) +
  [`otb_args_spec()`](https://r-spatial.github.io/link2GI/reference/otb_args_spec.md) +
  [`otb_build_cmd()`](https://r-spatial.github.io/link2GI/reference/otb_build_cmd.md) +
  [`otb_set_out()`](https://r-spatial.github.io/link2GI/reference/otb_set_out.md)
  and then execute via
  [`runOTB()`](https://r-spatial.github.io/link2GI/reference/runOTB.md).
