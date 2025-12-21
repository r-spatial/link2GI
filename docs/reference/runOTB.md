# Run an OTB application (new workflow C)

Executes an Orfeo ToolBox application via the launcher/wrapper described
by \`gili\` (typically returned by \[linkOTB()\]). This wrapper is
non-invasive: it does not permanently modify PATH or the user
environment.

## Usage

``` r
runOTB(
  otbCmdList,
  gili = NULL,
  retRaster = TRUE,
  retCommand = FALSE,
  quiet = TRUE
)
```

## Arguments

- otbCmdList:

  List. OTB command list. The first element must be the algorithm name;
  remaining named elements are parameter keys/values.

- gili:

  List. OTB installation descriptor as returned by \[linkOTB()\]. If
  \`NULL\`, \[linkOTB()\] is called.

- retRaster:

  Logical. If \`TRUE\`, return a \`terra::SpatRaster\` for the primary
  raster output (when detectable). If \`FALSE\`, return the output
  path(s) (character) or a status code depending on implementation.

- retCommand:

  Logical. If \`TRUE\`, do not execute; return the exact CLI command
  string that would be run.

- quiet:

  Logical. If \`TRUE\`, suppress console output from OTB (best-effort).

## Value

Depending on \`retCommand\` / \`retRaster\`, returns either a command
string, a \`terra::SpatRaster\`, or a character vector/status describing
the produced output.

## Details

The command is provided as a list in "link2GI style": -
\`otbCmdList\[\[1\]\]\` is the application name (e.g.,
\`"DimensionalityReduction"\`) - named elements are OTB parameter keys
(without leading \`-\`)

Parameter values can be: - a scalar character/numeric (converted to
character) - \`NA\` / \`NA_character\_\` to omit the parameter - for
pixel-typed outputs: a character vector of length 2 \`c("\<path\>",
"\<pixel_type\>")\` (e.g. \`c("out.tif","float")\`)
