# Execute an OTB application

Builds a CLI call from a parameter list created by
\[parseOTBFunction()\] and executes it. On Linux, execution uses the OTB
launcher with a call-local environment (no global env mutation).

## Usage

``` r
runOTB(
  otbCmdList = NULL,
  gili = NULL,
  retRaster = TRUE,
  retCommand = FALSE,
  quiet = TRUE
)
```

## Arguments

- otbCmdList:

  List as returned by \[parseOTBFunction()\], with parameters set.

- gili:

  Optional list returned by \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

- retRaster:

  Logical. If \`TRUE\`, attempt to read raster/vector/XML output.

- retCommand:

  Logical. If \`TRUE\`, return only a printable command string.

- quiet:

  Logical. If \`FALSE\`, do not suppress stdout/stderr.

## Value

Depending on \`retRaster\` and output type: a \`terra::SpatRaster\`, an
\`xml2::xml_document\`, an \`sf\` object, or \`invisible(NULL)\`.

## Examples

``` r
if (FALSE) { # \dontrun{
otb <- link2GI::linkOTB()
if (otb$exist) {
  cmd <- parseOTBFunction("ComputeImagesStatistics", otb)
  cmd[["input_il"]] <- "/path/to/image.tif"
  cmd[["out.xml"]]  <- tempfile(fileext = ".xml")
  runOTB(cmd, otb, retRaster = TRUE, quiet = FALSE)
}
} # }
```
