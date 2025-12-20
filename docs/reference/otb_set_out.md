# Set an OTB output parameter safely (file-based output)

Assign a file-based output parameter (e.g. `out`, `io.out`,
`mode.vector.out`) in a runOTB command list created by
[`otb_build_cmd`](https://r-spatial.github.io/link2GI/reference/otb_build_cmd.md).

## Usage

``` r
otb_set_out(
  cmd,
  gili = NULL,
  key = "out",
  path,
  pixel_type = NULL,
  overwrite = TRUE,
  create_dir = TRUE
)
```

## Arguments

- cmd:

  List. A command template created by
  [`otb_build_cmd`](https://r-spatial.github.io/link2GI/reference/otb_build_cmd.md).

- gili:

  Optional list returned by
  [`linkOTB`](https://r-spatial.github.io/link2GI/reference/linkOTB.md).
  If `NULL`,
  [`linkOTB()`](https://r-spatial.github.io/link2GI/reference/linkOTB.md)
  is called.

- key:

  Character scalar. Output parameter key to set (default: `"out"`).

- path:

  Character scalar. Output file path.

- pixel_type:

  Optional character scalar. Pixel type (e.g. `"float"`, `"uint16"`).
  Only applied if `key` is a `[pixel]` output. If `NULL`, the function
  uses the parsed default if available, otherwise falls back to
  `"float"`.

- overwrite:

  Logical. If `FALSE`, error if `path` already exists.

- create_dir:

  Logical. If `TRUE`, create the parent directory of `path` if missing.

## Value

The modified `cmd` list.

## Details

This helper enforces the package policy of explicit on-disk outputs: it
never creates temporary paths and never fills unrelated parameters such
as `outinv`, `outmatrix`, or `bv` with pixel types.

Some OTB outputs are typed as `[pixel]` in the help output. For such
keys, OTB accepts either:

- a single path (OTB uses its internal default), or

- `c(path, pixel_type)` (explicit pixel type).

This function will only append `pixel_type` for keys that are detected
as `[pixel]` outputs.

## Examples

``` r
if (FALSE) { # \dontrun{
otb <- link2GI::linkOTB()
cmd <- link2GI::otb_build_cmd("DimensionalityReduction", otb, include_optional="defaults")
cmd[["in"]] <- "in.tif"
cmd <- link2GI::otb_set_out(cmd, otb, key="out", path="pca.tif")
link2GI::runOTB(cmd, otb, quiet=FALSE)
} # }
```
