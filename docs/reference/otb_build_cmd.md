# Build a runOTB-compatible command template from OTB application metadata

Constructs a command list in the format expected by
[`runOTB`](https://r-spatial.github.io/link2GI/reference/runOTB.md): the
first element is the application name, followed by named parameters
(without leading `-`).

## Usage

``` r
otb_build_cmd(
  algo,
  gili = NULL,
  include_optional = c("none", "defaults", "all_na"),
  require_output = TRUE
)
```

## Arguments

- algo:

  Character scalar. Name of the OTB application.

- gili:

  Optional list returned by
  [`linkOTB`](https://r-spatial.github.io/link2GI/reference/linkOTB.md).
  If `NULL`,
  [`linkOTB()`](https://r-spatial.github.io/link2GI/reference/linkOTB.md)
  is called internally.

- include_optional:

  Character. One of `"none"`, `"defaults"`, `"all_na"`:

  `"none"`

  :   Do not include optional parameters.

  `"defaults"`

  :   Include optional parameters and prefill parsed defaults where
      available.

  `"all_na"`

  :   Include optional parameters but set all to `NA_character_`.

- require_output:

  Logical. If `TRUE` (default), enforce that at least one file-based
  output parameter key is present in the returned command template (e.g.
  `out`, `io.out`, `mode.vector.out`). This reflects the link2GI
  workflow policy to always write outputs to disk, even if OTB marks
  some output parameters as optional.

## Value

A list suitable for
[`runOTB`](https://r-spatial.github.io/link2GI/reference/runOTB.md).

## Details

Mandatory parameters reported by OTB are always included and initialized
as `NA_character_` to force explicit user input.

Optional parameters can be included either with parsed defaults (when
available) or as `NA_character_`.

By default, this function also enforces presence of at least one
file-based output parameter in the template (see `require_output`),
reflecting the package policy to always write outputs to disk.

## See also

[`otb_capabilities`](https://r-spatial.github.io/link2GI/reference/otb_capabilities.md),
[`otb_args_spec`](https://r-spatial.github.io/link2GI/reference/otb_args_spec.md),
[`otb_required`](https://r-spatial.github.io/link2GI/reference/otb_required.md),
[`otb_optional`](https://r-spatial.github.io/link2GI/reference/otb_optional.md),
[`otb_set_out`](https://r-spatial.github.io/link2GI/reference/otb_set_out.md)

## Examples

``` r
if (FALSE) { # \dontrun{
otb <- link2GI::linkOTB()
cmd <- otb_build_cmd("BandMathX", otb, include_optional = "defaults")
cmd[["il"]]  <- c("a.tif", "b.tif")
cmd <- otb_set_out(cmd, otb, path = "out.tif")
cmd[["exp"]] <- "im1b1 + im2b1"
runOTB(cmd, otb, quiet = FALSE)
} # }
```
