# Required parameter keys under link2GI output policy

Extends
[`otb_required`](https://r-spatial.github.io/link2GI/reference/otb_required.md)
by optionally enforcing the presence of at least one file-based output
parameter key in the returned set. This reflects the package policy
"always write outputs to disk" even if OTB marks output parameters as
optional.

## Usage

``` r
otb_required_with_output(algo, gili = NULL, enforce_output = TRUE)
```

## Arguments

- algo:

  Character scalar. OTB application name (e.g. `"BandMathX"`).

- gili:

  Optional list returned by
  [`link2GI::linkOTB`](https://r-spatial.github.io/link2GI/reference/linkOTB.md).
  If `NULL`,
  [`link2GI::linkOTB()`](https://r-spatial.github.io/link2GI/reference/linkOTB.md)
  is called internally.

- enforce_output:

  Logical. If `TRUE` (default), include the first matching file output
  key found in the application's parameter set.

## Value

Character vector of required keys (without leading `-`).

## Details

The function does *not* guess output pixel types or create temporary
file names. It only returns keys.

## Examples

``` r
if (FALSE) { # \dontrun{
otb <- link2GI::linkOTB()
if (isTRUE(otb$exist)) {
  otb_required_with_output("DimensionalityReduction", otb)
}
} # }
```
