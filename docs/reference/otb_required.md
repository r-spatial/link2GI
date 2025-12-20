# OTB-required parameter keys for an application

Returns the parameter keys that are strictly required according to the
OTB CLI help output. A parameter is considered required if it is marked
as `MISSING` in the "Parameters:" block or contains `(mandatory)` in its
description.

## Usage

``` r
otb_required(algo, gili = NULL)
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

## Value

Character vector of required parameter keys (without leading `-`).
Returns an empty character vector if no parameters could be parsed.

## Details

This function does **not** apply any additional package policy such as
enforcing file-based outputs. For that, use
[`otb_required_with_output`](https://r-spatial.github.io/link2GI/reference/otb_required_with_output.md).

## Examples

``` r
if (FALSE) { # \dontrun{
otb <- link2GI::linkOTB()
if (isTRUE(otb$exist)) {
  otb_required("DimensionalityReduction", otb)
}
} # }
```
