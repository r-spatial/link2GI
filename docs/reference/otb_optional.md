# Get optional parameters for an OTB application (keys + defaults optional)

Get optional parameters for an OTB application (keys + defaults
optional)

## Usage

``` r
otb_optional(algo, gili = NULL, with_defaults = TRUE)
```

## Arguments

- algo:

  Character. OTB application name.

- gili:

  Optional list returned by \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

- with_defaults:

  Logical. If TRUE, fill parsed defaults where available; else NA.

## Value

Named list.
