# Retrieve the argument list from an OTB application

Queries \`-help\` output and returns a parameter list prefilled with
detected defaults.

## Usage

``` r
parseOTBFunction(algo = NULL, gili = NULL)
```

## Arguments

- algo:

  Character. OTB application name (see \[parseOTBAlgorithms()\]).

- gili:

  Optional list returned by \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

## Value

A list whose first element is the \`algo\` name, followed by parameters,
plus a \`\$help\` element with per-parameter help text.

## Examples

``` r
if (FALSE) { # \dontrun{
otb <- link2GI::linkOTB()
if (otb$exist) parseOTBFunction("ComputeImagesStatistics", otb)
} # }
```
