# Retrieve available OTB applications

On Linux, do NOT rely on \`-print_applications\` (not supported in your
CLI layout). Instead, list \`otbapp\_\*.so\` (or .dll/.dylib) under
OTB_APPLICATION_PATH. On Windows, list \`otbcli\_\<Algo\>.bat/.exe\`
wrappers in \`bin\`.

## Usage

``` r
parseOTBAlgorithms(gili = NULL)
```

## Arguments

- gili:

  Optional list returned by \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

## Value

Character vector of application names.

## Examples

``` r
if (FALSE) { # \dontrun{
otb <- link2GI::linkOTB()
if (otb$exist) parseOTBAlgorithms(otb)
} # }
```
