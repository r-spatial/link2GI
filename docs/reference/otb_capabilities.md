# Retrieve OTB application capabilities (help text + parsed parameters)

Retrieve OTB application capabilities (help text + parsed parameters)

## Usage

``` r
otb_capabilities(algo, gili = NULL, include_param_help = FALSE)
```

## Arguments

- algo:

  Character. OTB application name (e.g. "BandMathX").

- gili:

  Optional list returned by \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

- include_param_help:

  Logical. If \`TRUE\`, calls \`-help \<param\>\` for each key.

## Value

List with elements:

- text:

  Character vector of the \`-help\` output.

- params:

  Parsed parameter table (data.frame / tibble depending on your parser).

- param_help:

  Named list of per-parameter help texts (or \`NULL\`).
