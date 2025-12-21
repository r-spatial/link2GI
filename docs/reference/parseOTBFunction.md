# Retrieve the argument list from an OTB application

Legacy convenience wrapper that returns a list containing: - first
element: algo name - named entries: parameter defaults (if any) and
"mandatory" markers - \`\$help\`: per-parameter help text (if available)

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

List (legacy format).

## Details

Under the hood this uses the NEW introspection API:
\[otb_capabilities()\] and \[otb_args_spec()\].
