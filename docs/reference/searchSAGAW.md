# Search for valid SAGA GIS installations on Windows

Uses \`where /R \<DL\> saga_cmd.exe\` and derives \`binDir\` and
\`moduleDir\`.

## Usage

``` r
searchSAGAW(DL = "C:/", quiet = TRUE)
```

## Arguments

- DL:

  Character. Search root (e.g. \`"C:/"\`).

- quiet:

  Logical. Suppress messages.

## Value

\`FALSE\` or a \`data.frame\` with columns \`binDir\`, \`moduleDir\`,
\`installation_type\`.
