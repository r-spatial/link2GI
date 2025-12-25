# Search for valid SAGA GIS installation(s) on Unix (Linux/macOS)

Strategy: 1) Search for \`saga_cmd\` under given mountpoint(s) using
portable \`find\` primaries (no GNU-only \`-executable\`, no \`!
-readable\`). 2) For each hit, derive \`binDir\` and \`moduleDir\` by
checking sibling folders (\`tools\` preferred, then \`modules\`). 3)
Parse version best-effort via \`saga_cmd –version\`.

## Usage

``` r
searchSAGAX(MP = "default", quiet = TRUE)
```

## Arguments

- MP:

  Character. Search root(s). \`"default"\` expands to
  \`c("~","/opt","/usr/local","/usr")\`.

- quiet:

  Logical. Suppress messages.

## Value

\`FALSE\` or a \`data.frame\` with columns \`binDir\`, \`moduleDir\`,
\`version\`, \`installation_type\`.
