# Search for valid GRASS GIS installation(s) on Unix (Linux/macOS)

Strategy: 1) Prefer \`grass –config path\` (returns GISBASE on modern
GRASS) 2) Fallback: locate \`grass\` via \`Sys.which()\`, then infer
common GISBASE paths

## Usage

``` r
searchGRASSX(MP = "default", quiet = TRUE)
```

## Arguments

- MP:

  Character. Ignored for detection (kept for API compatibility). You may
  pass a directory or an executable path; it will be used only as a
  hint.

- quiet:

  Logical.

## Value

FALSE or data.frame(instDir, version, installation_type)
