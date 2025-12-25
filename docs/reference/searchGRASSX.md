# Search for valid GRASS GIS installation(s) on Unix (Linux/macOS)

Strategy: 1) Prefer \`grass –config path\` (returns GISBASE on modern
GRASS) 2) Fallback: locate candidates under common roots (/usr/lib,
/usr/local/lib, /opt)

## Usage

``` r
searchGRASSX(MP = "default", quiet = TRUE)
```

## Arguments

- MP:

  Character. Optional hint: directory or executable path.

- quiet:

  Logical.

## Value

FALSE or data.frame(instDir, version, installation_type)
