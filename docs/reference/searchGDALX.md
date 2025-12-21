# Search recursively for valid GDAL installation(s) on Linux/macOS

Searches for an executable \`gdalinfo\` and returns a normalized
installations table plus best-effort lists of GDAL binaries (\`gdal\*\`)
and python tools (\`\*.py\`) found alongside the detected \`gdalinfo\`.

## Usage

``` r
searchGDALX(MP = "default", quiet = TRUE)
```

## Arguments

- MP:

  Character. Search root. \`"default"\` expands to
  \`c("~","/opt","/usr/local","/usr")\`. You may also pass a single
  directory (e.g. \`"/usr"\`).

- quiet:

  Logical. If \`TRUE\`, suppress messages.

## Value

A list with:

- gdalInstallations:

  data.frame with columns \`binDir\`, \`baseDir\`,
  \`installation_type\`.

- bin:

  list of data.frames (column \`gdal_bin\`) with detected GDAL binaries
  per installation.

- py:

  list of data.frames (column \`gdal_py\`) with detected GDAL python
  tools per installation.

## Details

This implementation is portable: it does NOT use GNU-only \`find\`
primaries such as \`-readable\`, and it uses \`system2(..., args=...)\`
with proper token separation (no shell parsing assumptions).

## Examples

``` r
if (FALSE) { # \dontrun{
x <- searchGDALX()
x$gdalInstallations
} # }
```
