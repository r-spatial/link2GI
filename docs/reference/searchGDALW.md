# Search recursively for valid GDAL installation(s) on Windows

Finds \`gdalinfo.exe\` via \`where /R\`, derives \`binDir\` and
\`baseDir\`, classifies the installation type by path heuristics, and
lists available GDAL CLI tools (\`gdal\*\`) + python tools (\`\*.py\`)
in each \`binDir\`.

## Usage

``` r
searchGDALW(DL = "C:/", quiet = TRUE)
```

## Arguments

- DL:

  Character. Search root (e.g. \`"C:/"\`).

- quiet:

  Logical. Suppress messages.

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
