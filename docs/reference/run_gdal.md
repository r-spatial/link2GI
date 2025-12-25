# Run a GDAL utility deterministically

Run a GDAL utility deterministically

## Usage

``` r
run_gdal(
  gdal,
  cmd,
  args = character(),
  wd = NULL,
  timeout = NULL,
  echo = FALSE,
  capture = TRUE,
  log = TRUE
)
```

## Arguments

- gdal:

  A gdal_context.

- cmd:

  Character. Utility name (e.g., "gdalwarp") or a full path.

- args:

  Character vector of CLI args (tokens).

- wd:

  Working directory (optional).

- timeout:

  Timeout seconds (best-effort; no hard kill in base R).

- echo:

  Logical. If TRUE, print the resolved command line.

- capture:

  Logical. If TRUE, capture stdout/stderr.

- log:

  Logical. If TRUE, append an NDJSON record to gdal\$log_file.

## Value

A list with status/stdout/stderr/call/meta.
