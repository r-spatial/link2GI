# Create a GDAL context from linkGDAL()

Convenience helper that derives a GDAL CLI context from the result of
\[linkGDAL()\], ensuring that the linked GDAL binaries are used.

## Usage

``` r
gdal_context_from_link(
  gdal_link,
  env = character(),
  log_file = file.path(tempdir(), "gdal-run.ndjson"),
  help_cache_dir = file.path(tempdir(), "gdal-help-cache")
)
```

## Arguments

- gdal_link:

  Result of \[linkGDAL()\].

- env:

  Named character vector of environment variables.

- log_file:

  NDJSON log file path.

- help_cache_dir:

  Directory for cached GDAL help output.

## Value

An object of class \`gdal_context\`.
