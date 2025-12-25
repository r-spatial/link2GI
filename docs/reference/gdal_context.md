# Create a GDAL CLI context from linked binaries

Resolves executable paths for GDAL utilities (e.g., gdalinfo, gdalwarp,
ogr2ogr) under a specific binDir and prepares a per-call environment
(PATH + optional vars).

## Usage

``` r
gdal_context(
  binDir,
  env = character(),
  log_file = file.path(tempdir(), "gdal-run.ndjson"),
  help_cache_dir = file.path(tempdir(), "gdal-help-cache")
)
```

## Arguments

- binDir:

  Character. Directory containing GDAL executables.

- env:

  Named character vector. Additional env vars (e.g., GDAL_DATA,
  PROJ_LIB).

- log_file:

  Character. NDJSON log file path (default: "gdal-run.ndjson" in
  tempdir()).

- help_cache_dir:

  Character. Directory for cached help texts (default: "gdal-help-cache"
  in tempdir()).

## Value

A list (class "gdal_context") with resolved executables and env.
