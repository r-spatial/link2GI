# Collect a GDAL capability fingerprint for a context

Collect a GDAL capability fingerprint for a context

## Usage

``` r
gdal_fingerprint(gdal, log = TRUE)
```

## Arguments

- gdal:

  A gdal_context.

- log:

  Logical. If TRUE, append fingerprint record to NDJSON
  (type="fingerprint").

## Value

A list with version strings and formats.
