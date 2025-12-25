# Build a minimal CLI skeleton from "–help"

Extracts the first "Usage:" line (best-effort) and a flat list of flags.
Does NOT attempt semantic validation.

## Usage

``` r
gdal_skeleton(gdal, cmd)
```

## Arguments

- gdal:

  A gdal_context.

- cmd:

  Utility name, e.g. "gdalwarp".

## Value

List with exe, usage, flags, args_template.
