# Assemble args from a skeleton and a named list

Assemble args from a skeleton and a named list

## Usage

``` r
gdal_build_args(skel, opts = list(), positional = character())
```

## Arguments

- skel:

  Result of gdal_skeleton().

- opts:

  Named list. Names map to flags without leading dash, e.g.
  list(t_srs="EPSG:25832").

- positional:

  Character. Additional positional args (e.g. input/output files).

## Value

Character vector of CLI args.
