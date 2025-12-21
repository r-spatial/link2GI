# Select "newest" OTB installation row index

Heuristic: parse a version-like token from binDir/baseDir path. If none
can be parsed, fall back to the last row.

## Usage

``` r
getrowotbVer(paths)
```

## Arguments

- paths:

  Character vector of binDir paths.

## Value

Integer row index (1..length(paths))
