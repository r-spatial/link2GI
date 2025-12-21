# Select newest OTB installation row index from a vector of binDir paths

Heuristic order: 1) ../VERSION file (preferred; parses "OTB Version:
x.y.z") 2) folder name pattern "OTB-x.y.z" (fallback)

## Usage

``` r
getrowotbVer(paths)
```

## Arguments

- paths:

  Character vector of binDir paths (may have trailing slash).

## Value

Integer row index (1..length(paths)), defaults to 1 on failure.

## Details

Uses numeric_version for correct semantic version comparison.
