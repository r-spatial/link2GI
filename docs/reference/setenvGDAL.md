# Usually for internally usage, initializes and set up access to the 'GDAL' command line interface

Initializes and sets up access to the 'GDAL' command line interface

## Usage

``` r
setenvGDAL(bin_GDAL = NULL)
```

## Arguments

- bin_GDAL:

  Character. Path to GDAL bin directory OR full path to gdalinfo(.exe).

## Value

The normalized GDAL bin directory path (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
setenvGDAL("C:/OSGeo4W64/bin")
} # }
```
