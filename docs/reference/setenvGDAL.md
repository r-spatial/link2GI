# Usually for internally usage, initializes and set up access to the 'GDAL' command line interface

Initializes and set up access to the 'GDAL' command line interface

## Usage

``` r
setenvGDAL(bin_GDAL = NULL)
```

## Arguments

- bin_GDAL:

  string contains the path to the 'GDAL' binaries

## Value

Adds 'GDAL' paths to the environment and creates the variable global
string variable `gdalCmd`, that contains the path to the 'GDAL'
binaries.

## Examples

``` r
run = FALSE
if (run) {
## example for the most common default OSGeo4W64 installation of GDAL
setenvGDAL(bin_GDAL = 'C:/OSGeo4W64/bin/',
          root_GDAL = 'C:/OSGeo4W64')
}
```
