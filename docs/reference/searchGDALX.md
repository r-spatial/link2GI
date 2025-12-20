# Search recursively for valid 'GDAL' installation(s) on a 'X-based' OS

Search for valid 'GDAL' installations on a 'X-based' OS

## Usage

``` r
searchGDALX(MP = "/usr/bin", quiet = TRUE)
```

## Arguments

- MP:

  drive letter default is '/usr/bin'

- quiet:

  boolean switch for supressing messages default is TRUE

## Value

A dataframe with the 'GDAL' root folder(s) the version name(s) and the
installation type(s).

## Author

Chris Reudenbach

## Examples

``` r
run = FALSE
if (run) {
# get all valid GDAL installation folders and params
searchGDALX()
}
```
