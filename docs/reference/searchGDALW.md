# Search recursively for valid 'GDAL' installation(s) on a 'Windows' OS

Search for valid 'GDAL' installations on a 'Windows' OS

## Usage

``` r
searchGDALW(DL = "C:/", quiet = TRUE)
```

## Arguments

- DL:

  drive letter default is 'C:/'

- quiet:

  boolean switch for supressing console messages default is TRUE

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
searchGDALW()
}
```
