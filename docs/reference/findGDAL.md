# Search recursivly existing 'GDAL binaries' installation(s) at a given drive/mountpoint

Provides an list of valid 'GDAL' installation(s) on your 'Windows'
system. There is a major difference between osgeo4W and stand_alone
installations. The functions trys to find all valid installations by
analysing the calling batch scripts.

## Usage

``` r
findGDAL(searchLocation = "default", quiet = TRUE)
```

## Arguments

- searchLocation:

  drive letter to be searched, for Windows systems default is `C:/`, for
  Linux systems default is `/usr/bin`.

- quiet:

  boolean switch for supressing console messages default is TRUE

## Value

A dataframe with the 'GDAL' root folder(s), and command line
executable(s)

## Author

Chris Reudenbach

## Examples

``` r
run = FALSE
if (run) {
# find recursively all existing 'GDAL' installations folders starting 
# at the default search location
findGDAL()
}
```
