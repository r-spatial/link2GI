# Search recursively existing 'SAGA GIS' installation(s) at a given drive/mount point

Provides an list of valid 'SAGA GIS' installation(s) on your 'Windows'
system. There is a major difference between osgeo4W and stand_alone
installations. The functions tries to find all valid installations by
analyzing the calling batch scripts.

## Usage

``` r
findSAGA(searchLocation = "default", quiet = TRUE)
```

## Arguments

- searchLocation:

  drive letter to be searched, for Windows systems default is `C:/`, for
  Linux systems default is `/usr/bin`.

- quiet:

  boolean switch for suppressing console messages default is TRUE

## Value

A dataframe with the 'SAGA GIS' root folder(s), version name(s) and
installation type code(s)

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# find recursively all existing 'SAGA GIS' installation folders starting 
# at the default search location
findSAGA()
} # }
```
