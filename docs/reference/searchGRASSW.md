# Search recursivly valid 'GRASS GIS' installation(s) on a given 'Windows' drive

Provides an list of valid 'GRASS GIS' installation(s) on your 'Windows'
system. There is a major difference between osgeo4W and stand_alone
installations. The functions trys to find all valid installations by
analysing the calling batch scripts.

## Usage

``` r
searchGRASSW(DL = "C:/", quiet = TRUE)
```

## Arguments

- DL:

  drive letter to be searched, default is `C:/`

- quiet:

  boolean switch for supressing console messages default is TRUEs

## Value

A dataframe with the 'GRASS GIS' root folder(s), version name(s) and
installation type code(s)

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# get all valid 'GRASS GIS' installation folders and params at 'C:/'
searchGRASSW()
} # }
```
