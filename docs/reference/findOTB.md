# Search recursivly existing 'Orfeo Toolbox' installation(s) at a given drive/mountpoint

Provides an list of valid 'OTB' installation(s) on your 'Windows'
system. There is a major difference between osgeo4W and stand_alone
installations. The functions trys to find all valid installations by
analysing the calling batch scripts.

## Usage

``` r
findOTB(searchLocation = "default", quiet = TRUE)
```

## Arguments

- searchLocation:

  drive letter to be searched, for Windows systems default is `C:/`, for
  Linux systems default is `/usr/bin`.

- quiet:

  boolean switch for supressing console messages default is TRUE

## Value

A dataframe with the 'OTB' root folder(s), and command line
executable(s)

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# find recursively all existing 'Orfeo Toolbox' installations folders starting 
# at the default search location
findOTB()
} # }
```
