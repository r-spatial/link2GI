# Search recursively for valid 'OTB' installation(s) on a 'Windows' OS

Search for valid 'OTB' installations on a 'Windows' OS

## Usage

``` r
searchOTBW(DL = "default", quiet = TRUE)
```

## Arguments

- DL:

  drive letter default is `C:/`

- quiet:

  boolean switch for supressing console messages default is TRUE

## Value

A dataframe with the 'OTB' root folder(s) the version name(s) and the
installation type(s).

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# get all valid OTB installation folders and params
searchOTBW()
} # }
```
