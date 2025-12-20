# Search recursively for valid 'OTB' installation(s) on a 'Linux' OS

Search for valid 'OTB' installations on a 'Linux' OS

## Usage

``` r
searchOTBX(MP = "default", quiet = TRUE)
```

## Arguments

- MP:

  default mount point is the home directory '~' (as suggested by the OTB
  team)

- quiet:

  boolean switch for supressing messages default is TRUE

## Value

A dataframe with the 'OTB' root folder(s) the version name(s) and the
installation type(s).

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# get all valid OTB installation folders and params
searchOTBX()
} # }
```
