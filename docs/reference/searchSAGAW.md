# Searches recursively for existing 'Windows' 'SAGA GIS' installation(s)

Searches recursively for existing 'SAGA GIS' installation(s) on a given
'Windows' drive

## Usage

``` r
searchSAGAW(DL = "C:/", quiet = TRUE)
```

## Arguments

- DL:

  drive letter default is `C:/`

- quiet:

  boolean switch for suppressing messages default is TRUE

## Value

A data frame containing the 'SAGA GIS' root folder(s), the version
name(s) and the installation type(s)

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
#### Examples how to use searchSAGAW 

# get all valid SAGA installation folders and params
searchSAGAW()
} # }
```
