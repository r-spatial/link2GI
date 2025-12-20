# Searches recursively for existing 'Windows' 'SAGA GIS' installation(s)

Search for valid 'GRASS GIS' installations at a given 'Linux' mount
point

## Usage

``` r
searchSAGAX(MP = "/usr/bin", quiet = TRUE)
```

## Arguments

- MP:

  default mount point is `/usr/bin`

- quiet:

  Boolean switch for suppressing console messages default is TRUE

## Value

A data frame containing the 'SAGA GIS' root folder(s), the version
name(s) and the installation type(s)

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
#### Examples how to use searchSAGAX

# get all valid SAGA installation folders and params
searchSAGAX()
} # }
```
