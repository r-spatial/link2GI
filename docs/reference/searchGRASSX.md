# Return attributes of valid 'GRASS GIS' installation(s) in 'Linux'

Searches recursively for valid 'GRASS GIS' installations at a given
'Linux' mount point. Returns attributes for each installation.

## Usage

``` r
searchGRASSX(MP = "/usr/bin", quiet = TRUE)
```

## Arguments

- MP:

  default is /usr. This is the directory from which the grass executable
  file is searched, i.e. one executable for each GRASS installation on
  the system.

- quiet:

  boolean. switch for suppressing console messages default is TRUE

## Value

data frame containing 'GRASS GIS' binary folder(s) (i.e. where the
individual GRASS commands are installed), version name(s) and
installation type code(s)

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# get all valid 'GRASS GIS' installation folders in the /usr/bin directory (typical location)
searchGRASSX('/usr/bin')

# get all valid 'GRASS GIS' installation folders in the home directory
searchGRASSX('~/')
} # }
```
