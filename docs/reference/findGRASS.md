# Returns attributes of valid 'GRASS GIS' installation(s) on the system.

Retrieve a list of valid 'GRASS GIS' installation(s) on your system.
There is a big difference between osgeo4W and stand_alone installations.
The function tries to find all valid installations by analyzing the
calling batch scripts.

## Usage

``` r
findGRASS(searchLocation = "default", ver_select = FALSE, quiet = TRUE)
```

## Arguments

- searchLocation:

  Location to search for the grass executable, i.e. one executable for
  each GRASS installation on the system. For Windows systems it is
  mandatory to include an uppercase Windows drive letter and a colon.
  Default for Windows systems is `C:/`, for Linux systems the default is
  `/usr/bin`.

- ver_select:

  boolean, Default is FALSE. If there is more than one 'GRASS GIS'
  installation and `ver_select` = TRUE, the user can interactively
  select the preferred 'GRASS GIS' version.

- quiet:

  boolean, default is TRUE. switch to suppress console messages

## Value

data frame with the 'GRASS GIS' binary folder(s) (i.e. where the
individual individual GRASS commands are installed), version name(s) and
installation type code(s)

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# find recursively all existing 'GRASS GIS' installation folders starting 
# at the default search location
findGRASS()
} # }
```
