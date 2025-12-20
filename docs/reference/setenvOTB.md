# Usually for internally usage, initializes and set up access to the 'OTB' command line interface

Initializes and set up access to the 'OTB' command line interface

## Usage

``` r
setenvOTB(bin_OTB = NULL, root_OTB = NULL)
```

## Arguments

- bin_OTB:

  string contains the path to the 'OTB' binaries

- root_OTB:

  string contains the full string to the root folder containing the
  'OTB' installation'

## Value

Adds 'OTB' paths to the environment and creates the variable global
string variable `otbCmd`, that contains the path to the 'OTB' binaries.

## Examples

``` r
if (FALSE) { # \dontrun{
## example for the most common default OSGeo4W64 installation of OTB
setenvOTB(bin_OTB = 'C:\\OSGeo4W64\\bin\\',
          root_OTB = 'C:\\OSGeo4W64')
} # }
```
