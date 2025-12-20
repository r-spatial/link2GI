# Locate and set up 'GDAL' API bindings

Locate and set up ['GDAL - Geospatial Data Abstraction
Librar'](https://gdal.org/) API bindings

## Usage

``` r
linkGDAL(
  bin_GDAL = NULL,
  searchLocation = NULL,
  ver_select = FALSE,
  quiet = TRUE,
  returnPaths = TRUE
)
```

## Arguments

- bin_GDAL:

  string contains path to where the gdal binaries are located

- searchLocation:

  string hard drive letter default is `C:/`

- ver_select:

  Boolean default is FALSE. If there is more than one 'GDAL'
  installation and `ver_select` = TRUE the user can select interactively
  the preferred 'GDAL' version

- quiet:

  Boolean switch for suppressing messages default is TRUE

- returnPaths:

  Boolean if set to FALSE the paths of the selected version are written
  to the PATH variable only, otherwise all paths and versions of the
  installed GRASS versions ae returned.

## Value

add gdal paths to the environment and creates global variables path_GDAL

## Details

It looks for the `gdalinfo(.exe)` file. If the file is found in a `bin`
folder it is assumed to be a valid 'GDAL' binary installation.

if called without any parameter `linkGDAL()` it performs a full search
over the hard drive `C:`. If it finds one or more 'GDAL' binaries it
will take the first hit. You have to set `ver_select = TRUE` for an
interactive selection of the preferred version.

## Note

You may also set the path manually. Using a 'OSGeo4W64'
<https://trac.osgeo.org/osgeo4w/> installation it is typically
`C:/OSGeo4W64/bin/`

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# call if you do not have any idea if and where GDAL is installed
gdal<-linkGDAL()
if (gdal$exist) {
# call it for a default OSGeo4W installation of the GDAL
print(gdal)
}
} # }
```
