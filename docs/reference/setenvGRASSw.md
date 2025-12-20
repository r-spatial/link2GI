# Usually for internally usage, create valid 'GRASS GIS 7.xx' rsession environment settings according to the selected GRASS GIS 7.x and Windows Version

Initializes and set up access to 'GRASS GIS 7.xx' via the `rgrass`
wrapper or command line packages. Set and returns all necessary
environment variables and additionally returns the GISBASE directory as
string.

## Usage

``` r
setenvGRASSw(
  root_GRASS = NULL,
  grass_version = NULL,
  installation_type = NULL,
  jpgmem = 1e+06,
  quiet = TRUE
)
```

## Arguments

- root_GRASS:

  grass root directory i.e. 'C:\OSGEO4~1',

- grass_version:

  grass version name i.e. 'grass-7.0.5'

- installation_type:

  two options 'osgeo4w' as installed by the 'OSGeo4W'-installer and
  'NSIS' that is typical for a stand_alone installation of 'GRASS GIS'.

- jpgmem:

  jpeg2000 memory allocation size. Default is 1000000

- quiet:

  boolean switch for suppressing console messages default is TRUE

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# set selected 'GRASS GIS' installation folders 
setenvGRASSw(root_GRASS = 'C:\\PROGRA~1\\QGIS2~1.18',
             grass_version =  'grass-7.2.1',
             installation_type =  'osgeo4W')
} # }
```
