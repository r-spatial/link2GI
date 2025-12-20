# Locate and set up 'GRASS' API bindings

Initializes the session environment and the system paths for an easy
access to ['GRASS GIS 7.x/8.x'](https://grass.osgeo.org/). The correct
setup of the spatial and projection parameters is automatically
performed by using either an existing and valid `raster`, `terra`, `sp`
or `sf` object, or manually by providing a list containing the minimum
parameters needed.  

## Usage

``` r
linkGRASS(
  x = NULL,
  epsg = NULL,
  default_GRASS = NULL,
  search_path = NULL,
  ver_select = FALSE,
  gisdbase_exist = FALSE,
  gisdbase = NULL,
  use_home = FALSE,
  location = NULL,
  spatial_params = NULL,
  resolution = NULL,
  quiet = TRUE,
  returnPaths = TRUE
)
```

## Arguments

- x:

  raster/terra or sf/sp object

- epsg:

  manual epsg override

- default_GRASS:

  default is `NULL` If is `NULL` an automatic search for all installed
  versions is performed. If you provide a valid list the corresponding
  version is initialized. An example for OSGeo4W64 is:
  `c('C:/OSGeo4W64','grass-7.0.5','osgeo4w')`

- search_path:

  Path or mount point to search for.

- ver_select:

  Boolean if TRUE you may choose interactively the binary version (if
  found more than one), by default FALSE

- gisdbase_exist:

  default is FALSE if set to TRUE the arguments gisdbase and location
  are expected to be an existing GRASS gisdbase

- gisdbase:

  default is `NULL`, invoke
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) to the 'GRASS'
  database. Alternatively you can provide a individual path.

- use_home:

  default is `FALSE`, set the GISRC path to tempdir(), if TRUE the HOME
  or USERPROFILE setting is used for writing the GISRC file

- location:

  default is `NULL`, invoke `basename(tempfile())` for defining the
  'GRASS' location. Alternatively you can provide a individual path.

- spatial_params:

  default is `NULL`. Instead of a spatial object you may provide the
  geometry as a list. E.g. c(xmin,ymin,xmax,ymax,proj4_string)

- resolution:

  resolution in map units for the GRASS raster cells

- quiet:

  Boolean switch for suppressing console messages default is TRUE

- returnPaths:

  Boolean if set to FALSE the paths of the selected version are written
  to the PATH variable only, otherwise all paths and versions of the
  installed GRASS versions ae returned.

## Note

GRASS GIS is excellently supported by the `rgrass` wrapper package.
Nevertheless, 'GRASS GIS' is known for its its high demands on the
correct spatial and reference setup and environment requirements. This
becomes even worse on 'Windows platforms or when there are several
alternative 'GRASS GIS' installations available. If you know how to use
the `rgrass` package setup function
[`rgrass::initGRASS`](https://osgeo.github.io/rgrass/reference/initGRASS.html)
works fine on Linux. This is also true for known configurations under
the 'Windows' operating system. However, on university labs or corporate
machines with limited privileges and/or different releases such as the
['OSGeo4W'](https://trac.osgeo.org/osgeo4w/) distribution and the
['GRASS' stand-alone](https://grass.osgeo.org/download/windows/)
installation, or different software releases (e.g. 'GRASS 7.0.5 and
GRASS 8.1.0), it often becomes inconvenient or even to get the correct
links.  
The function `linkGRASS` tries to find all valid 'GRASS GIS' binaries by
\#' analyzing the startup script files. GRASS GIS' startup script files.
After identifying the 'GRASS GIS' binaries, all \#' necessary system
variables and settings are system variables and settings are generated
and passed to a temporary R environment. The concept is simple, but
helpful for everyday use. You need to either provide a `raster` or `sp`
`sf` spatial object that has the correct spatial and projection
properties, or you can link directly to an existing 'GRASS' gisdbase and
mapset. If you choose a spatial object to initialize a correct 'GRASS'
mapset, it will be used to create either a temporary or permanent
mapset. [rgrass](https://CRAN.R-project.org/package=rgrass) environment
with the correct 'GRASS' structure.  
  
The most time consuming part on Windows systems is the search process.
This can easily take 10 minutes or more. To speed up this process, you
can also provide a correct parameter set. The best way to do this is to
manually call `searchGRASSW` or for 'Linux' `searchGRASSX`. and call
`linkGRASS` with the version arguments of your choice. linkGRASS will
initialize the use of GRASS. If you have more than one valid
installation and call `linkGRASS()` without arguments, you will be asked
to select one.

## Author

Chris Reudenbach

## Examples

``` r
run = FALSE
if (run) {
library(link2GI)
require(sf)

# get  data                         
nc = st_read(system.file('shape/nc.shp', package='sf'))
# Automatic linking of GRASS binaries using the nc data object for spatial referencing
# This is the best practice linking procedure for on-the-fly jobs.
# NOTE: If more than one GRASS installation is found, you will have to select one. 
grass = linkGRASS(nc)

# Select the GRASS installation (if more than one)
linkGRASS(nc, ver_select = TRUE)

# Select the GRASS installation and define the search location
linkGRASS(nc, ver_select = TRUE, search_path = '~/')

# Set up GRASS manually with spatial parameters of the nc data
epsg = 28992
proj4_string <- sp::CRS(paste0('+init=epsg:',epsg))

linkGRASS(spatial_params = c(178605,329714,181390,333611,proj4_string@projargs),epsg=epsg)


# create some temporary project folders for a permanent gisdbase
root_folder = tempdir()
grass_path = link2GI::createFolder(root_folder = root_folder,
                         folders = c('project1/'))
if (grass$exist){
# CREATE and link to a permanent GRASS folder at 'root_folder', location named 'project1' 
linkGRASS(nc, gisdbase = root_folder, location = 'project1')   

# ONLY LINK to a permanent GRASS folder in 'root_folder', location named 'project1' 
linkGRASS(gisdbase = root_folder, location = 'project1', gisdbase_exist = TRUE )   

# Manual creation of a GRASS gisdbase with the spatial parameters of the NC data. 
# additional use of a permanent directory 'root_folder' and the location 'nc_spatial_params'.
epsg = 4267
proj4_string = sp::CRS(paste0('+init=epsg:',epsg))
linkGRASS(gisdbase = root_folder,
           location = 'nc_spatial_params',
           spatial_params = c(-84.32385, 33.88199,-75.45698,36.58965,proj4_string),epsg = epsg)
}
}
```
