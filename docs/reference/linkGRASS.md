# Locate and set up GRASS GIS API bindings

Initializes a GRASS GIS 7.x/8.x runtime environment and prepares a valid
temporary or permanent GRASS location and mapset for use from R.

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

  A spatial object used to initialize the GRASS location. Supported
  classes are \`terra::SpatRaster\`, \`sf\`, \`sp\`, \`stars\`, or a
  file path to a raster dataset.

- epsg:

  Integer EPSG code used to define the GRASS projection. If \`NULL\`,
  the EPSG code is inferred from \`x\` when possible.

- default_GRASS:

  Optional character vector defining a GRASS installation (e.g.
  \`c("/usr/lib/grass83", "8.3.2", "grass")\`).

- search_path:

  Character path used to search for GRASS installations.

- ver_select:

  Logical or numeric value controlling interactive or indexed GRASS
  version selection.

- gisdbase_exist:

  Logical; if \`TRUE\`, \`gisdbase\` and \`location\` must already exist
  and will only be linked.

- gisdbase:

  Path to the GRASS database directory.

- use_home:

  Logical; if \`TRUE\`, the user home directory is used for GISRC.

- location:

  Name of the GRASS location to create or link.

- spatial_params:

  Optional numeric vector defining extent manually (\`xmin, ymin, xmax,
  ymax\[, proj\]\`).

- resolution:

  Numeric raster resolution used for \`g.region\`.

- quiet:

  Logical; suppress console output if \`TRUE\`.

- returnPaths:

  Logical; return detected GRASS installation paths.

## Value

A list describing the selected GRASS installation and status, or
\`NULL\` if no valid installation was found.

## Details

The function detects installed GRASS versions, initializes required
environment variables, and derives spatial reference information either
from an existing spatial object or from manually provided parameters.

GRASS requires a fully initialized runtime environment (PATH, GISBASE,
PROJ, GDAL). On some platforms, R must be started from a shell where
GRASS is already available.

The function ensures that \`PROJ_INFO\` and \`PROJ_UNITS\` are written
by explicitly calling \`g.proj\` before region initialization.

## See also

[`initGRASS`](https://osgeo.github.io/rgrass/reference/initGRASS.html),
[`execGRASS`](https://osgeo.github.io/rgrass/reference/execGRASS.html)

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
library(link2GI)
library(sf)

# Example 1: initialize a temporary GRASS location from an sf object
nc <- st_read(system.file("shape/nc.shp", package = "sf"))
grass <- linkGRASS(nc)

# Example 2: select GRASS version interactively if multiple installations exist
linkGRASS(nc, ver_select = TRUE)

# Example 3: create a permanent GRASS location
root <- tempdir()
linkGRASS(
  x        = nc,
  gisdbase = root,
  location = "project1"
)

# Example 4: link to an existing GRASS location without recreating it
linkGRASS(
  gisdbase        = root,
  location        = "project1",
  gisdbase_exist  = TRUE
)

# Example 5: manual setup using spatial parameters only
epsg <- 28992
linkGRASS(
  spatial_params = c(178605, 329714, 181390, 333611),
  epsg = epsg
)
} # }
```
