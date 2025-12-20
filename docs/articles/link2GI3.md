# GRASS application for real world big data

## Real world example

A typical use case is working with an existing GRASS project database.  
GRASS stores all data in a structured *gisdbase* consisting of locations
and mapsets.

Raster and vector data are fully managed inside this structure.  
This example shows how to integrate large external datasets into GRASS.

### Downloading census data

We use German 2011 census data as a real-world big data example.  
The dataset contains over 35 million grid points with multiple
attributes.

``` sh
# Download census grid data
wget https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip?__blob=publicationFile&v=3
```

Key properties of the dataset:

- Provided in a typical governmental open-data format
- Large enough to demonstrate big data workflows
- Spatially explicit and well suited for raster and vector analysis

``` r
require(link2GI)
require(curl)

# Create project folder structure
dirs <- link2GI::createFolders(
  root_folder = file.path(tempdir(), "link2GI_examples"),
  folders     = c("run/")
)

# Download census data
url <- "https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip?__blob=publicationFile&v=3"
zipfile <- file.path(dirs$path_run, "testdata.zip")
curl::curl_download(url, zipfile)

# Extract CSV file
unzip(
  zipfile,
  files = grep("[.]csv$", unzip(zipfile, list = TRUE)$Name, value = TRUE),
  exdir = dirs$path_run,
  junkpaths = TRUE,
  overwrite = TRUE
)
```

### Preprocessing the data

The dataset is essentially gridded xyz data. Each row represents one 100
m × 100 m grid cell.

``` r
# Fast CSV import
xyz <- data.table::fread(
  file.path(dirs$path_run, "Zensus_Bevoelkerung_100m-Gitter.csv")
)

head(xyz)
```

Because the data is already gridded, rasterization is straightforward.
We only need to identify x, y, and value columns.

``` r
require(RColorBrewer)
require(terra)
require(mapview)

# Detect coordinate and value columns
cn <- names(xyz)
num_cols <- cn[vapply(xyz, is.numeric, logical(1))]
xcol <- num_cols[1]
ycol <- num_cols[2]
vcol <- num_cols[3]

# Build xyz matrix
xyz3 <- xyz[, c(xcol, ycol, vcol)]
names(xyz3) <- c("x", "y", "z")

# Create raster from xyz table
r <- terra::rast(xyz3, type = "xyz")
terra::crs(r) <- "EPSG:3035"

# Visualize raster
p <- colorRampPalette(brewer.pal(8, "Reds"))
mapview::mapviewOptions(mapview.maxpixels = min(2e6, ncell(r)))
mapview::mapview(
  r,
  col.regions = p,
  at = c(-1, 10, 25, 50, 100, 500, 1000, 2500),
  legend = TRUE
)
```

### Setting up a GRASS project

We now create a permanent GRASS location from the raster object.
[`linkGRASS()`](https://r-spatial.github.io/link2GI/reference/linkGRASS.md)
searches for an installed GRASS version and initializes it.

``` r
require(link2GI)

link2GI::linkGRASS(
  x = r,
  epsg = 3035,
  gisdbase = file.path(tempdir(), "link2GI_examples"),
  location = "microzensus2011"
)
```

### Importing raster data into GRASS

The raster is written to GeoTIFF before import. Any GDAL-supported
format would also work.

``` r
require(rgrass)
require(terra)

# Write raster to GeoTIFF
tif_file <- file.path(dirs$path_run, "Zensus_Bevoelkerung_100m-Gitter.tif")
terra::writeRaster(r, tif_file, overwrite = TRUE)

# Register raster as external GRASS dataset
rgrass::execGRASS(
  "r.external",
  flags = "o",
  input  = tif_file,
  output = "Zensus_Bevoelkerung_100m_Gitter",
  band   = "1"
)

# Inspect raster metadata
rgrass::execGRASS("r.info", map = "Zensus_Bevoelkerung_100m_Gitter")
```

### Importing data as GRASS vector points

We first convert the xyz table into an `sf` point object. Each grid cell
becomes a single point feature.

``` r
require(sf)

xyz_sf <- sf::st_as_sf(
  xyz,
  coords = c("x_mp_100m", "y_mp_100m"),
  crs = "EPSG:3035",
  agr = "constant"
)

plot(sf::st_geometry(xyz_sf))
```

The GRASS database already exists. We therefore link to it and import
the vector points.

``` r
require(link2GI)
require(rgrass)

# Reuse existing GRASS location
link2GI::linkGRASS(
  x = xyz_sf,
  epsg = 3035,
  gisdbase = file.path(tempdir(), "link2GI_examples"),
  location = "microzensus2011",
  gisdbase_exist = TRUE
)

# Import sf object as GRASS vector
vname <- "Zensus_Bevoelkerung_100m"
sf2gvec(
  x = xyz_sf,
  obj_name = vname,
  gisdbase = file.path(tempdir(), "link2GI_examples"),
  location = "microzensus2011",
  gisdbase_exist = TRUE
)

# Inspect imported vector data
rgrass::execGRASS("g.list", type = "vector")
rgrass::execGRASS("v.info", map = vname)
```
