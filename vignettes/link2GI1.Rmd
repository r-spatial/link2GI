---
author: "Chris Reudenbach"
date: '2018-01-28'
output:
  html_document:
    theme: united
    toc: yes
  pdf_document:
    latex_engine: xelatex
    toc: yes
  rmarkdown: default
vignette: >
  %\VignetteIndexEntry{Link GIS to R}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---


# What is link2GI?

The [link2GI](https://CRAN.R-project.org/package=link2GI) package tries to provide an easy entrance door for linking GRASS, SAGA GIS and QGIS as well as other awesome command line tools like the Orfeo Toolbox (OTB) for R users that are not operating system specialists or highly experienced GIS users. It is a result of numerous graduate courses with R-GIS beginners in the hostile world of university computer pools running under restricted Windows systems. It is is sometimes really cumbersome to get all necessary settings in a line. If have to do so on 20 ore so individual Windows Laptops you will get an idea why it could be comfortable to automate this procedure.

This vignette:

* shows how to use `link2GI` according to specific system requirements 
* gives hands on examples of how to use it for exemplary use of the linked software 
* give some examples of how they can be used for meaningful and more efficient spatial analysis


# Why link2GI?

R has quite a lot of classes for storing and dealing with spatial data. For vector data the [sp](https://CRAN.R-project.org/package=sp) and recently the great [sf](https://CRAN.R-project.org/package=sf) packages are well known and the raster data world is widely covered by the [raster](https://CRAN.R-project.org/package=raster) package. Additionally external spatial data formats are interfaced by wrapping packages as [rgdal](https://CRAN.R-project.org/package=rgdal) or [gdalUtils](https://CRAN.R-project.org/package=gdalUtils). For more specific links as needed for manipulating atmospheric modeling packages as [ncdf4](https://CRAN.R-project.org/package=ncdf4) are very helpful.

The spatial analysis itself is often supported by wrapper packages that integrate external libraries, command line tools or a mixture of both in an R-like syntax [rgeos](https://CRAN.R-project.org/package=rgeos), [geosphere](https://CRAN.R-project.org/package=geosphere), [Distance](https://CRAN.R-project.org/package=Distance), [maptools](https://CRAN.R-project.org/package=maptools), [igraph](https://CRAN.R-project.org/package=igraph) or [spatstat](https://CRAN.R-project.org/package=spatstat). However it  would be a never ending story to complete this list. 

Despite all this capabilities of spatial analysis and data handling in the world of R, it can be stated (at least from a non-R point of view), that there is a enormous gap between R and the mature open source Geographic Information System (GIS) and Remote Sensing (RS) software community. QGIS, GRASS GIS and SAGA GIS are providing a comprehensive, growing and mature  collection of highly sophisticated algorithms. In most cases their algorithms are fast, stable and most of them are well proofed. Probably most of the R users that are somehow related to the GI community know that there are some awesome good wrapper packages for bridging this gap. For [GRASS GIS](https://grass.osgeo.org/) it is [rgrass7](https://CRAN.R-project.org/package=rgrass7) and for [SAGA GIS](http://www.saga-gis.org/)  the [RSAGA](https://CRAN.R-project.org/package=RSAGA) package. The development of the [RQGIS](https://CRAN.R-project.org/package=RQGIS) wrapper is the most recent outcome to get a simple access from the R side to the great [QGIS](https://www.qgis.org/) command line interface.

Unfortunately usually one will run into a lot of technical problems due to the choosen operating system  or library dependencies. In case of e.g. `RSAGA` the main problem seems to be the fact that the SAGA GIS developers are not only changing the syntax and strategy of the command line interface (CLI) but also within a release the calls differ from OS to OS. So the maintenance of RSAGA is obviously laborious (but thumbs up is still done). Currently `RSAGA` supports the `SAGA GIS`versions 2.0.4 - 2.2.3. Another example is  given by `GRASS GIS`  which is well known for a sophisticated setup of the environment and the spatial properties of the database. It is not really simple to get an easy working temporary or permanent setup from “outside” due to spatial and projection issues but even more caused by challenging system and environment settings which is  even more complex because of version and Windows OS systems. 

To make it short it is a bit cumbersome to deal with all this stuff if one just want to start e.g. GRASS from the R command line for e.g. a powerful random walk function call.


# What means linking?
Linking means simply to provide all necessary environment settings as well as the full access to the the command line APIs of the mentioned software tools. It tries at  much as possible to bridge the gap between R and these tools. The strategy differs from software to software and OS to OS. 

GRASS GIS has the most challenging requirements. It needs a bunch of environment and path variables as well as a correct setup of the geographical data parameters. The `linkGRASS7` function tries to find all installations let you choose one and generate the necessary variables. As a result you can use both the rgrass7 package  or the command line `API` of `GRASS`.

`SAGA GIS` is a bit easier to set up. The strategy is similar. The `linkSAGA` function tries to find all `SAGA` installations, let you choose one and generate some global variables. You may use `RSAGA` if you run a `SAGA` version of 2.0.4 - 2.2.3. Nevertheless it is strongly recommended to use the  `R` system() call to  interface `R` with the `saga_cmd` API. 

The `Orfeo Toolbox` (OTB) is a very powerful remote sensing toolbox. It is widely used for classification filtering and machine learning applications. You will find a lot of the implemented algorithm within different R packages but *always* much slower or restricted to small data chunks. The linkage is performed similar so that it is easy to use the command line API of the `OTB`. Currently there is no `OTB` wrapper available. 

# Usage of the link2GI package

We will start with `GRASS` .

##  linkGRASS7 - Locate and set up 'GRASS 7' API bindings

`linkGRASS7` Initializes the session environment and the system paths for an easy access to `GRASS GIS 7.x.` The correct setup of the spatial and projection parameters is automatically performed by using either an existing and valid `raster`, `sp` or `sf` object, or manually by providing a list containing the minimum parameters needed. These properties are used to initialize either a temporary or a permanent `rgrass7` environment including the correct 'GRASS 7' database structure. If you provide none of the before mentioned objects `linkGRASS` will create a EPSG:4326 world wide location.

The most time consuming part on 'Windows' Systems is the search process. This can easily take 10 or more minutes. To speed up this process you can also provide a correct parameter set. Best way to do so is to call manually `findGRASS`. Then call `linkGRASS7` with the returned version arguments of your choice.

The function `linkGRASS7` tries to find all valid  `GRASS GIS` binaries by analyzing the startup script files of `GRASS GIS`. After identifying the `GRASS GIS` binaries all necessary system variables and settings will be generated and passed to a temporary `R` environment.

If you have more than one valid installation and run `linkGRASS7` with the  arguments `select_ver = TRUE`, then you will be ask to select one.

### Basic Examples 

#### Standard Full Search Usage 
Automatic search and find of `GRASS` binaries using the meuse sp data object for spatial referencing.
This is the highly recommended linking procedure.  NOTE: if more than one `GRASS` installation is found the first one is selected automatically. 


```r
# get meuse data as sp object
require(link2GI)
require(sp)
data(meuse) 
coordinates(meuse) <- ~x+y 
proj4string(meuse) <-CRS("+init=epsg:28992") 

# get meuse data as sf object
require(sf)
meuse_sf = st_as_sf(meuse, 
                    coords = 
                      c("x", "y"), 
                    crs = 28992, 
                    agr = "constant")

# create a temporary GRASS linkage using the meuse data

linkGRASS7(meuse)
```

#### Typical call for standalone distro
Assuming a typical standalone non-OSGeo4W installation and using the meuse sp data object for spatial referencing



```r
linkGRASS7(meuse,c("C:/Program Files/GRASS GIS7.0.5","GRASS GIS 7.0.5","NSIS")) 

```

#### Typical OSGeo4W64 installation
Typical `OSGeo4W64` installation using the meuse sp data object for spatial referencing


```r
linkGRASS7(meuse,c("C:/OSGeo4W64","grass-7.0.5","osgeo4W"))
```

#### Manual choosing the version
Choose manually the GRASS installation  additionally using the meuse `sf` object for spatial referencing


```r
linkGRASS7(meuse_sf, 
                     ver_select = TRUE)
```

#### Choose another searchpath
Choose manually the GRASS installation and change the search location additionally using the meuse `sf` object for spatial referencing


```r
linkGRASS7(meuse_sf, 
                     search_path = "D:/")

```

#### Creating a permanent gisbase folder

Creating a  permanent `GRASS` gisdbase (folder structure) at "~/temp3" with the standard mapset "PERMANENT"" and the location named "project1". For all spatial attributes use the the meuse `sf` object.


```r
linkGRASS7(x = meuse_sf, 
                     gisdbase = "~/temp3",
                     location = "project1")   
```


#### Using a Permanent gisbase folder
Link to the permanent `GRASS` gisdbase (folder structure) at "~/temp3" with the standard mapset "PERMANENT" and the location named "project1". For all spatial attributes use the the meuse `sf` object.


```r
linkGRASS7(gisdbase = "~/temp3",
                     location = "project1", 
                     gisdbase_exist = TRUE)   
```

#### Manual Setup of the spatial attributes
Setting up `GRASS` manually with spatial parameters of the meuse data


```r
 linkGRASS7(spatial_params = c(178605,329714,181390,333611,"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +no_defs +a=6377397.155 +rf=299.1528128 +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +to_meter=1")) 

```

###  Advanced examples 

#### Creating a GRASS project

First of all we need some real data. In this this case the gridded [2011 micro zensus](https://www.zensus2011.de/EN/2011Census/2011_Census_node.html) [population data](https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=3) of Germany. It has some nice aspects. It is provided in a typical authority format, it is big enough and it is pretty instructive for a lot of spatial analysis. We also have to download a [meta data description file](https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/Datensatzbeschreibung_Bevoelkerung_100m_Gitter.xlsx;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=2) provided as an excel sheet for some informations about projection and data concepts.




```r
 # we need some additional packages

 require(link2GI)
 require(curl)

# first of all we create  a project folder structure 
  link2GI::initProj(projRootDir = "~/link2GI_examples", 
                    projFolders =  c("data/","output/","run/"))

# set runtime directory
  setwd(path_run)

# get some typical authority generated data 
  url<-"https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=3"
 res <- curl::curl_download(url, "testdata.zip")

# unzip it
 unzip(res,files = grep(".csv", unzip(res,list = TRUE)$Name,value = TRUE),junkpaths = TRUE,overwrite = TRUE)
fn <- list.files(pattern = "[.]csv$", path = getwd(), full.names = TRUE)

```

After downloading the data we will use it for some demonstration stuff. If you have a look the data is nothing than x,y,z with assuming some projection information.


```r
# get the filename
 
# fast read with data.table 
 xyz <- data.table::fread(paste0(getwd(),"/Zensus_Bevoelkerung_100m-Gitter.csv"))

 head(xyz)
```

We can easy rasterize this data as it is intentionally gridded data.that means we have in at a grid size of 100 by 100 meters a value.


```r
 require(RColorBrewer)
 require(raster)
 require(mapview)


# clean dataframe
 xyz <- xyz[,-1]

# rasterize it according to the projection 
 r <- raster::rasterFromXYZ(xyz,crs = sp::CRS("+init=epsg:3035"))


# map it
 p <- colorRampPalette(brewer.pal(8, "Reds"))
 # aet resolution to 1 sqkm
 mapview::mapviewOptions(mapview.maxpixels = r@ncols*r@nrows/100)
 mapview::mapview(r, col.regions = p, at = c(-1,10,25,50,100,500,1000,2500), legend = TRUE)

```

So far nothing new. Now we create a new but permanent `GRASS` gisbase using the spatial parameters from the raster object. As you know the `linkGRASS7` function performs a full search for one or more than one existing  `GRASS` installations. If a valid `GRASS` installation exists all parameter are setup und the package `rgrass7`  is linked.

Due to the fact that the `gisdbase_exist` is by default set to FALSE it will create a new structure according to the `R` object. 


```r
require(link2GI)
# initialize GRASS and set up a permanent structure  
link2GI::linkGRASS7(x = r, gisdbase = "~/link2GI_examples",location = "microzensus2011")   
```

Finally we can now import the data to the `GRASS` gisdbase using the `rgass7` package functionality. 

First we must convert the raster object to `GeoTIFF` file. Any `GDAL` format is possible but `GeoTIFF` is very common and stable.


```r
require(raster)
require(rgrass7)

# write it to geotiff
  raster::writeRaster(r, paste0(getwd(),"/Zensus_Bevoelkerung_100m-Gitter.tif"), overwrite = TRUE)

# import raster to GRASS
rgrass7::execGRASS('r.external',
                   flags=c('o',"overwrite","quiet"),
                   input=paste0(getwd(),"/Zensus_Bevoelkerung_100m-Gitter.tif"),
                   output="Zensus_Bevoelkerung_100m_Gitter",
                   band=1
)

# check imported data set
rgrass7::execGRASS('r.info',
                   map = "Zensus_Bevoelkerung_100m_Gitter") 
```
Let's do now the same import as a vector data set. First we create a `sf` object.


```r

 # xyz_sf = st_as_sf(xyz, 
 #                    coords = c("x_mp_100m", "y_mp_100m"), 
 #                    crs = 3035, 
 #                    agr = "constant")

#map points
# sf::plot_sf(xyz_sf)
```

The `GRASS` gisdbase already exists. So we pass  `linkGRASS7` the argument `gisdbase_exist=TRUE` and import the xyz data as generic GRASS vector points.


```r
 require(sf)
 require(sp)
 require(link2GI)
 data(meuse)
 meuse_sf = st_as_sf(meuse, 
                    coords = c("x", "y"), 
                    crs = 28992, 
                    agr = "constant")

 w_gvec(x =  meuse_sf,
           obj_name = "meuse",
           gisdbase = "~/link2GI_examples",
           location = "meuse",
           gisdbase_exist = FALSE
          
           )
 
# check imported data set
rgrass7::execGRASS('v.info', map = "meuse") 
```


#### Working with a GRASS project

A typical example is the usage of an already existing project database in `GRASS`. `GRASS` organizes all data in an internal file structure that is known as gisdbase folder, a mapset and one or more locations within this mapset. All raster and vector data is stored inside this structure and the organisation is performed by `GRASS`. So a typical task could be to work on data sets that are already stored in an existing `GRASS` structure


ToDo
