---
author: "Chris Reudenbach"
date: '2018-08-18'
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
editor_options: 
  chunk_output_type: console
---


# What is link2GI?

The [link2GI](https://CRAN.R-project.org/package=link2GI) package provides a small linking tool to simplify the usage of GRASS and SAGA GIS and Orfeo Toolbox (OTB) for R users. the focus is to simplify the the accessibility of this software for non operating system specialists or highly experienced GIS geeks.  Acutally it is a direct result of numerous graduate courses with R(-GIS) beginners in the hostile world of university computer pools running under extremely restricted Windows systems. 

This vignette:

* shows how to use `link2GI` according to specific system requirements 
* gives some hands on examples of how to use  
* give some applied examples for more efficient spatial analysis 


# Why link2GI now?

R has quite a lot of classes for storing and dealing with spatial data. For vector data the [sp](https://CRAN.R-project.org/package=sp) and recently the great [sf](https://CRAN.R-project.org/package=sf) packages are well known and the raster data world is widely covered by the [raster](https://CRAN.R-project.org/package=raster) package. Additionally external spatial data formats are interfaced by wrapping packages as [rgdal](https://CRAN.R-project.org/package=rgdal) or [gdalUtils](https://CRAN.R-project.org/package=gdalUtils). For more specific links as needed for manipulating atmospheric modeling packages as [ncdf4](https://CRAN.R-project.org/package=ncdf4) are very helpful.

The spatial analysis itself is often supported by wrapper packages that integrate external libraries, command line tools or a mixture of both in an R-like syntax [rgeos](https://CRAN.R-project.org/package=rgeos), [geosphere](https://CRAN.R-project.org/package=geosphere), [Distance](https://CRAN.R-project.org/package=Distance), [maptools](https://CRAN.R-project.org/package=maptools), [igraph](https://CRAN.R-project.org/package=igraph) or [spatstat](https://CRAN.R-project.org/package=spatstat). 

It would be a never ending story to complete this list. 

Despite all this capabilities of spatial analysis and data handling in the world of `R`, it can be stated (at least from a non-R point of view), that there is still a enormous gap between R and the mature open source Geographic Information System (GIS) and even more Remote Sensing (RS) software community. `QGIS`, `GRASS GIS` and `SAGA GIS` are providing a comprehensive, growing and mature  collection of highly sophisticated algorithms. The provided algorithms are fast, stable and most of them are well proofed. Probably most of the `R` users who are somehow related to the GI community know that there are awesome good wrapper packages for bridging this gap. For [GRASS GIS 7](https://grass.osgeo.org/) it is [rgrass7](https://CRAN.R-project.org/package=rgrass7) and for [SAGA GIS](http://www.saga-gis.org/)  the [RSAGA](https://CRAN.R-project.org/package=RSAGA) package. The development of the [RQGIS](https://CRAN.R-project.org/package=RQGIS) wrapper is the most recent outcome to provide a simple  usage of the powerful [QGIS](https://www.qgis.org/) command line interface.

Unfortunately one will run into a lot of technical problems depending on the choosen operating system (OS) or library dependencies or GIS software versions. In case of e.g. `RSAGA` the main problem has been that the SAGA GIS developers are not only changing the syntax and strategy of the command line interface (CLI) but also within the same release the calls differ from OS to OS. So the maintenance of RSAGA is at least laborious (but thumbs up is running again).  Another example is  given by `GRASS GIS`  which is well known for a sophisticated setup of the environment and the spatial properties of the database. If you "just" want to use a specific GRASS algorithm from R, you will probablys get lost in setting up all OS-dependencies that are neccessary to set up a correct temporary or permanent GRASS-environment from “outside”. This is not only caused due to the strict spatial and projection requirements of GRASS but much more by challenging OS enviroments especially Windows. 

To make it short it is a bit cumbersome to deal with all this stuff if one just want to start e.g. GRASS from the R command line for e.g. a powerful random walk cost analysis (`r.walk`) call as provided by GRASS.


# What means linking?
Linking means simply to provide all necessary environment settings that satisfy the existing wrapper packages as well as in addition the full access to the the command line (CLI) APIs of the mentioned software tools. `link2GI` tries to analyze which software is installed to set up an temporary enviroment meeting the above mentioned needs. 

### GRASS GIS

`GRASS GIS` has the most challenging requirements. It needs a bunch of environment and path variables as **and** a correct setup of the geographical data parameters. The `linkGRASS7` function tries to find all installations let you (optionally) choose the one you want to use and generate the necessary variables. As a result you can use both the rgrass7 package  or the command line `API` of `GRASS`.

### SAGA GIS

`SAGA GIS` is a far easier to set up. Again the `linkSAGA` function tries to find all `SAGA` installations, let you (optionally) choose one and generate the necessary variables. You may also use `RSAGA` but you have to hand over the result of `linkSAGA` like `RSAGA::rsaga.env(path = saga$sagaPath)`. For a straightforward usage you may simply use the  `R` system() call to  interface `R` with the `saga_cmd` API. 

### OTB

The `Orfeo Toolbox` (OTB) is a very powerful remote sensing toolbox. It is widely used for classification, filtering and machine learning applications. You will find some of the implemented algorithm within different R packages but **always** much slower or only running on small data chunks. Due to a missing wrapper the linkage is performed to use the command line API of the `OTB`. Currently link2GI provides very basic list-based `OTB` wrapper. 

### GDAL
GDAL is perfectly integrated in R. However in some cases it is beneficial to uses system calls and grab the binaries directly. `link2GI` generates a list of all pathes and commands so you may easily use also python scripts calls and other chains. 

# Usage of the link2GI package - Basic Examples 

## Brute force search usage 
Automatic search and find of the installed GIS software binaries is performed by the `find` functions. Depending of you OS and the number of installed versions you will get a dataframe providing the binary and module folders.



```r
# find all SAGA GIS installations at the default search location
require(link2GI)
saga <- link2GI::findSAGA()
saga
```

Same with GRASS and OTB


```r
# find all SAGA GIS installations at the default search location
require(link2GI)
grass <- link2GI::findGRASS()
grass
otb <- link2GI::findOTB()
otb
```

The `find` functions are providing an overview of the installed software. This functions are not establishing any linkages or changing settings.

## Setting up project structures

If you just call link2GI on the fly , that means for a single temporary operation, there will be no need for setting up folders and project structures. If you work on a more complex project it is seems to be helpful to support this by a fixed structure. Same with existing GRASS projects wich need to be in specific mapsets and locations. 

A straightforward  (you may call it also dirty) approach is the ìnitProj`function that creates folder structures (if not existing) and establishes (if wanted) global variables containing the pathes as strings.

```r
# find all SAGA GIS installations at the default search location
require(link2GI)
link2GI::initProj(projRootDir = tempdir(),
                 projFolders = c("data/",
                                 "data/level0/",
                                 "data/level1/",
                                  "output/",
                                  "run/",
                                  "fun/"),
                 path_prefix = "path_to_" ,
                 global =TRUE)
```

##  linkSAGA - Locate and set up 'SAGA' API bindings
In earlier times it has been pretty cumbersome to link the correct `SAGA GIS` version. Since the version 1.x.x  of `RSAGA` things turned much better. The new `RSAGA::rsaga.env()` function is at getting the first `RSAGA` version in the search path. For using `RSAGA` with `link2GI` it is strongly recommended to call `RSAGA.env()` with the preferred path as provided by a '  `findSAGA()` call. It is also possible to provide the version number as shown below. Storing the result in adequate variables will then even give  the opportunity to easyly switch  between different `SAGA GIS` installations.




```r
saga1<-link2GI::linkSAGA(ver_select = 1) 
saga1
sagaEnv1<- RSAGA::rsaga.env(path = saga1$sagaPath)
```

##  linkGRASS7 - Locate and set up 'GRASS 7' API bindings

`linkGRASS7` Initializes the session environment and the system paths for an easy access to `GRASS GIS 7.x.` The correct setup of the spatial and projection parameters is automatically performed by using either an existing and valid `raster`, `sp` or `sf` object, or manually by providing a list containing the minimum parameters needed. These properties are used to initialize either a temporary or a permanent `rgrass7` environment including the correct 'GRASS 7' database structure. If you provide none of the before mentioned objects `linkGRASS` will create a EPSG:4326 world wide location.

The most time consuming part on 'Windows' Systems is the search process. This can easily take 10 or more minutes. To speed up this process you can also provide a correct parameter set. Best way to do so is to call manually `findGRASS`. Then call `linkGRASS7` with the returned version arguments of your choice.

The function `linkGRASS7` tries to find all valid  `GRASS GIS` binaries by analyzing the startup script files of `GRASS GIS`. After identifying the `GRASS GIS` binaries all necessary system variables and settings will be generated and passed to a temporary `R` environment.

If you have more than one valid installation and run `linkGRASS7` with the  arguments `select_ver = TRUE`, then you will be ask to select one.



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

#  Advanced examples 
A typical example is the usage of an already existing project database in `GRASS`. `GRASS` organizes all data in an internal file structure that is known as gisdbase folder, a mapset and one or more locations within this mapset. All raster and vector data is stored inside this structure and the organisation is performed by `GRASS`. So a typical task could be to work on data sets that are already stored in an existing `GRASS` structure

## Creating a GRASS project

First of all we need some real world data. In this this case the gridded [2011 micro zensus](https://www.zensus2011.de/EN/2011Census/2011_Census_node.html) [population data](https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=3) of Germany. It has some nice aspects:

  - It is provided in a typical authority format
  - It is big enough >35 Mio points 
  - It is pretty instructive for a lot of spatial analysis. 

We also have to download a [meta data description file](https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/Datensatzbeschreibung_Bevoelkerung_100m_Gitter.xlsx;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=2) (excel sheet) for informations about projection and data concepts and so on.



```r
 # we need some additional packages
 require(link2GI)
 require(curl)

# first of all we create  a project folder structure 
  link2GI::initProj(projRootDir = paste0(tempdir(),"/link2GI_examples"), 
                    projFolders =  c("run/"),
                    path_prefix = "path_",
                    global = TRUE)

# set runtime directory
  setwd(path_run)

# get some typical authority generated data 
  url<-"https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=3"
 res <- curl::curl_download(url, paste0(path_run,"testdata.zip"))

# unzip it
 unzip(res,files = grep(".csv", unzip(res,list = TRUE)$Name,value = TRUE),junkpaths = TRUE,overwrite = TRUE)
fn <- list.files(pattern = "[.]csv$", path = path_run, full.names = TRUE)
```

After downloading the data we will use it for some demonstration stuff. If you have a look the data is nothing than x,y,z with assuming some projection information.



```r
# get the filename
 
# fast read with data.table 
 xyz <- data.table::fread(paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.csv"))

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
 mapview::mapviewOptions(mapview.maxpixels = r@ncols*r@nrows/10)
 mapview::mapview(r, col.regions = p, at = c(-1,10,25,50,100,500,1000,2500), legend = TRUE)
```

So far nothing new. Now we create a new but permanent `GRASS` gisbase using the spatial parameters from the raster object. As you know the `linkGRASS7` function performs a full search for one or more than one existing  `GRASS` installations. If a valid `GRASS` installation exists all parameter are setup und the package `rgrass7`  is linked.

Due to the fact that the `gisdbase_exist` is by default set to FALSE it will create a new structure according to the `R` object. 



```r
require(link2GI)
# initialize GRASS and set up a permanent structure  
link2GI::linkGRASS7(x = r, gisdbase = paste0(tempdir(),"/link2GI_examples"),location = "microzensus2011")   
```

Finally we can now import the data to the `GRASS` gisdbase using the `rgass7` package functionality. 

First we must convert the raster object to `GeoTIFF` file. Any `GDAL` format is possible but `GeoTIFF` is very common and stable.



```r
require(raster)
require(rgrass7)

# write it to geotiff
  raster::writeRaster(r, paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"), overwrite = TRUE)

# import raster to GRASS
rgrass7::execGRASS('r.external',
                   flags=c('o',"overwrite","quiet"),
                   input=paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"),
                   output="Zensus_Bevoelkerung_100m_Gitter",
                   band=1
)

# check imported data set
rgrass7::execGRASS('r.info',
                   map = "Zensus_Bevoelkerung_100m_Gitter") 
```
Let's do now the same import as a vector data set. First we create a `sf` object. Please note this will take quite a while.



```r
 xyz_sf = st_as_sf(xyz,
                    coords = c("x_mp_100m", "y_mp_100m"),
                    crs = 3035,
                    agr = "constant")

#map points
 sf::plot_sf(xyz_sf)
```

The `GRASS` gisdbase already exists. So we pass  `linkGRASS7` the argument `gisdbase_exist=TRUE` and import the xyz data as generic GRASS vector points.



```r
 require(sf)
 require(sp)
 require(link2GI)
 
  sf2gvec(x =  xyz_sf,
           obj_name = "Zensus_Bevoelkerung_100m_",
           gisdbase = paste0(tempdir(),"/link2GI_examples"),
           location = "microzensus2011",
           gisdbase_exist = TRUE
          
           )
 
# check imported data set
rgrass7::execGRASS('v.info', map = "Zensus_Bevoelkerung_100m_") 
```








