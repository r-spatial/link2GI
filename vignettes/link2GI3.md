---
author: "Chris Reudenbach"
title: "linkGRASS real world data usecase"
date: "2022-09-04"
editor_options:
  chunk_output_type: console
output:
  html_document: 
    theme: united
    toc: yes
  rmarkdown: default
  pdf_document:
    latex_engine: xelatex
    toc: yes
urlcolor: blue
vignette: >
  %\VignetteIndexEntry{linkGRASS real world data usecase}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---


#  Real world example
A typical example is the usage of an already existing project database in `GRASS`. `GRASS` organizes all data in an internal file structure that is known as gisdbase folder, a mapset and one or more locations within this mapset. All raster and vector data is stored inside this structure and the organisation is performed by `GRASS`. So a typical task could be to work on data sets that are already stored in an existing `GRASS` structure

## Creating a GRASS project

## Download Zensus Data
First of all we need some real world data. In this this case the gridded German 2011 micro zensus data (see https://www.zensus2011.de/EN/Home/). *NOTE:* This is a real big data file organised as an `csv` ASCII file. You may download the data e.g. using `wget`:

```sh
wget https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip?__blob=publicationFile&v=3

```
The data set has some nice aspects:

  - it is provided in a typical (means ineffective and weird) authority format
  - it is big enough (>35 Mio points) to be a challenge
  - it is pretty instructive for a lot of spatial analysis. 

We also have to download the meta data description file from the above website for information about projection, data concepts and so on.



```r
 # we need some additional packages
 require(link2GI)
 require(curl)


# first of all we create  a project folder structure 
ggis_fn = paste0(tempdir(),"/link2GI_examples")
  link2GI::initProj(projRootDir = ggis_fn, 
                    projFolders =  c("run/"),
                    path_prefix = "path_",
                    global = TRUE)

# set runtime directory
  setwd(path_run)

# get some typical authority generated data 
  url<-"https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=3"
 res <- curl::curl_download(url, paste0(path_run,"testdata.zip"))

# unzip it
 unzip(res,files = grep(".csv", unzip(res,list = TRUE)$Name,value = TRUE),
       junkpaths = TRUE, overwrite = TRUE)

 # get the filename 
fn <- list.files(pattern = "[.]csv$", path = getwd(), full.names = TRUE)
```

### Preprocessing of the data
After downloading the data we will use it for some demonstration stuff. If you have a look the data is nothing than x,y,z with assuming some projection information. Actually the projection of the data set is EPSG 3035. We will change it to EPSG 4313.

We can easily rasterize this data as it is intentionally gridded data.that means we have in at a grid size of 100 by 100 meters a value.



```r
 require(RColorBrewer)
 require(stars)
 require(terra)
 require(sf)
# fast read with data.table 
 xyz <- data.table::fread(paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.csv"))
 
 head(xyz)

# clean dataframe
 xyz <- xyz[,-1]

# rasterize it according to the projection 
 r =	stars::st_as_stars(terra::rast(xyz,type = "xyz",crs= sf::st_crs(3035)$wkt))
 
# reprojection to DHDN/Bessel system using st_warp for a regular grid (https://epsg.io/4314)
# r2 = stars::st_warp(r, crs = sf::st_crs(4314))

# map it
 p <- colorRampPalette(RColorBrewer::brewer.pal(9, "OrRd"))

# resolution is downsampled to 1 sqkm
 tmap::tm_shape(r)+
 tmap::tm_raster("Einwohner", breaks = c(-1,0,1,5,10,50,100,200,300,Inf),  
		palette = p(9), title="Residents/ha", midpoint = NA) 
```

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/residents.png)
### Setup GRASS Project
So far nothing new. Now we create a new but permanent `GRASS` gisbase using the spatial parameters from the raster object. As you know the `linkGRASS` function performs a full search for one or more than one existing  `GRASS` installations. If a valid `GRASS` installation exists all parameter are setup und the package `rgrass`  is linked.

Due to the fact that the `gisdbase_exist` is by default set to FALSE it will create a new structure according to the `R` object. 



```r
require(link2GI)
require(sf)
# initialize GRASS and set up a permanent structure  
link2GI::linkGRASS(x = rast(r), 
                    gisdbase = ggis_fn,
                    location = "microzensus2011")   
```

Finally we can now import the data to the `GRASS` gisdbase using the `rgass` package functionality. 

First we must save the raster/terra object to a `GeoTIFF` file. Any `GDAL` format is possible but `GeoTIFF` is very common and stable.



```r
require(link2GI)
require(stars)
require(rgrass)

# write it to geotiff
stars::write_stars(r, paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"), 
                      overwrite = TRUE)

# import raster to GRASS
rgrass::execGRASS('r.external',
                   flags=c('a','o',"overwrite","quiet"),
                   input=paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"),
                   output="Zensus_Bevoelkerung_100m_Gitter",
                   band=1)

# check imported data set
rgrass::execGRASS('r.info',
                   map = "Zensus_Bevoelkerung_100m_Gitter") 
```

Let's do now the same import as a vector data set. First we create and reproject from the original table a `sf` object. Please note this will take quite a while.



```r
 xyz_sf = sf::st_as_sf(xyz,
                    coords = c("x_mp_100m", "y_mp_100m"),
                    crs = 3035,
                    agr = "constant")
 sf_crs(xyz_sf)  = sf::st_crs(xyz_sf)$wkt
#xyz_sf = sf::st_transform(xyz_sf,4314)
```



The `GRASS` gisdbase already exists. So we pass  `linkGRASS` the argument `gisdbase_exist=TRUE` and import the xyz data as generic GRASS vector points.



```r
 require(rgrass)
 require(terra)
# import point data to GRASS via gpgk and rgrass

  rgrass::write_VECT(terra::vect(xyz_sf),
                     flags = c("o","overwrite"),
                     vname = "Bevoelkerung100m_gpgk",
                     ignore.stderr = FALSE), 


# import point vector vector via sqlite 

sf2gvec(x = xyz_sf,
        obj_name = "Bevoelkerung100m-",
        gisdbase = ggis_fn,
        location = "microzensus2011",
        epsg = 3035,
        gisdbase_exist = TRUE)
```


```r
# microbenchmark  
 sf2gvec     write_VECT
 mean        mean   
 1163.185    1930.099
```


```r
# check imported data set
rgrass::execGRASS('v.info', map = "bevoelkerung100m_sqlite@PERMANENT") 
```
