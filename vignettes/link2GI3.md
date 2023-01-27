---
author: "Chris Reudenbach"
title: "linkGRASS real world data usecase"
date: "2023-01-27"
editor_options:
  chunk_output_type: console
output:
  html_document: 
    theme: united
    toc: yes
  rmarkdown: default
   rmarkdown::html_vignette:
    md_extensions: [ 
      "-autolink_bare_uris" 
    ]
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
First of all we need some real world data. In this this case the gridded German 2011 micro zensus data. Download the data:

```sh
# (https://www.zensus2011.de/EN/Service/Contact/contact_node.html)
wget https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip?__blob=publicationFile&v=3

```
of Germany. It has some nice aspects:

  - It is provided in a typical authority format
  - It is big enough >35 Mio points 
  - It is pretty instructive for a lot of spatial analysis. 

We also have to download the meta data description file from the above website for informations about projection and data concepts and so on.



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
 unzip(res,files = grep(".csv", unzip(res,list = TRUE)$Name,value = TRUE),
       junkpaths = TRUE, overwrite = TRUE)
fn <- list.files(pattern = "[.]csv$", path = getwd(), full.names = TRUE)
```

### Preprocessing of the data
After downloading the data we will use it for some demonstration stuff. If you have a look the data is nothing than x,y,z with assuming some projection information.



```r
# fast read with data.table 
 xyz <- data.table::fread(paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.csv"))

 head(xyz)
```

We can easy rasterize this data as it is intentionally gridded data.that means we have in at a grid size of 100 by 100 meters a value.



```r
 require(RColorBrewer)
 require(terra)
 require(mapview)


# clean dataframe
 xyz <- xyz[,-1]

# rasterize it according to the projection 
  r <- terra::rast(xyz, type="xyz")
 terra::crs(r) <- 3035

# map it
 p <- colorRampPalette(brewer.pal(8, "Reds"))
 # aet resolution to 1 sqkm
 mapview::mapviewOptions(mapview.maxpixels = r@ncols*r@nrows/10)
 mapview::mapview(r, col.regions = p, 
                  at = c(-1,10,25,50,100,500,1000,2500), 
                  legend = TRUE)
```


### Setup GRASS Project
So far nothing new. Now we create a new but permanent `GRASS` gisbase using the spatial parameters from the raster object. As you know the `linkGRASS` function performs a full search for one or more than one existing  `GRASS` installations. If a valid `GRASS` installation exists all parameter are setup und the package `rgrass`  is linked.

Due to the fact that the `gisdbase_exist` is by default set to FALSE it will create a new structure according to the `R` object. 



```r
require(link2GI)
# initialize GRASS and set up a permanent structure  
link2GI::linkGRASS(x = r, 
                    gisdbase = paste0(tempdir(),"/link2GI_examples"),
                    location = "microzensus2011")   
```

Finally we can now import the data to the `GRASS` gisdbase using the `rgass` package functionality. 

First we must convert the raster/terra object to a `GeoTIFF` file. Any `GDAL` format is possible but `GeoTIFF` is very common and stable.



```r
require(link2GI)
require(raster)
require(rgrass)

# write it to geotiff
  terra::writeRaster(r, paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"), 
       x               overwrite = TRUE)

# import raster to GRASS
rgrass::execGRASS('r.external',
                   flags=c('o',"overwrite","quiet"),
                   input=paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"),
                   output="Zensus_Bevoelkerung_100m_Gitter",
                   band=1)

# check imported data set
rgrass::execGRASS('r.info',
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



The `GRASS` gisdbase already exists. So we pass  `linkGRASS` the argument `gisdbase_exist=TRUE` and import the xyz data as generic GRASS vector points.



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
rgrass::execGRASS('v.info', map = "Zensus_Bevoelkerung_100m_") 
```
