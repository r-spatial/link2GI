---
author: "Chris Reudenbach"
title: "linkGRASS real world data usecase"
date: "2024-05-10"
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
  %\VignetteIndexEntry{link2GI - linkGRASS real world data usecase}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---




# Real world example
A typical example is the use of an existing project database in `GRASS`. GRASS organizes all data into an internal file structure known as a gisdbase folder, a mapset, and one or more locations within that mapset. All raster and vector data is stored within this structure and is organized by `GRASS`. So a typical task might be to work on datasets that are already stored in an existing `GRASS` structure.

## Create GRASS project

## Downloading census data

First, we need data from the real world. In this case, it's the 2011 German microcensus data, which, with over 35 million points and several dozen characteristics, can definitely be called Big Data. 

Download the data:

```sh
# (https://www.zensus2011.de/EN/Service/Contact/contact_node.html)
wget https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip?__blob=publicationFile&v=3

```
The presentation of a real example has some interesting aspects:

  - It is provided in a typical government format
  - It is large enough (>35 million points) 
  - It is very informative for many spatial analyses. 

We also need to download the metadata description file from the website mentioned above to get information about projection, data concepts, etc.


```r
 # we need some additional packages
 require(link2GI)
 require(curl)

# first of all we create  a project folder structure 
  dirs = link2GI::createFolders(root_folder = paste0(tempdir(),"/link2GI_examples"), 
                                  folders =  c("run/"))

# set runtime directory
  setwd(dirs$path_run)

# get some typical authority generated data 
  url<-"https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=3"
 res <- curl::curl_download(url, paste0(path_run,"testdata.zip"))

# unzip it
 unzip(res,files = grep(".csv", unzip(res,list = TRUE)$Name,value = TRUE),
       junkpaths = TRUE, overwrite = TRUE)
fn <- list.files(pattern = "[.]csv$", path = getwd(), full.names = TRUE)
```

### Preprocessing the data
After downloading the data, we will use it for some demonstration stuff. If you have a look, the data is nothing but x,y,z with some projection information assumed.




```r
# fast read with data.table 
 xyz <- data.table::fread(paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.csv"))

 head(xyz)
```

We can easily rasterize this data because it is intentionally gridded data. that means we have a value in at a grid size of 100 by 100 meters.



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


### Setting up a GRASS project
Nothing new so far. Now we create a new but permanent `GRASS` gisbase using the spatial parameters from the raster object. As you know, the `linkGRASS` function performs a full search for one or more existing `GRASS` installations. If a valid `GRASS` installation exists, all parameters are set and the `rgrass` package is linked.

Due to the fact that `gisdbase_exist` is set to FALSE by default, it will create a new structure according to the `R` object. 



```r
require(link2GI)
# initialize GRASS and set up a permanent structure  
link2GI::linkGRASS(x = r, 
                    gisdbase = paste0(tempdir(),"/link2GI_examples"),
                    location = "microzensus2011")   
```

Finally, we can import the data into the `GRASS` gisdbase using the functionality of the `rgass` package. 

First we need to convert the raster/terra object into a `GeoTIFF` file. Any `GDAL` format is possible, but `GeoTIFF` is very common and stable.




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

Let's do now the same import as a vector data set. First we create a `sf` object.



```r
 xyz_sf = st_as_sf(xyz,
                    coords = c("x_mp_100m", "y_mp_100m"),
                    crs = 3035,
                    agr = "constant")

#map points
 sf::plot_sf(xyz_sf)
```



The GRASS gisdbase already exists. So we pass `linkGRASS` the argument `gisdbase_exist=TRUE` and import the xyz data as generic GRASS vector points. Note that this will take some time.


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
