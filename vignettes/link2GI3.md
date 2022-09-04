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















