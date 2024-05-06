---
author: "Chris Reudenbach"
title: "link2GI Basic Examples"
date: "2024-05-06"
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
  %\VignetteIndexEntry{link2GI Basic Examples}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---


# Usage of the link2GI package - Basic Examples 

## Brute force search usage 
Automatic search and find of the installed GIS software binaries is performed by the `find` functions. Depending of you OS and the number of installed versions you will get a dataframe providing the binary and module folders.



```r
# find all SAGA GIS installations at the default search location
require(link2GI)
saga <- link2GI::findSAGA()
saga
```

Same with `GRASS` and `OTB`


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

If you just call link2GI on the fly , that means for a single temporary operation, there will be no need for setting up folders and project structures. If you work on a more complex project it is seems to be helpful to support this by a fixed structure. Same with existing `GRASS` projects wich need to be in specific mapsets and locations. 

A straightforward  (you may call it also dirty) approach is the `createFolders`function that creates folder structures (if not existing) and establishes (if wanted) global variables containing the pathes as strings.


```r
require(link2GI)
envrmt = link2GI::createFolders(root_folder = tempdir(),
                                folders = c("data/",
                                            "data/level0/",
                                            "data/level1/",
                                            "output/",
                                            "run/",
                                            "fun/"),
                                path_prefix = "path")
envrmt
```

##  linkSAGA - Locate and set up 'SAGA' API bindings
In earlier times it has been pretty cumbersome to link the correct `SAGA GIS` version. Since the version 1.x.x  of `RSAGA` things turned much better. The new `RSAGA::rsaga.env()` function is at getting the first `RSAGA` version in the search path. For using `RSAGA` with `link2GI` it is strongly recommended to call `RSAGA.env()` with the preferred path as provided by a '  `findSAGA()` call. It is also possible to provide the version number as shown below. Storing the result in adequate variables will then even give  the opportunity to easyly switch  between different `SAGA GIS` installations.



```r
saga1<-link2GI::linkSAGA(ver_select = 1) 
saga1
sagaEnv1<- RSAGA::rsaga.env(path = saga1$sagaPath)
```

##  linkGRASS - Locate and set up 'GRASS 7/8' API bindings

`linkGRASS` Initializes the session environment and the system paths for an easy access to `GRASS GIS 7.x./8.x` The correct setup of the spatial and projection parameters is automatically performed by using either an existing and valid `raster` or `terra` , `sp` or `sf` object, or manually by providing a list containing the minimum parameters needed. These properties are used to initialize either a temporary or a permanent `rgrass` environment including the correct `GRASS 7/8` database structure. If you provide none of the before mentioned objects `linkGRASS` will create a EPSG:4326 world wide location.

The most time consuming part on 'Windows' Systems is the search process. This can easily take 10 or more minutes. To speed up this process you can also provide a correct parameter set. Best way to do so is to call manually `findGRASS`. Then call `linkGRASS` with the returned version arguments of your choice.

The function `linkGRASS` tries to find all valid  `GRASS GIS` binaries by analyzing the startup script files of `GRASS GIS`. After identifying the `GRASS GIS` binaries all necessary system variables and settings will be generated and passed to a temporary `R` environment.

If you have more than one valid installation and run `linkGRASS` with the  arguments `select_ver = TRUE`, then you will be ask to select one.



#### Standard Full Search Usage 
The most common way to use `GRASS` is just for one call or algorithm. So the user is not interested in the cumbersome setting up of all parameters. `linGRASS7(georeferenced-dataset)` does an automatic search and find all `GRASS` binaries using the georeferenced-dataset object for spatial referencing and the necessary other settings. 
**NOTE:** This is the highly recommended linking procedure for all on the fly calls of `GRASS`. Please note also: If more than one `GRASS` installation is found the one with the highest version number is selected automatically. 

Have a look at the following examples which show a typical call for  the well known `sp`and `sf` vector data objects.

Starting with `sp`.


```r
# get meuse data as sp object and link it temporary to GRASS
require(link2GI)
require(sp)

# get data 
data(meuse) 
# add georeference
coordinates(meuse) <- ~x+y 
proj4string(meuse) <-CRS("+init=epsg:28992") 

# Automatic search and find of GRASS binaries
# using the meuse sp data object for spatial referencing
# This is the highly recommended linking procedure for on the fly jobs
# NOTE: if more than one GRASS installation is found the highest version will be choosed

linkGRASS(meuse)
```
Now do the same with  `sf` based data.


```r
 require(link2GI)
 require(sf)

 # get  data
 nc <- st_read(system.file("shape/nc.shp", package="sf"))

 # Automatic search and find of GRASS binaries
 # using the nc sf data object for spatial referencing
 # This is the highly recommended linking procedure for on the fly jobs
 # NOTE: if more than one GRASS installation is found the highest version will be choosed
 
 grass<-linkGRASS(nc,returnPaths = TRUE)
```
 
 The second most common situation is the usage of an existing `GRASS` location and project either with existing data sets or manually provided parameters. 


```r
  library(link2GI)
 require(sf)

 # proj folders
 root_folder<-tempdir()
 paths<-link2GI::createFolders(root_folder = root_folder,
                          folders = c("project1/"))

 # get  data
 nc <- st_read(system.file("shape/nc.shp", package="sf"))

 # CREATE and link to a permanent GRASS folder at "root_folder", location named "project1"
 linkGRASS(nc, gisdbase = root_folder, location = "project1")

 # ONLY LINK to a permanent GRASS folder at "root_folder", location named "project1"
 linkGRASS(gisdbase = root_folder, location = "project1", gisdbase_exist = TRUE )


 # setting up GRASS manually with spatial parameters of the nc data
 proj4_string <- as.character(sp::CRS("+init=epsg:28992"))
 linkGRASS(spatial_params = c(178605,329714,181390,333611,proj4_string))

 # creating a GRASS gisdbase manually with spatial parameters of the nc data
 # additionally using a peramanent directory "root_folder" and the location "nc_spatial_params "
 proj4_string <- as.character(sp::CRS("+init=epsg:4267"))
 linkGRASS(gisdbase = root_folder,
            location = "nc_spatial_params",
            spatial_params = c(-84.32385, 33.88199,-75.45698,36.58965,proj4_string))
```

 



#### Typical for specified search pathes and OS
 
The full disk search can be cumbersome especially running Windos it can easily take 10 minutes and more. So it is helpful to provide a searchpath for narrowing down the search. Searching for `GRASS` installations in the home directory you may use the following command. 


```r
# Link the GRASS installation and define the search location
 linkGRASS(nc, search_path = "~")
```

If  you already did a full search and kow your installation fo example using the command `findGRASS` you can use the result directly for linking.


```r
findGRASS()
     instDir version installation_type
1 /opt/grass   7.8.1           grass78

# now linking it 
linkGRASS(nc,c("/opt/grass","7.8.15","grass78")) 

# corresponding linkage running windows
linkGRASS(nc,c("C:/Program Files/GRASS GIS7.0.5","GRASS GIS 7.0.5","NSIS")) 
```


#### Manual choosing the version
Finally some more specific examples related to interactive selection or OS specific settings.
Choose manually the `GRASS` installation  additionally using the meuse `sf` object for spatial referencing



```r
linkGRASS(nc, ver_select = TRUE)
```


#### Creating a permanent gisbase folder

Creating and linking a  permanent `GRASS` gisdbase (folder structure) at "~/temp3" with the standard mapset "PERMANENT"" and the location named "project1". For all spatial attributes use the the meuse `sf` object.




```r
linkGRASS(x = nc, 
                     gisdbase = "~/temp3",
                     location = "project1")   
```


#### Using a Permanent gisbase folder
Link to the permanent `GRASS` gisdbase (folder structure) at "~/temp3" with the standard mapset "PERMANENT" and the location named "project1". For all spatial attributes use the formerly referencend nc `sf` object parameter.



```r
linkGRASS(gisdbase = "~/temp3", location = "project1", 
                     gisdbase_exist = TRUE)   
```

#### Manual Setup of the spatial attributes
Setting up `GRASS` manually with spatial parameters of the meuse data



```r
 linkGRASS(spatial_params = c(178605,329714,181390,333611,
                              "+proj=sterea +lat_0=52.15616055555555 
                               +lon_0=5.38763888888889 +k=0.9999079 
                               +x_0=155000 +y_0=463000 +no_defs 
                               +a=6377397.155 +rf=299.1528128
                               +towgs84=565.4171,50.3319,465.5524,
                                -0.398957,0.343988,-1.8774,4.0725
                               +to_meter=1")) 
```

## A typical usecase for the Orfeo Toolbox wrapper
link2GI supports the use of the Orfeo Toolbox with a listbased simple wrapper function. Actually two functions parse the modules and functions syntax dumps and generate a command list that is easy to modify with the necessary arguments.

Usually you have to get the module list first:



```r
# link to the installed OTB 
otblink<-link2GI::linkOTB()


# get the list of modules from the linked version
algo<-parseOTBAlgorithms(gili = otblink)
```

Based on the modules of the current version of `OTB` you can then choose the module(s) you want to use.



```r
## for the example we use the edge detection, 
algoKeyword<- "EdgeExtraction"

## extract the command list for the choosen algorithm 
cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)

## print the current command
print(cmd)
```

Admittedly this is a very straightforward and preliminary approach. Nevertheless it provids you a valid list of all `OTB` API calls that can easily manipulated for your needs. The following working example will give you an idea how to use it.



```r
require(link2GI)
require(terra)
require(listviewer)

otblink<-link2GI::linkOTB()
 root_folder<-tempdir()
 
fn <- system.file("ex/elev.tif", package = "terra")

## for the example we use the edge detection, 
algoKeyword<- "EdgeExtraction"

## extract the command list for the choosen algorithm 
cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)

## get help using the convenient listviewer
listviewer::jsonedit(cmd$help)

## define the mandantory arguments all other will be default
cmd$input  <- fn
cmd$filter <- "touzi"
cmd$channel <- 2
cmd$out <- file.path(root_folder,paste0("out",cmd$filter,".tif"))

## run algorithm
retStack<-runOTB(cmd,gili = otblink)

## plot filter raster on the green channel
plot(retStack)
```

