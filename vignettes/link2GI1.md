---
author: "Chris Reudenbach"
title: "Link GIS to R"
date: "2019-11-22"
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
  %\VignetteIndexEntry{Link GIS to R}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
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

A comprehensive introduction to the spatial R-biotope and its backgrounds is excellently treated in [Geocomputation with R](https://geocompr.robinlovelace.net) wich is highly recommend as a reference textbook.

Despite all this capabilities of spatial analysis and data handling in the world of `R`, it can be stated (at least from a non-R point of view), that there is still a enormous gap between R and the mature open source Geographic Information System (GIS) and even more Remote Sensing (RS) software community. `QGIS`, `GRASS GIS` and `SAGA GIS` are providing a comprehensive, growing and mature  collection of highly sophisticated algorithms. The provided algorithms are fast, stable and most of them are well proofed. Probably most of the `R` users who are somehow related to the GI community know that there are awesome good wrapper packages for bridging this gap. For [GRASS GIS 7](https://grass.osgeo.org/) it is [rgrass7](https://CRAN.R-project.org/package=rgrass7) and for [SAGA GIS](http://www.saga-gis.org/)  the [RSAGA](https://CRAN.R-project.org/package=RSAGA) package. The development of the [RQGIS](https://CRAN.R-project.org/package=RQGIS) wrapper is the most recent outcome to provide a simple  usage of the powerful [QGIS](https://www.qgis.org/) command line interface.

Unfortunately one will run into a lot of technical problems depending on the choosen operating system (OS) or library dependencies or GIS software versions. In case of e.g. `RSAGA` the main problem has been that the SAGA GIS developers are not only changing the syntax and strategy of the command line interface (CLI) but also within the same release the calls differ from OS to OS. So the maintenance of RSAGA is at least laborious (but thumbs up is running again).  Another example is  given by `GRASS GIS`  which is well known for a sophisticated setup of the environment and the spatial properties of the database. If you "just" want to use a specific GRASS algorithm from R, you will probablys get lost in setting up all OS-dependencies that are neccessary to set up a correct temporary or permanent GRASS-environment from “outside”. This is not only caused due to the strict spatial and projection requirements of GRASS but much more by challenging OS enviroments especially Windows. 

To make it short it is a bit cumbersome to deal with all this stuff if one just want to start e.g. GRASS from the R command line for e.g. a powerful random walk cost analysis (`r.walk`) call as provided by GRASS.


# What means linking?
Linking means simply to provide all necessary environment settings that satisfy the existing wrapper packages as well as in addition the full access to the the command line (CLI) APIs of the mentioned software tools. `link2GI` tries to analyze which software is installed to set up an temporary enviroment meeting the above mentioned needs. 

### GRASS GIS

`GRASS GIS` has the most challenging requirements. It needs a bunch of environment and path variables as **and** a correct setup of the geographical data parameters. The `linkGRASS7` function tries to find all installations let you (optionally) choose the one you want to use and generate the necessary variables. As a result you can use both the rgrass7 package  or the command line `API` of `GRASS`.

### SAGA GIS

`SAGA GIS` is a far easier to set up. Again the `linkSAGA` function tries to find all `SAGA` installations, let you (optionally) choose one and generate the necessary variables. You may also use `RSAGA` but you have to hand over the result of `linkSAGA` like `RSAGA::rsaga.env(path = saga$sagaPath)`. For a straightforward usage you may simply use the  `R` system() call to  interface `R` with the `saga_cmd` API. 

### Orfeo Toolbox (OTB)

The `Orfeo Toolbox` (OTB) is a very powerful remote sensing toolbox. It is widely used for classification, filtering and machine learning applications. You will find some of the implemented algorithm within different R packages but **always** much slower or only running on small data chunks. `link2GI` searches and connects all `OTB` installations of a given search path and provides the result as a clear list.  Due to a missing wrapper package, a list-based `OTB` module and function parser is also available, which can be piped into the function `runOTB` for a convenient function call.

### GDAL
Notwithstanding that `GDAL` is perfectly integrated in R in some cases it is beneficial to use system calls and grab the binaries directly. In particular the evolution to `GDAL 3.x` and optionally various boxed versions of `GDAL` binaries working together with different `Python` and `proj4/proj6` libs makes it sometimes  difficult to grab the correct version of `GDAL`.  `link2GI` generates a list of all pathes and commands of all `GDAL` installation in the provided search path.  With this list, you can easily use all available API calls of each installation. 

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
The most common way to use GRASS is just for one call or algorithm. So the user is not interested in the cumbersome setting up of all parameters. `linGRASS7(georeferenced-dataset)` does an automatic search and find all `GRASS` binaries using the georeferenced-dataset object for spatial referencing and the necessary other settings. 
**NOTE:** This is the highly recommended linking procedure for all on the fly calls of GRASS. Please note also: If more than one `GRASS` installation is found the one with the highest version number is selected automatically. 

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

linkGRASS7(meuse)
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
 
 grass<-linkGRASS7(nc,returnPaths = TRUE)
```
 
 The second most common situation is the usage of an existing GRASS location and project either with existing data sets or manually provided parameters. 


```r
  library(link2GI)
 require(sf)

 # proj folders
 projRootDir<-tempdir()
 paths<-link2GI::initProj(projRootDir = projRootDir,
                          projFolders = c("project1/"))

 # get  data
 nc <- st_read(system.file("shape/nc.shp", package="sf"))

 # CREATE and link to a permanent GRASS folder at "projRootDir", location named "project1"
 linkGRASS7(nc, gisdbase = projRootDir, location = "project1")

 # ONLY LINK to a permanent GRASS folder at "projRootDir", location named "project1"
 linkGRASS7(gisdbase = projRootDir, location = "project1", gisdbase_exist = TRUE )


 # setting up GRASS manually with spatial parameters of the nc data
 proj4_string <- as.character(sp::CRS("+init=epsg:28992"))
 linkGRASS7(spatial_params = c(178605,329714,181390,333611,proj4_string))

 # creating a GRASS gisdbase manually with spatial parameters of the nc data
 # additionally using a peramanent directory "projRootDir" and the location "nc_spatial_params "
 proj4_string <- as.character(sp::CRS("+init=epsg:4267"))
 linkGRASS7(gisdbase = projRootDir,
            location = "nc_spatial_params",
            spatial_params = c(-84.32385, 33.88199,-75.45698,36.58965,proj4_string))
```

 



#### Typical for specified search pathes and OS
 
The full disk search can be cumbersome especially running Windos it can easily take 10 minutes and more. So it is helpful to provide a searchpath for narrowing down the search. Searching for `GRASS installations in the home directory you may use the following command. 


```r
# Link the GRASS installation and define the search location
 linkGRASS7(nc, search_path = "~")
```

If  you already did a full search and kow your installation fo example using the command `findGRASS` you can use the result directly for linking.


```r
findGRASS()
     instDir version installation_type
1 /opt/grass   7.8.1           grass78

# now linking it 
linkGRASS7(nc,c("/opt/grass","7.8.15","grass78")) 

# corresponding linkage running windows
linkGRASS7(nc,c("C:/Program Files/GRASS GIS7.0.5","GRASS GIS 7.0.5","NSIS")) 
```


#### Manual choosing the version
Finally some more specific examples related to interactive selection or OS specific settings.
Choose manually the GRASS installation  additionally using the meuse `sf` object for spatial referencing



```r
linkGRASS7(nc, ver_select = TRUE)
```


#### Creating a permanent gisbase folder

Creating and linking a  permanent `GRASS` gisdbase (folder structure) at "~/temp3" with the standard mapset "PERMANENT"" and the location named "project1". For all spatial attributes use the the meuse `sf` object.




```r
linkGRASS7(x = nc, 
                     gisdbase = "~/temp3",
                     location = "project1")   
```


#### Using a Permanent gisbase folder
Link to the permanent `GRASS` gisdbase (folder structure) at "~/temp3" with the standard mapset "PERMANENT" and the location named "project1". For all spatial attributes use the formerly referencend nc `sf` object parameter.



```r
linkGRASS7(gisdbase = "~/temp3", location = "project1", 
                     gisdbase_exist = TRUE)   
```

#### Manual Setup of the spatial attributes
Setting up `GRASS` manually with spatial parameters of the meuse data



```r
 linkGRASS7(spatial_params = c(178605,329714,181390,333611,
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

Based on the modules of the current version of OTB you can then choose the module(s) you want to use.



```r
## for the example we use the edge detection, 
algoKeyword<- "EdgeExtraction"

## extract the command list for the choosen algorithm 
cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)

## print the current command
print(cmd)
```

Admittedly this is a very straightforward and preliminary approach. Nevertheless it provids you a valid list of all OTB API calls that can easily manipulated for your needs. The following working example will give you an idea how to use it.


















