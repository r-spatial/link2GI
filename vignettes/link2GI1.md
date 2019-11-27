---
author: "Chris Reudenbach"
title: "Link GI to R"
date: "2019-11-27"
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
  %\VignetteIndexEntry{Link GI to R}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---


# What is link2GI?

The [link2GI](https://CRAN.R-project.org/package=link2GI) package provides a small linking tool to simplify the usage of `GRASS GIS`, `SAGA GIS`, `Orfeo Toolbox` (`OTB`) and `GDAL` binaries for R users. the focus is to simplify the the accessibility of this software for non operating system specialists or highly experienced GIS geeks.  Acutally it is a direct result of numerous graduate courses with R(-GIS) beginners in the hostile world of university computer pools running under extremely restricted Windows systems. 

This vignette:

* shows how to use `link2GI` according to specific system requirements 
* gives some hands on examples of how to use  
* give some applied examples for more efficient spatial analysis 


# Why link2GI now?

R has quite a lot of classes for storing and dealing with spatial data. For vector data the [sp](https://CRAN.R-project.org/package=sp) and recently the great [sf](https://CRAN.R-project.org/package=sf) packages are well known and the raster data world is widely covered by the [raster](https://CRAN.R-project.org/package=raster) package. Additionally external spatial data formats are interfaced by wrapping packages as [rgdal](https://CRAN.R-project.org/package=rgdal) or [gdalUtils](https://CRAN.R-project.org/package=gdalUtils). For more specific links as needed for manipulating atmospheric modeling packages as [ncdf4](https://CRAN.R-project.org/package=ncdf4) are very helpful.

The spatial analysis itself is often supported by wrapper packages that integrate external libraries, command line tools or a mixture of both in an R-like syntax [rgeos](https://CRAN.R-project.org/package=rgeos), [geosphere](https://CRAN.R-project.org/package=geosphere), [Distance](https://CRAN.R-project.org/package=Distance), [maptools](https://CRAN.R-project.org/package=maptools), [igraph](https://CRAN.R-project.org/package=igraph) or [spatstat](https://CRAN.R-project.org/package=spatstat). 

A comprehensive introduction to the spatial R-biotope and its backgrounds is excellently treated in [Geocomputation with R](https://geocompr.robinlovelace.net) wich is highly recommend as a reference textbook.

Despite all this capabilities of spatial analysis and data handling in the world of `R`, it can be stated (at least from a non-R point of view), that there is still a enormous gap between R and the mature open source Geographic Information System (GIS) and even more Remote Sensing (RS) software community. `QGIS`, `GRASS GIS` and `SAGA GIS` are providing a comprehensive, growing and mature  collection of highly sophisticated algorithms. The provided algorithms are fast, stable and most of them are well proofed. Probably most of the `R` users who are somehow related to the GI community know that there are awesome good wrapper packages for bridging this gap. For [GRASS GIS 7](https://grass.osgeo.org/) it is [rgrass7](https://CRAN.R-project.org/package=rgrass7) and for [SAGA GIS](http://www.saga-gis.org/)  the [RSAGA](https://CRAN.R-project.org/package=RSAGA) package. The development of the [RQGIS](https://CRAN.R-project.org/package=RQGIS) wrapper is the most recent outcome to provide a simple  usage of the powerful [QGIS](https://www.qgis.org/) command line interface.
In addition there is no wrapper for the great `OTB`. It seems to be at least convenient to provide a lightweight wrapping utility for the usage of `OTB` modules from `R`.

Unfortunately one will run into a lot of technical problems depending on the choosen operating system (OS) or library dependencies or GIS software versions. In case of e.g. `RSAGA` the main problem has been that the `SAGA` GIS developers are not only changing the syntax and strategy of the command line interface (CLI) but also within the same release the calls differ from OS to OS. So the maintenance of RSAGA is at least laborious (but thumbs up is running again).  Another example is  given by `GRASS GIS`  which is well known for a sophisticated setup of the environment and the spatial properties of the database. If you "just" want to use a specific `GRASS` algorithm from R, you will probablys get lost in setting up all OS-dependencies that are neccessary to set up a correct temporary or permanent `GRASS`-environment from “outside”. This is not only caused due to the strict spatial and projection requirements of `GRASS` but much more by challenging OS enviroments especially Windows. 

To make it short it is a bit cumbersome to deal with all this stuff if one just want to start e.g. `GRASS` from the R command line for e.g. a powerful random walk cost analysis (`r.walk`) call as provided by `GRASS`.


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


So the most straightforward call to link temporary to `GRASS GIS` woud be:


```r
# find all SAGA GIS installations at the default search location
require(link2GI)

grass <- link2GI::linkGRASS7()
grass
```


