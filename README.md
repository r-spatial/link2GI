# link2GI

[![Travis-CI Build Status](https://travis-ci.org/r-spatial/link2GI.svg?branch=master)](https://travis-ci.org/r-spatial/link2GI)
![](https://cranlogs.r-pkg.org/badges/grand-total/link2GI?color=green)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)


Package website: [release](https://r-spatial.github.io/link2GI/) | [dev](https://r-spatial.github.io/link2GI/dev/)

`link2GI` provide some functions which make it a bit easier to connect straightforward the common open source GI software packages to the R-biotop. It supports both the use of wrapper packages and the direct API-use via system calls. It focuses on `Linux` and `WindowsX` operating systems but nevertheless it should also work with `OSX`.



If you have several versions installed or if you want to use the full power of the GI software it will be a nice and helpful tool to deal with some strange behaviours and requirements. Especially helpful is the support of `OTB`, the `GDAL-Python` functions and a simple support to use any `SAGA` version via the CLI. Hopefully `RSAGA` and [`Rsagacmd`](https://github.com/stevenpawley/Rsagacmd) will join forces in one package in the future. Anyway the brand new release 1.2.0 of  `RSAGA` as well as the current `Rsagacmd` will work with the environment settings as provided by `linkSAGA`.


# Installation

`link2GI`is up to CRAN. For the installation of the stable version please use `install.packages("link2GI")`. 

To install the  latest stable version use:
  ```r
# devtools package
devtools::install_github("r-spatial/link2GI", ref = "master")
# remotes package
remotes::install_github("r-spatial/link2GI",ref = "master")
```

## Third Party software
To utilize the power of the open source GI tools from within `R` you need to install  them first. As a first promising opportunity to do fulfill most of the requirements you may install `QGIS, GRASS`- and `SAGA-GIS` .

# Documentation

### Updated OTB wrapper

the OTB wrapper is updated for a more convinient usage. Please have a look at the usecase below:
  
```r

## link to OTB
require(link2GI)
require(raster)
require(listviewer)

otblink<-link2GI::linkOTB()
projRootDir<-tempdir()

data('rgb', package = 'link2GI')  
raster::plotRGB(rgb)
r<-raster::writeRaster(rgb, 
                       filename=file.path(projRootDir,"test.tif"),
                       format="GTiff", overwrite=TRUE)
## for the example we use the edge detection, 
algoKeyword<- "EdgeExtraction"

## extract the command list for the choosen algorithm 
cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)

## get help using the convenient listviewer
listviewer::jsonedit(cmd$help)

## define the mandantory arguments all other will be default
cmd$input  <- file.path(projRootDir,"test.tif")
cmd$filter <- "touzi"
cmd$channel <- 2
cmd$out <- file.path(projRootDir,paste0("out",cmd$filter,".tif"))

## run algorithm
retStack<-runOTB(cmd,gili = otblink)

## plot filter raster on the green channel
plot(retStack)
```


### Online Vignette

[Online Vignettes](https://r-spatial.github.io/link2GI/)


### Usecases presented on the GEOSTAT August 2018



During the [GEOSTAT 2018](https://www.opengeohub.org/node/146) in Prague some more complex usescases have been presented.

#### Slides and materials
- [Presentation slides](https://gisma.github.io/link2gi2018/link2gi2018.html#1)
- [Github Repository](https://github.com/gisma/link2gi2018)

#### Basic usage of SAGA and OTB calls 
- [SAGA & OTB basic usecase](https://github.com/gisma/link2gi2018/blob/master/R/usecases/saga-otb/useCaseSAGA-OTB.R)
                        
#### Wrapping  GRASS 
- Wrapping a [GRASS GIS example](https://neteler.gitlab.io/grass-gis-analysis/02_grass-gis_ecad_analysis/) of Markus Neteler as presented on GEOSTAT 2018 
- [Analysing the ECA&D climatic data - reloaded](https://github.com/gisma/link2gi2018/blob/master/R/usecases/grass/useCaseGRASS-Neteler2018.R)
                        
#### GRASS based cost analysis
- Performing a GRASS based cost analysis on a huge cost raster 
- [Beetle spread over high asia](https://github.com/gisma/link2gi2018/blob/master/R/usecases/cost-analysis/useCaseBeetle.R)
                        
#### API/CLI mixed approach
- Deriving a [Canopy Height Model from UAV derived point clouds](https://github.com/gisma/link2gi2018/blob/master/R/usecases/uav-pc/useCaseCHM.R) a mixed API approach 
                        
