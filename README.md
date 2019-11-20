[![Travis-CI Build Status](https://travis-ci.org/r-spatial/link2GI.svg?branch=master)](https://travis-ci.org/r-spatial/link2GI)
<a href="https://cran.r-project.org/web/checks/check_results_link2GI.html"><img border="0" src="http://www.r-pkg.org/badges/version/link2GI" alt="CRAN version"></a>
![](https://cranlogs.r-pkg.org/badges/grand-total/link2GI?color=green)

[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

# link2GI

`link2GI` provide some functions which make it a bit easier to connect straightforward the common open source GI software packages to the R-biotop. It supports both the use of wrapper packages and the direct API-use via system calls. It focuses on `Linux` and `WindowsX` operating systems but nevertheless it should also work with `OSX`.

To utilize the power of the open source GI tools from within `R` you need to install  them first. As a first promising opportunity to do fullfil most of the requirements you may install `QGIS, GRASS`- and `SAGA-GIS` following the excellent [installation instructions](https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd)  of the [RQGIS](https://CRAN.R-project.org/package=RQGIS) package will have a good first try to ensure a smooth working environment. Note: for most of the cases you may use just `RQGIS` as wrapper for the `QGIS` related functionality. 

If you have several versions installed or if you want to use the full power of the GI software it will be a nice and helpful tool to deal with some strange behaviours and requirements. Especially helpful is the support of `OTB`, the `GDAL-Python` functions and a simple support to use any `SAGA` version via the CLI. Hopefully `RSAGA` and [`Rsagacmd`](https://github.com/stevenpawley/Rsagacmd) will join forces in one package in the future. Anyway the brand new release 1.2.0 of  `RSAGA` as well as the current `Rsagacmd` will work with the environment settings as provided by `linkSAGA`.


# Installation

`link2GI`is up to CRAN. For the installation of the stable version please use `install.packages("link2GI")`. To install the  cutting edge version use:
```r
# devtools package
devtools::install_github("r-spatial/link2GI", ref = "master")
# remotes package
remotes::install_github("r-spatial/link2GI")
```


# Usecases presented on the GEOSTAT August 2018

During the [GEOSTAT 2018](http://opengeohub.org/node/146) in Prague some more complex usescases has been [presented](https://htmlpreview.github.io/?https://github.com/gisma/link2gi2018/blob/master/link2gi2018.html).

### Find slides and materials
[GEOSTAT 2018](https://htmlpreview.github.io/?https://github.com/gisma/link2gi2018/blob/master/link2gi2018.html) presentation slides.

[link2GI GEOSTAT](https://github.com/gisma/link2gi2018) github repository.

## Prerequisites
Please check the R dependencies:

```r
install.packages(c("sf", "raster",  "rgdal",  "tools", "rgrass7", "sp", "RSAGA", "link2GI"))

# for the Canopy height model usecase you need to install uavRst
devtools::install_github("r-spatial/uavRst", ref = "master")

```

In addition you need at least one installation of the following GIS software.

- For `GRASS`- and `SAGA-GIS` follow the [RQGIS installation instructions](https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd) as provided by Jannes Muenchow. For standalone GRASS you may have a look at the the [geostat2018 instructions](https://gitlab.com/veroandreo/grass-gis-geostat-2018) as provided by Veronica Andreos.
- For installing the `Orfeo Toolbox`, please follow the OTB cookbook [installation instructions](https://www.orfeo-toolbox.org/CookBook/Installation.html).

Please download the data and scripts for the exercises.

**PLEASE NOTE:** 

If you run the following code you will create the folder *link2gi-master* in your **home folder**. During the tutorial it is assumed to be the root folder.


```r
url <- "https://github.com/gisma/link2gi2018/archive/master.zip"
res <- curl::curl_download(url, paste0(tmpDir(),"master.zip"))
utils::unzip(zipfile = res, exdir = "~")
```

## The examples

- Basic usage of SAGA and OTB calls - [SAGA & OTB basic usecase](https://github.com/gisma/link2gi2018/blob/master/R/usecases/saga-otb/useCaseSAGA-OTB.R)

- Wrapping a [GRASS GIS example](https://neteler.gitlab.io/grass-gis-analysis/02_grass-gis_ecad_analysis/) of Markus Neteler as presented on GEOSTAT 2018 - [Analysing the ECA&D climatic data - reloaded](https://github.com/gisma/link2gi2018/blob/master/R/usecases/grass/useCaseGRASS-Neteler2018.R)

- Performing a GRASS based cost analysis on a huge cost raster - [Beetle spread over high asia](https://github.com/gisma/link2gi2018/blob/master/R/usecases/cost-analysis/useCaseBeetle.R)

- Deriving a canopy height model using a mixed API approach - [Canopy Height Model from UAV derived point clouds](https://github.com/gisma/link2gi2018/blob/master/R/usecases/uav-pc/useCaseCHM.R)
