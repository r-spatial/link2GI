[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) 
[![Travis-CI Build Status](https://travis-ci.org/r-spatial/link2GI/link2GI.svg?branch=master)](https://travis-ci.org/r-spatial/link2GI/link2GI)
<a href="https://cran.r-project.org/web/checks/check_results_link2GI.html"><img border="0" src="http://www.r-pkg.org/badges/version/link2GI" alt="CRAN version"></a>
![](https://cranlogs.r-pkg.org/badges/grand-total/link2GI?color=green)
[![GitHub last commit](https://img.shields.io/github/last-commit/google/skia.svg)](/master/)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

# link2GI

`link2gi` provides functions that make it a bit easier to connect most of the common open source GI software packages to the R-biotop in a straightforward way. It supports both the use of wrapper packages and the direct use via system calls. It focuses on `Linux` and `WindowsX` operating systems but nevertheless it should also work with `OSX`.

To utilize the power of the open source GI tools from within `R` you need to install  them first. As a first promising opportunity to do fullfil most of the requirements you may install `QGIS, GRASS`- and `SAGA-GIS` following the excellent [installation instructions](https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd)  of the [RQGIS](https://cran.r-project.org/web/packages/RQGIS/index.html) package will have a good first try to ensure a smooth working environment. Note: for most of the cases you may use just `RQGIS` as wrapper for the `QGIS` related functionality. 

If you have several versions installed or if you want to use the full power of the GI software it will be a nice and helpful tool to deal with some strange behaviours and requirements. Especially helpful is the support of `OTB`, the `GDAL-Python` functions and a simple support to use any `SAGA` version straightforward via the CLI. Hopefully `RSAGA` and [`Rsagacmd`](https://github.com/stevenpawley/Rsagacmd) will join forces in one package in the future. Anyway the brand new release 1.0.0 of  `RSAGA` as well as the current `Rsagacmd` will work with the environment settings as provided by `linkSAGA`.

Next major release will focus on adding CLI interfaces to the GI tools that will not anymore be hosted under the `QGIS` processing hood and hence will also loose the interface via `RQGIS`. 

# Installation

`link2GI`is up to CRAN. For the installation of the stable version please use `install.packages("link2GI")`. To install the  cutting edge version use `devtools::install_github("gisma/link2GI", ref = "master")`.

