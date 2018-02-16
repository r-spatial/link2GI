[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) 
[![Travis-CI Build Status](https://travis-ci.org/gisma/link2GI.svg?branch=master)](https://travis-ci.org/gisma/link2GI)
<a href="https://cran.r-project.org/web/checks/check_results_link2GI.html"><img border="0" src="http://www.r-pkg.org/badges/version/link2GI" alt="CRAN version"></a>
![](https://cranlogs.r-pkg.org/badges/grand-total/link2GI?color=green)
![](https://cranlogs.r-pkg.org/badges/link2GI?color=green)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

# link2GI

`link2gi` provides functions that make it possible to connect most of the common open source GI software packages to the R-biotop in a straightforward way. It focuses on `Linux` and `WindowsX` operating systems but nevertheless it should also work with `OSX`.

To utilize the power of the open source GI tools from within `R` you need to install  them first. As a first promising opportunity to do fullfil most of the requirements you may install `QGIS, GRASS`- and `SAGA-GIS` following the excellent [installation instructions](https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd)  of the [RQGIS](https://cran.r-project.org/web/packages/RQGIS/index.html) package will have a good first try to ensure a smooth working environment. Note: for most of the cases you may use just `RQGIS` as wrapper for the `QGIS` related functionality. 

If you have several versions installed or if you want to use the full power of the GI software it will be a nice and helpful tool to deal with some strange behaviours and requirements. Especially helpful is the support of `OTB`, the `GDAL-Python` functions and (at least until the well known `RSAGA` wrapper package is updated) the opportunity to deal with any `SAGA` version using straightforward the CLI. Probably the `RSAGA` update will be not that important anymore because most recently  the very promising  [`Rsagacmd`](https://github.com/stevenpawley/Rsagacmd) wrapper package is providing a new list oriented wrapping approach that is also working seamingless with `link2GI::findSAGA`.

For the future it is planned to interface the `liblas` libraries, `Fusion`, `taudem` and some more exotic GI tools.

# Installation

`link2GI`is up to CRAN. For the installation of the stable version please use `install.packages("link2GI")`. To install the  cutting edge version use `devtools::install_github("gisma/link2GI", ref = "master")`.

