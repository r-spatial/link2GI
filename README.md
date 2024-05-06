# link2GI
` `
[![cran
checks](https://badges.cranchecks.info/worst/link2GI.svg)](https://cran.r-project.org/web/checks/check_results_link2GI.html)
![monthly](https://cranlogs.r-pkg.org/badges/link2GI)
![total](https://cranlogs.r-pkg.org/badges/grand-total/link2GI)
[![CRAN](https://www.r-pkg.org/badges/version/link2GI?color=009999)](https://cran.r-project.org/package=link2GI)
[![](https://img.shields.io/github/stars/r-spatial/link2GI?style=flat)](https://github.com/r-spatial/link2GI)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)

` `

Package website: [release](https://r-spatial.github.io/link2GI/)

`link2GI` provide some functions which make it a bit easier to connect straightforward the common open source GI software packages to the R-biotop. It supports both the use of wrapper packages and the direct API-use via system calls. It focuses on `Linux` and `WindowsX` operating systems but nevertheless it should also work with `OSX`.



If you have several versions installed or if you want to use the full power of the GI software it will be a nice and helpful tool to deal with some strange behaviours and requirements. Especially helpful is the support of `OTB`, the `GDAL-Python` functions and a simple support to use any `SAGA` version via the CLI. Hopefully `RSAGA` and [`Rsagacmd`](https://github.com/stevenpawley/Rsagacmd) will join forces in one package in the future. Anyway the current releases (1.4.1) of  `RSAGA` as well as `Rsagacmd` will work with the environment settings as provided by `linkSAGA`.


# Installation

`link2GI`is up to CRAN. For the installation of the stable version please use `install.packages("link2GI")`. 

However it is strongly recommended to install the  latest stable version:
```r
# devtools package
devtools::install_github("r-spatial/link2GI", ref = "master")
# remotes package
remotes::install_github("r-spatial/link2GI",ref = "master")
```

## Third Party software
To utilize the power of the open source GI tools from within `R` you need to install  them first. As a first promising opportunity to do fulfill most of the requirements you may install `QGIS, GRASS`, `SAGA-GIS` and `Orfeo-toolbox`. For further Information have a look at the [Installation guide for link2GI related Software](https://r-spatial.github.io/link2GI/articles/link2GI6.html).

# Documentation



## OTB wrapper

the OTB wrapper is updated for a more convenient usage. Please have a look at the use case below:
  
```r

## link to OTB
require(link2GI)
require(terra)

otblink<-link2GI::linkOTB()
projRootDir<-tempdir()

fn <- system.file("ex/elev.tif", package = "terra")


## for the example we use the edge detection, 
algoKeyword<- "EdgeExtraction"

## extract the command list for the choosen algorithm 
cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)


## define the mandantory arguments all other will be default
cmd$help = NULL
cmd$input_in  <- fn
cmd$filter <- "touzi"
cmd$channel <- 1
cmd$out <- file.path(projRootDir,paste0("out",cmd$filter,".tif"))

## run algorithm
retStack<-runOTB(cmd,gili = otblink)

## plot filter raster on the green channel
plot(retStack)
```
##  `initProj` for reproducible projects
`initProj` sets up a reproducible project environment complete with a defined folder structure, an RStudio project, initial scripts and setup templates, and optionally `git` repositories and `renv` environment. Most important parameters are the project root folder and subfolders for data documentation and scripts. It supports automatic loading and if possible installation of required libraries, a default setup option to simplify project initialisation. The function creates a skeleton of the main control script `main.R`, manages variable configurations through the `src/functions/000_settings.R script` and maintains easy access to paths through the `envrmt` list variable, facilitating an light weighted efficient and reproducible project management and data handling.

```r
require(link2GI)

initProj(root_folder = tempdir(), standard_setup = "baseSpatial_git", newsession = TRUE)
  
```

In addition you may use the `File -> New Project -> New directory -> New Wizard ` dialogue choosing the ***Create Project structure with link2GI*** template to create a new project via the Rstudio menu.

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/initproj2.png)


## Further examples


### Usecases presented on the GEOSTAT August 2018



During the GEOSTAT 2018 (see https://opengeohub.org) in Prague some more complex usescases have been presented.

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
                        
