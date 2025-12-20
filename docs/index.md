# `link2GI`

[![cran
checks](https://badges.cranchecks.info/worst/link2GI.svg)](https://cran.r-project.org/web/checks/check_results_link2GI.html)
![monthly](https://cranlogs.r-pkg.org/badges/link2GI)![total](https://cranlogs.r-pkg.org/badges/grand-total/link2GI)[![CRAN](https://www.r-pkg.org/badges/version/link2GI?color=009999)](https://cran.r-project.org/package=link2GI)
[![Stars](https://img.shields.io/github/stars/r-spatial/link2GI?style=flat)](https://github.com/r-spatial/link2GI)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)

Package website: [release](https://r-spatial.github.io/link2GI/)

`link2GI` provide some functions which make it a bit easier to connect
straightforward the common open source GI software packages to the
R-biotop. It supports both the use of wrapper packages and the direct
API-use via system calls. It focuses on `Linux` and `WindowsX` operating
systems but nevertheless it should also work with `OSX`.

If you have several versions installed or if you want to use the full
power of the GI software it will be a nice and helpful tool to deal with
some strange behaviours and requirements. Especially helpful is the
support of `OTB`, the `GDAL-Python` functions and a simple support to
use any `SAGA` version via the CLI. Hopefully `RSAGA` and `Rsagacmd`
will join forces in one package in the future. Anyway the current
releases (1.4.1) of `RSAGA` as well as `Rsagacmd` will work with the
environment settings as provided by `linkSAGA`.

# Installation

`link2GI`is up to CRAN. For the installation of the stable version
please use `install.packages("link2GI")`.

However it is strongly recommended to install the latest stable version:

``` r
# devtools package
devtools::install_github("r-spatial/link2GI", ref = "master")
# remotes package
remotes::install_github("r-spatial/link2GI",ref = "master")
```

## Third Party software

To utilize the power of the open source GI tools from within `R` you
need to install them first. As a first promising opportunity to do
fulfill most of the requirements you may install `QGIS, GRASS`,
`SAGA-GIS` and `Orfeo-toolbox`. For further Information have a look at
the [Installation guide for link2GI related
Software](https://r-spatial.github.io/link2GI/articles/link2GI6.html).

# New Feature

## `initProj` for reproducible projects

`initProj` provides a complete and flexible working environment for GI
projects. The focus is on a simple but powerful structure. The basic
framework is formed by a defined folder structure, initial scripts and
configuration templates as well as optional Git repositories and an
`renv` environment. A corresponding RStudio project file is also
created. It supports the automatic installation (if needed) and loading
of the required libraries including various standard setup skeletons to
simplify project initialisation.

The function creates a skeleton of the skeleton scripts
`main-control.R`, `pre-processing.R`, `10-processing.R` and
`post-processing.R`, and creates corresponding parameter configurations
files stored as `yaml` files in `scr/configs/`. The script
src/functions/000_settings.R holds all specific project settings. Easy
access to all project paths is provided via the list variable `dirs`,
which enables simple, efficient and reproducible project management and
data handling.

For further Information have a look at the article [Reproducible
Projects](https://r-spatial.github.io/link2GI/articles/link2GI5.html).

When using RStudio, a new project can be created by simply selecting the
***Create Project Structure (link2GI)*** template from the ***File -\>
New Project -\> New Directory -\> New Project Wizard*** dialogue.

![Animated demonstration of the link2GI GUI workflow (select API,
configure paths,
run).](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/usegui.gif)

Animated demonstration of the link2GI GUI workflow (select API,
configure paths, run).

Animated demonstration of the link2GI GUI workflow (select API,
configure paths, run).
