#' Functions for linking  GI/RS functionality to R
#'
#' @description A straightforward helper tool for linking  GI/RS functionality to R. The goal of the package 
#' is to correctly initialize both the existing wrapper packages \code{rgrass7} and \code{RSAGA} and
#' to smoothly enable the necessary system variables and path parameters for a direct access of the binaries 
#' via direct system calls on all operating systems. In particular, \code{rgrass7} and \code{RSAGA} can cause severe 
#' problems during initialization of parallel installations of \code{GRASS GIS} or \code{SAGA GIS} under the
#' Windows operating system(s). \code{link2GI} tries to set the correct system settings and returns if system 
#' calls are required the necessary paths and command strings. \cr
#' Furthermore the package provides a linkage to the Orfeo Toolbox (OTB) software. 
#' Due to the difficulties linking  the correct GDAL binaries  a  new system wide search for GDAL binaries is implemented.
#' \cr\cr
#' Finally  there are some usefull functions for creating project folder structures and project environments. To deal with the not always consistent API-calls of OTB a list based command parser and generator is provided.
#' 

#' @note To utilize the power of the open source GI tools from within \code{R} you need to install them first.
#' The link2GI package just tries to generate correct environment settings as system and path variables
#' for the most of the known issues. 
#'  The installation of the \code{QGIS, GRASS GIS 7.x} 
#'  and \code{SAGA-GIS} GIS software can performed by following the excellent 
#'  \href{https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd}{installation instructions}  
#'  of the \href{https://CRAN.R-project.org/package=RQGIS}{RQGIS} package. \cr\cr 
#'  For a broad number of problems you may just use the \code{RQGIS} package as wrapper for the full \code{QGIS} functionality.
#' \code{link2GI} is tested under Windows 7/10 as well as on the Ubuntu/Debian/Arch Linux distributions. 
#' The OSX operation system should run but is not tested. 
#' @name link2GI
#' @docType package
#' @title Bridges to the GI-World
#' @author Chris Reudenbach Tim Appelhans
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#'
#' 
NULL
#' @docType data
#' @name rgb
#' @title RGB ortho-image from an arbitray Marburg Open Forest (MOF) plot
#' @description Example data set containing a RGB ortho-image of a small plot sampled in the Maburg University Forest aka Marburg Open Forest (MOF). The resolution is 10 cm, projection ETRS89 UTM32
#' @format \code{"raster::raster"}
NULL
