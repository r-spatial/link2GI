#' Functions for linking  GI/RS functionality to R
#'
#' @description A straightforward helper tool for linking  GI/RS functionality to R. The goals of the package 
#' are to correctly initialize both the existing wrapper packages \code{rgrass7} and \code{RSAGA} and
#' to smoothly enable the necessary system variables and path parameters for a direct access of the binaries 
#' via system calls on all operating systems. In particular, \code{rgrass7} and \code{RSAGA} can cause severe 
#' problems during initialization of parallel installations of \code{GRASS GIS} or \code{SAGA GIS} under the
#' Windows operating system(s). \code{link2GI} tries to set the correct system settings and returns if system 
#' calls are required the necessary paths and commands.\cr\cr
#' Nevertheless there are also some usefull functions for creating project folder structures, direct reading and 
#' writing of GRASS vector data.
#' 

#' @note To utilize the power of the open source GI tools from within \code{R} you need to installthem first.
#' The link2GI package just tries to generate correct environment settings as system and path variables
#' for the most of the known issues. 
#'  As a first promising opportunity to do fullfil most of the requirements you may install \code{QGIS, GRASS GIS 7.x} 
#'  and \code{SAGA-GIS} following the excellent 
#'  \href{https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd}{installation instructions}  
#'  of the \href{https://CRAN.R-project.org/package=RQGIS}{RQGIS} package will have a good first try to ensure
#'  a smooth working environment. \cr\cr 
#'  For a broad number of problems you may just use the \code{RQGIS} package as wrapper for the functionalitythat can 
#'  be reached via \code{QGIS}.
#' \code{link2GI} is tested under Windows 7/10 as well as on the Ubuntu/Debian Linux distributions. 
#' The OSX operation system should run but is not tested. 
#' @name link2GI
#' @docType package
#' @title Bridges to the GI-World
#' @author Chris Reudenbach Tim Appelhans
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#'
#' @import methods
#' @import sp 
#' @import raster
#' @import rgdal 
#' @import gdalUtils 
#' @import tools 
#' @import rgrass7 
#' @import RSAGA
#' @import roxygen2
#' @import devtools
#' @keywords package
#' 
NULL

