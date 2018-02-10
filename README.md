# link2GI

`link2GI` provides functions that make it possible to connect the common opensource GI software packages to the R-biotope.  It focuses on `Linux` and `WindowsX` operating systems but nevertheless it should also work with `OSX`. 

To utilize the power of the open source GI tools from within `R` you need to install  them first. As a first promising opportunity to do fullfil most of the requirements you may install `QGIS, GRASS`- and `SAGA-GIS` following the excellent [installation instructions](https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd)  of the [RQGIS](https://cran.r-project.org/web/packages/RQGIS/index.html) package will have a good first try to ensure a smooth working environment. Note: for most of the cases you may use just `RQGIS` as wrapper for the `QGIS` related functionality. 

If you have several versions installed or if you want to use the full power of the GI software it will be a nice and helpful tool to deal with some strange behaviours and requirements. Especially helpful is the support of `OTB`, the `GDAL-Python` functions and at least until the great `RSAGA` wrapper package is updated the opportunity to deal with any `SAGA` version using straightforward the CLI.

For the future it is planned to interface the `liblas` libraries, `Fusion`, `taudem` and some more exotic GI tools.



For installation use devtools::install_github().

```S
devtools::install_github("gisma/link2GI", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/link2GI", ref = "master", dependencies = TRUE, force = TRUE)
```
