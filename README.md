# link2GI

Provides functions to access some of the most important GIS/RS and image analysis command line tools from R.  It focuses on 'Linux' and 'WindowsX' operating systems but nevertheless it should also work with 'OSX'. 

For the full power of the open source GI toools you need to install  them. As a first promising step to do fullfil most of the requirements you may install QGIS, GRASS- and SAGA-GIS following the excellent [installation instructions](https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd)  of the [RQGIS](https://cran.r-project.org/web/packages/RQGIS/index.html) package will have a good first try to ensure a smooth working environment. Note: for moste of the cases you may use just RQGIS as wrapper for the QGIS related functionality. 

If you have several versions installed or if you want to use the full power of the GI software it will be a nice and helpful tool to deal with some strange behaviours and requirements. Especially the support of OTB, the GDAL Python functions and currently and the opportunity to deal with any Saga version using the CLI could be helpful.

For the future it is planned to interface the liblas, Fusion taudem and some more exotic GI tools.


For installation use devtools::install_github().

```S
devtools::install_github("gisma/link2GI", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/link2GI", ref = "master", dependencies = TRUE, force = TRUE)
```
