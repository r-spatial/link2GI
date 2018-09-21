## ---- eval=FALSE---------------------------------------------------------
#  # find all SAGA GIS installations at the default search location
#  require(link2GI)
#  saga <- link2GI::findSAGA()
#  saga

## ---- eval=FALSE---------------------------------------------------------
#  # find all SAGA GIS installations at the default search location
#  require(link2GI)
#  grass <- link2GI::findGRASS()
#  grass
#  otb <- link2GI::findOTB()
#  otb

## ---- eval=FALSE---------------------------------------------------------
#  # find all SAGA GIS installations at the default search location
#  require(link2GI)
#  link2GI::initProj(projRootDir = tempdir(),
#                   projFolders = c("data/",
#                                   "data/level0/",
#                                   "data/level1/",
#                                    "output/",
#                                    "run/",
#                                    "fun/"),
#                   path_prefix = "path_to_" ,
#                   global =TRUE)

## ----eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE----------------
#  # find all SAGA GIS installations at the default search location
#  require(link2GI)
#  require(RSAGA)

## ---- eval=FALSE---------------------------------------------------------
#  saga1<-link2GI::linkSAGA(ver_select = 1)
#  saga1
#  sagaEnv1<- RSAGA::rsaga.env(path = saga1$sagaPath)

## ----eval=FALSE----------------------------------------------------------
#  # get meuse data as sp object
#  require(link2GI)
#  require(sp)
#  data(meuse)
#  coordinates(meuse) <- ~x+y
#  proj4string(meuse) <-CRS("+init=epsg:28992")
#  
#  # get meuse data as sf object
#  require(sf)
#  meuse_sf = st_as_sf(meuse,
#                      coords =
#                        c("x", "y"),
#                      crs = 28992,
#                      agr = "constant")
#  
#  # create a temporary GRASS linkage using the meuse data
#  
#  linkGRASS7(meuse)

## ---- eval=FALSE---------------------------------------------------------
#  linkGRASS7(meuse,c("C:/Program Files/GRASS GIS7.0.5","GRASS GIS 7.0.5","NSIS"))
#  

## ---- eval=FALSE---------------------------------------------------------
#  linkGRASS7(meuse,c("C:/OSGeo4W64","grass-7.0.5","osgeo4W"))

## ---- eval=FALSE---------------------------------------------------------
#  linkGRASS7(meuse_sf,
#                       ver_select = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  linkGRASS7(meuse_sf,
#                       search_path = "D:/")
#  

## ---- eval=FALSE---------------------------------------------------------
#  linkGRASS7(x = meuse_sf,
#                       gisdbase = "~/temp3",
#                       location = "project1")

## ---- eval=FALSE---------------------------------------------------------
#  linkGRASS7(gisdbase = "~/temp3",
#                       location = "project1",
#                       gisdbase_exist = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#   linkGRASS7(spatial_params = c(178605,329714,181390,333611,"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +no_defs +a=6377397.155 +rf=299.1528128 +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +to_meter=1"))
#  

## ---- eval=FALSE---------------------------------------------------------
#   # we need some additional packages
#   require(link2GI)
#   require(curl)
#  
#  # first of all we create  a project folder structure
#    link2GI::initProj(projRootDir = paste0(tempdir(),"/link2GI_examples"),
#                      projFolders =  c("run/"),
#                      path_prefix = "path_",
#                      global = TRUE)
#  
#  # set runtime directory
#    setwd(path_run)
#  
#  # get some typical authority generated data
#    url<-"https://www.zensus2011.de/SharedDocs/Downloads/DE/Pressemitteilung/DemografischeGrunddaten/csv_Bevoelkerung_100m_Gitter.zip;jsessionid=294313DDBB57914D6636DE373897A3F2.2_cid389?__blob=publicationFile&v=3"
#   res <- curl::curl_download(url, paste0(path_run,"testdata.zip"))
#  
#  # unzip it
#   unzip(res,files = grep(".csv", unzip(res,list = TRUE)$Name,value = TRUE),junkpaths = TRUE,overwrite = TRUE)
#  fn <- list.files(pattern = "[.]csv$", path = getwd(), full.names = TRUE)
#  

## ---- eval=FALSE---------------------------------------------------------
#  # get the filename
#  
#  # fast read with data.table
#   xyz <- data.table::fread(paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.csv"))
#  
#   head(xyz)

## ---- eval=FALSE---------------------------------------------------------
#   require(RColorBrewer)
#   require(raster)
#   require(mapview)
#  
#  
#  # clean dataframe
#   xyz <- xyz[,-1]
#  
#  # rasterize it according to the projection
#   r <- raster::rasterFromXYZ(xyz,crs = sp::CRS("+init=epsg:3035"))
#  
#  
#  # map it
#   p <- colorRampPalette(brewer.pal(8, "Reds"))
#   # aet resolution to 1 sqkm
#   mapview::mapviewOptions(mapview.maxpixels = r@ncols*r@nrows/10)
#   mapview::mapview(r, col.regions = p, at = c(-1,10,25,50,100,500,1000,2500), legend = TRUE)
#  

## ---- eval=FALSE---------------------------------------------------------
#  require(link2GI)
#  # initialize GRASS and set up a permanent structure
#  link2GI::linkGRASS7(x = r, gisdbase = paste0(tempdir(),"/link2GI_examples"),location = "microzensus2011")

## ---- eval=FALSE---------------------------------------------------------
#  require(raster)
#  require(rgrass7)
#  
#  # write it to geotiff
#    raster::writeRaster(r, paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"), overwrite = TRUE)
#  
#  # import raster to GRASS
#  rgrass7::execGRASS('r.external',
#                     flags=c('o',"overwrite","quiet"),
#                     input=paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"),
#                     output="Zensus_Bevoelkerung_100m_Gitter",
#                     band=1
#  )
#  
#  # check imported data set
#  rgrass7::execGRASS('r.info',
#                     map = "Zensus_Bevoelkerung_100m_Gitter")

## ---- eval=FALSE---------------------------------------------------------
#  
#   xyz_sf = st_as_sf(xyz,
#                      coords = c("x_mp_100m", "y_mp_100m"),
#                      crs = 3035,
#                      agr = "constant")
#  
#  #map points
#   sf::plot_sf(xyz_sf)

## ---- eval=FALSE---------------------------------------------------------
#   require(sf)
#   require(sp)
#   require(link2GI)
#  
#    sf2gvec(x =  xyz_sf,
#             obj_name = "Zensus_Bevoelkerung_100m_",
#             gisdbase = paste0(tempdir(),"/link2GI_examples"),
#             location = "microzensus2011",
#             gisdbase_exist = TRUE
#  
#             )
#  
#  # check imported data set
#  rgrass7::execGRASS('v.info', map = "Zensus_Bevoelkerung_100m_")

