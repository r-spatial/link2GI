## ---- eval=FALSE--------------------------------------------------------------
#   # we need some additional packages
#   require(link2GI)
#   require(curl)
#  
#  
#  # first of all we create  a project folder structure
#  ggis_fn = paste0(tempdir(),"/link2GI_examples")
#    link2GI::initProj(projRootDir = ggis_fn,
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
#   unzip(res,files = grep(".csv", unzip(res,list = TRUE)$Name,value = TRUE),
#         junkpaths = TRUE, overwrite = TRUE)
#  
#   # get the filename
#  fn <- list.files(pattern = "[.]csv$", path = getwd(), full.names = TRUE)
#  

## ----eval=FALSE---------------------------------------------------------------
#   require(RColorBrewer)
#   require(stars)
#   require(terra)
#   require(sf)
#  
#  # fast read with data.table
#   xyz <- data.table::fread(paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.csv"))
#  
#  # check the structure
#   head(xyz)
#  
#  # drop first col
#   xyz <- xyz[,-1]
#  
#  # rasterize it according to the projection
#   r =	stars::st_as_stars(terra::rast(xyz,type = "xyz",crs= sf::st_crs(3035)$wkt))
#  
#  # get some colors
#   p <- colorRampPalette(RColorBrewer::brewer.pal(9, "OrRd"))
#  
#  # map it with a downsampled resolkution of 1 sqkm
#   tmap::tm_shape(r)+
#   tmap::tm_raster("Einwohner", breaks = c(-1,0,1,5,10,50,100,200,300,Inf),
#  		palette = p(9), title="Residents/ha", midpoint = NA)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  require(link2GI)
#  require(sf)
#  # initialize GRASS
#  link2GI::linkGRASS(x = rast(r),
#                      gisdbase = ggis_fn,
#                      location = "microzensus2011")

## ---- eval=FALSE--------------------------------------------------------------
#  require(link2GI)
#  require(stars)
#  require(rgrass)
#  
#  # convenient function provided by rgrass
#   rgrass::write_RAST(x = rast(r),vname =c("Zensus_Bevoelkerung_100m_Gitter"), flags=c("o", "overwrite"))
#  
#  # common way using execGRASS
#  # write it to geotiff
#  stars::write_stars(r, paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"),
#                     overwrite = TRUE)
#  
#  # import raster to GRASS
#   rgrass::execGRASS('r.external',
#                     flags=c('a','o',"overwrite","quiet"),
#                     input=paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"),
#                     output="Zensus_Bevoelkerung_100m_Gitter",
#                     band=1)
#  
#  
#  # check imported data set
#   rgrass::execGRASS('r.info',
#                     map = "Zensus_Bevoelkerung_100m_Gitter")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#   xyz_sf = sf::st_as_sf(xyz,
#                      coords = c("x_mp_100m", "y_mp_100m"),
#                      crs = 3035,
#                      agr = "constant")
#   sf_crs(xyz_sf)  = sf::st_crs(xyz_sf)$wkt
#  #xyz_sf = sf::st_transform(xyz_sf,4314)
#  

## ---- eval=FALSE--------------------------------------------------------------
#   require(rgrass)
#   require(terra)
#  # import point data to GRASS via gpgk and rgrass
#    rgrass::write_VECT(terra::vect(xyz_sf),
#                       flags = c("o","overwrite"),
#                       vname = "Bevoelkerung100m_gpgk",
#                       ignore.stderr = FALSE),

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # import point vector vector via sqlite
#  
#  sf2gvec(x = xyz_sf,
#          obj_name = "Bevoelkerung100m-",
#          gisdbase = ggis_fn,
#          location = "microzensus2011",
#          epsg = 3035,
#          gisdbase_exist = TRUE)
#  
#  # check imported data set
#  rgrass::execGRASS('v.info', map = "bevoelkerung100m_sqlite@PERMANENT")

## ---- eval=FALSE--------------------------------------------------------------
#  # microbenchmark
#  # sf2gvec     write_VECT
#   mean        mean
#   1163.185    1930.099

