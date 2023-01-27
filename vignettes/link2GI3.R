## ---- eval=FALSE--------------------------------------------------------------
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
#   unzip(res,files = grep(".csv", unzip(res,list = TRUE)$Name,value = TRUE),
#         junkpaths = TRUE, overwrite = TRUE)
#  fn <- list.files(pattern = "[.]csv$", path = getwd(), full.names = TRUE)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  # fast read with data.table
#   xyz <- data.table::fread(paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.csv"))
#  
#   head(xyz)

## ---- eval=FALSE--------------------------------------------------------------
#   require(RColorBrewer)
#   require(terra)
#   require(mapview)
#  
#  
#  # clean dataframe
#   xyz <- xyz[,-1]
#  
#  # rasterize it according to the projection
#    r <- terra::rast(xyz, type="xyz")
#   terra::crs(r) <- 3035
#  
#  # map it
#   p <- colorRampPalette(brewer.pal(8, "Reds"))
#   # aet resolution to 1 sqkm
#   mapview::mapviewOptions(mapview.maxpixels = r@ncols*r@nrows/10)
#   mapview::mapview(r, col.regions = p,
#                    at = c(-1,10,25,50,100,500,1000,2500),
#                    legend = TRUE)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  require(link2GI)
#  # initialize GRASS and set up a permanent structure
#  link2GI::linkGRASS(x = r,
#                      gisdbase = paste0(tempdir(),"/link2GI_examples"),
#                      location = "microzensus2011")

## ---- eval=FALSE--------------------------------------------------------------
#  require(link2GI)
#  require(raster)
#  require(rgrass)
#  
#  # write it to geotiff
#    terra::writeRaster(r, paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"),
#         x               overwrite = TRUE)
#  
#  # import raster to GRASS
#  rgrass::execGRASS('r.external',
#                     flags=c('o',"overwrite","quiet"),
#                     input=paste0(path_run,"/Zensus_Bevoelkerung_100m-Gitter.tif"),
#                     output="Zensus_Bevoelkerung_100m_Gitter",
#                     band=1)
#  
#  # check imported data set
#  rgrass::execGRASS('r.info',
#                     map = "Zensus_Bevoelkerung_100m_Gitter")

## ---- eval=FALSE--------------------------------------------------------------
#  
#   xyz_sf = st_as_sf(xyz,
#                      coords = c("x_mp_100m", "y_mp_100m"),
#                      crs = 3035,
#                      agr = "constant")
#  
#  #map points
#   sf::plot_sf(xyz_sf)

## ---- eval=FALSE--------------------------------------------------------------
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
#  rgrass::execGRASS('v.info', map = "Zensus_Bevoelkerung_100m_")

