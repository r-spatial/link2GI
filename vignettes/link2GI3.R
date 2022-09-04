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

