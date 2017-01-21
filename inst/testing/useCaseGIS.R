# define the used input file(s)
inputFile<- "original_dem.tif"

# if at a new location create filestructure
initProj(projRootDir<- "~/proj/Monte_Bernorio" )

# set working directory
setwd(path_run)

######### initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal<- initgdalUtils()

# initialize SAGA GIS bindings for SAGA 3.x
initSAGA()

# initialize OTB bindings for OSGeo4W64 default 
initOTB()

library(sp)
data(meuse) 
coordinates(meuse) <- ~x+y 
proj4string(meuse) <-CRS("+init=epsg:28992") 
initGRASS(meuse)
r<- raster::stack(paste0(path_data,inputFile))

initGRASS(r)


