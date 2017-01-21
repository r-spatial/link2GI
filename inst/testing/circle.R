# Add required packages
require(raster)
require(rgeos)
require(sp)

# Create some data using meuse 
data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
gridded(meuse.grid) = TRUE    
r <- raster(meuse.grid) 
r[] <- runif(ncell(r))

# Create a polygon
f <- gBuffer(meuse[10,], byid=FALSE, id=NULL, width=250, 
             joinStyle="ROUND", quadsegs=10)   

# Plot full raster and polygon                       
plot(r)
plot(f,add=T)

# Crop using extent, rasterize polygon and finally, create poly-raster
#          **** This is the code that you are after ****  
cr <- crop(r, extent(f), snap="out")                    
fr <- rasterize(f, cr)   
lr <- mask(x=cr, mask=fr)

# Plot results
plot(lr)
plot(f,add=T)

p <- rasterToPolygons(r, dissolve=TRUE)
plot(p)

