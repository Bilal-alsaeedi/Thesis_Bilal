
#######################
#### Precipitation  ###
#######################

library(raster)
library(terra)
library(sf)
library(rgdal)
library(doParallel)
library(rgdal)
library(ncdf4)


dir<- "CMORPH/"
shapefile.path<- "gadm36_JOR_shp/"
output.path<- "output/cmorph_clipped/"

### Preparing the shape file and the rasters ###  

shape.jordan<- vect(shapefile.path)
plot(shape.jordan)
list.files <- list.files(dir, full.names = T, pattern = ".tif")
print(list.files) 



precipitation<- rast(list.files)
plot(precipitation[[1:5]])

#### MAsking and Croping ### 
precipitation_jordan<- crop (precipitation, shape.jordan, snap= "out")
precipitation_jordan<- mask (precipitation_jordan, shape.jordan)
plot(precipitation_jordan)


#####################################

#This is just a way to save the result as netCDF
jordan_NC <- file.path(output.path, paste0("Jordan_daily_Preci_CMORPH.nc"))
writeCDF(precipitation_jordan, jordan_NC, overwrite = TRUE)




##################
# Saving the Rasters with proper names 
for(i in 1:nlyr(precipitation_jordan)) { 
  file<-precipitation_jordan[[i]]
  
  dates <- (basename(list.files))
  dates<- substr(basename(list.files),33,42)
  #dates<- paste0 (substr(basename(list.files),14,17), "-",
  #paste0 (substr(basename(list.files),18,19), "-", paste0 (substr(basename(list.files),20,21))))
  names <- paste0 ("Jordan_daily_P_CMORPH", "-", dates, ".tif")
  
  writeRaster(file, file.path (output.path, names[i]), overwrite = TRUE)
  
} 


