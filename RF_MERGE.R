# Packages 
install.packages("RFmerge")
  library(zoo)
  library(sf)
  library(rgdal)
  library(raster)
  library(RFmerge)
  library(terra)
#csv and P paths
path.csv<-  "C:/Thesis/DATA/Trial/Guage/P_Daily_Total.csv"
path.guage.info <- "C:/Thesis/DATA/Trial/Guage/Guage_info.csv" 
p_products<-"C:/Thesis/DATA/trial"
Jordan_SHP <- "C:/Thesis/DATA/gadm36_JOR_shp/gadm36_JOR_0.shp"

 chirps_path<-     file.path(p_products,"CHIRPS")
 CMORPHv1_path<-   file.path(p_products, "CMORPH")
 ERA5_path <-      file.path(p_products, "ERA5")
 ERA5_land_path <- file.path(p_products, "ERA5")
 IMERG_FR_path<-   file.path(p_products, "IMERG")
 MSWEPv2.80_path<- file.path(p_products, "MSWEPv2.8")
 DEM_path<-        file.path(p_products, "DEM")

##############
chirps.fname<- system.file(chirps_path, package = "RFmerge")


CHIRPS5km <- brick(chirps.fname)
#######################
#     List raster     #
#######################

  p_1<- list.files(chirps_path, full.names = T, pattern = ".tif")
  p_2<- list.files(CMORPHv1_path, full.names =T , pattern = ".tif")
  p_3<- list.files(ERA5_path, full.names = T, pattern = ".tif")
  p_4<- list.files(ERA5_land_path, full.names = T, pattern = ".tif")
  p_5<- list.files(IMERG_FR_path, full.names = T, pattern = ".tif")
  p_6<- list.files(MSWEPv2.80_path, full.names = T, pattern = ".tif")
  p_7<- list.files(DEM_path, full.names = T, pattern = ".tif")

  #######################
  #       Stacking      #
  #######################
chirps_stack<- rast(p_1)
  p_2_stack<-  rast(p_2)
  P_3_stack <- rast(p_3)
  p_4_stack <- rast(p_4)
  p_5_stack <- rast(p_5)
  P_6_stack <- rast(p_6)
  P_7_stack <- rast(p_7)

  #######################
  #       Bricking      #
  #######################

 chirps5km<-      brick(chirps_stack)
 cmorph_5km <-    brick(p_2_stack)
 era5_5km <-      brick(P_3_stack)
 era5_land_5km <- brick(p_4_stack)
 imerg_5km <-     brick(p_5_stack)
 mswep_5km <-     brick(P_6_stack)
 dem_5km <-       brick(P_7_stack)
 
 
 #ldates <- hydroTSM::dip("1983-01-01", "1983-08-31")
 ldates <- seq(from=as.Date("2004-01-01"), to=as.Date("2004-01-31"), by="days")
 names(chirps5km) <- ldates
 names(cmorph_5km) <- ldates
 names(era5_5km) <- ldates
 names(era5_land_5km) <- ldates
 names(imerg_5km) <- ldates
 names(mswep_5km) <- ldates
 #names(dem_5km) <- ldates
 
data.gauge<- read.csv(path.csv, header = T)
main <- paste("Daily precipitation for the station", data.gauge$Code[1])
ylab <- "Precipitation [mm]"
x.ts <- data.gauge[,1]

#hydroTSM::hydroplot(x.ts, pfreq="o", main=main, ylab= ylab)
plot(x.ts, main=main, ylab= ylab, col="blue")
grid()

# Plotting the accumulated precipitation estimates and overlaying the boundaries of the study area

jordan_shp<- vect(Jordan_SHP)

chirps_total <- sum(chirps5km, na.rm = F)
cmorph_total <- sum(cmorph_5km, na.rm = F)
era5_total <- sum(era5_5km, na.rm = F)
era5_land_total <- sum(era5_land_5km, na.rm = F)
imerg_total <- sum(imerg_5km, na.rm = F)
mswep_total <- sum(mswep_5km, na.rm = F)

plot(chirps_total, main = "CHIRPSv2 [2004_Jan] ", xlab = "Longitude", ylab = "Latitude")
plot(jordan_shp, add=TRUE, col="transparent")

plot(cmorph_total, main = "CMORPH [2004_Jan] ", xlab = "Longitude", ylab = "Latitude")
plot(jordan_shp, add=TRUE, col="transparent")

plot(era5_total, main = "ERA5 [2004_Jan] ", xlab = "Longitude", ylab = "Latitude")
plot(jordan_shp, add=TRUE, col="transparent")

plot(era5_land_total, main = "ERA5_Land [2004_Jan] ", xlab = "Longitude", ylab = "Latitude")
plot(jordan_shp, add=TRUE, col="transparent")

plot(imerg_total, main = "IMERG [2004_Jan] ", xlab = "Longitude", ylab = "Latitude")
plot(jordan_shp, add=TRUE, col="transparent")

plot(mswep_total, main = "MSWEPv2.8 [2004_Jan] ", xlab = "Longitude", ylab = "Latitude")
plot(jordan_shp, add=TRUE, col="transparent")

# Preparing input data
# Spatial metadata
#In order to use the spatial information stored in stations CSV, we first need to convert it into a
# SpatialPointsDataFrame, using the latitude and longitude fields, stored in the lat and lon columns:
  stations <- read.csv(path.guage.info, header = T)
( stations <- st_as_sf(stations, coords = c('lon', 'lat'), crs = 4326) )
  
  
  plot(dem_5km, main="SRTM-v4", xlab="Longitude", ylab="Latitude", col=terrain.colors(255))
  plot(jordan_shp[1], add=TRUE, col="transparent")
  plot(stations[1], add=TRUE, pch = 16, col="black")
  
  nlayers(chirps5km)
  ( nlayers(chirps5km) == nlayers(cmorph_5km) )
  ( nlayers(chirps5km) == nrow(data.gauge) )
  
  #verify that the precipitation products and the DEM have the same spatial extent
  extent(chirps5km)
extent(dem_5km)  

 ( extent(chirps5km) == extent(cmorph_5km))
( extent(chirps5km) == extent(era5_5km))
( extent(chirps5km) == extent(era5_land_5km))
( extent(chirps5km) == extent(imerg_5km))
( extent(chirps5km) == extent(mswep_5km))

#resolution
res(chirps5km)

( res(chirps5km) == res(cmorph_5km))
( res(chirps5km) == res(era5_5km))
( res(chirps5km) == res(era5_land_5km))
( res(chirps5km) == res(imerg_5km))
( res(chirps5km) == res(mswep_5km))


  #Optional: Re projection into another CRS (UTM)
# the Universal Transverse Mercator (UTM) coordinate system, 
#the Earth is divided into 60 zones, each 6 degrees of longitude wide,
#extending from 84°S to 80°N. 

# first: project the rainfall observations 
stations.utm <- sf::st_transform(stations, crs=25837) # for 'sf' objects

# second, project the sataellite products
utmz37n.p4s <- sf::st_crs(stations.utm)$proj4string

chirps5km.utm <- projectRaster(from=chirps5km, crs=utmz37n.p4s)
cmorph_5km.utm <- projectRaster(from=cmorph_5km, crs=utmz37n.p4s)
era5_5km.utm <- projectRaster(from=era5_5km, crs=utmz37n.p4s)
era5_land_5km.utm <- projectRaster(from=era5_land_5km, crs=utmz37n.p4s)
imerg_5km.utm <- projectRaster(from=imerg_5km, crs=utmz37n.p4s)
mswep_5km.utm <- projectRaster(from=mswep_5km, crs=utmz37n.p4s)
dem_5km.utm <- projectRaster(from=dem_5km, crs=utmz37n.p4s)

# project the shapefile of Jordan: 
jordan_shp.utm <- sf::st_transform(jordan_shp, crs=25837)