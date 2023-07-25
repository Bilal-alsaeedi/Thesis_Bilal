install.packages("automap")
install.packages("gstat")
library(automap)
library(gstat)
library(raster)
library(doParallel)
#library(terra)
data_path     <- "C:/Thesis/Models/KED/NEW/Data/Gauge/2017/P_Daily.csv"
metadata.path <- "C:/Thesis/Models/KED/NEW/Data/Gauge/Guage_info.csv"
output   <- "E:/Thesis/KED/chirps_2009-2017"
dem      <- "C:/Thesis/Models/KED/Data/Gridded_data/DEM/Jordan_DEM_res.tif"


dir.pprods <- "C:/Thesis/Models/KED/Data/Gridded_data"

chirps       <- file.path(dir.pprods, "2009-2017/chirps")
#cmorph       <- file.path(dir.pprods, "CMORPHv1")
#ERA5         <- file.path(dir.pprods, "ERA5")
#ERA5_LAND    <- file.path(dir.pprods, "ERA5_LAND")
#IMERG        <- file.path(dir.pprods, "IMERG_FR")
#MSWEP        <- file.path(dir.pprods, "MSWEPv2.80")
#PERSIANN_CDR <- file.path(dir.pprods, "PERSIANN-CDR")
#mswep out,, it is already merged 

data     <- read.csv(data_path)
metadata <- read.csv(metadata.path)
chirps_list       <- list.files(chirps, full.names = TRUE)
#cmorph_list       <- list.files(cmorph, full.names = TRUE)
#era5_list         <- list.files(ERA5, full.names = TRUE)
#era5_land_list    <- list.files(ERA5_LAND, full.names = TRUE)
#imerg_list        <- list.files(IMERG, full.names = TRUE)
#mswep_list        <- list.files(MSWEP, full.names = TRUE)
#persiann_list     <- list.files(PERSIANN_CDR, full.names = TRUE)


metadata <- metadata[, 2:4]

cores <- detectCores() - 1
#cores <- 30
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i = 1:length(chirps_list), .packages = c("automap", "gstat", "raster")) %dopar% {
  
  
  date <- substr(basename(chirps_list[i]), 23, 32)
  # data.day <- metadata
  
  data.day <- metadata
  data.day$obs <- NA
  
  chirps.day       <- raster(chirps_list[i])
  #chirps.day       <- rast(chirps_list)
  
  #cmorph.day       <- raster(cmorph[i])
  #era5.day       <- raster(era5_list[i])
  #era5.land.day       <- raster(era5_land_list[i])
  #imerg.day       <- raster(imerge_list[i])
  #mswep.day       <- raster(mswep_list[i])
  #persiann.day       <- raster(persiann_list[i])
  
  
  
  for(j in 1:nrow(data.day)){
    
    data.day$obs[j] <- data[i,which(names(data) == data.day$Code[j])]
    
  }
  
  points <- metadata
  coordinates(points) <- ~ lon + lat
  
  #anything less than zero assign to zero 
  chirps.day[chirps.day < 0] <- 0
  #all the products
  
  chirps.points        <- as.numeric(raster::extract(chirps.day, points))
  #cmorph.points       <- as.numeric(raster::extract(cmorph.day, points))
  #era5.points         <- as.numeric(raster::extract(era5.day, points))
  #era5.land.points    <- as.numeric(raster::extract(era5.land.day, points))
  #imerg.points        <- as.numeric(raster::extract(imerg.day, points))
  #mswep.points        <- as.numeric(raster::extract(mswep.day, points))
  #persiann.cdr.points <- as.numeric(raster::extract(persiann.day, points))
  
  #na_indices <- which(is.na(chirps.points))
  
  #column_names <- colnames(data.day)[na_indices]
  
  
  data.day <- data.frame(data.day, chirps.points )#, cmorph.points,era5.points, era5.land.points
  #, imerg.points, mswep.points, persiann.cdr.points)
  
  names(data.day) <- c("Code", "longitude", "latitude", "obs", "CHIRPS")#, 
  # "CMORPH", "ERA5", "ERA5_LAND", "IMERG", "MSWEP", "PERSIANN_CDR")
  
  if(length(which(is.na(data.day$obs))) > 0)
    data.day <- data.day[-which(is.na(data.day$obs)),]
  
  
  data.day$lon <- as.numeric(data.day$lon)
  data.day$lat <- as.numeric(data.day$lat)
  coordinates(data.day) <- ~ lon + lat
  crs(data.day)         <- crs(chirps.day)
  
  #Semivariogram
  KED <- autofitVariogram(obs ~ CHIRPS, data.day)
  
  
  #chirps.SGDF       <- as(chirps.day, "SpatialGridDataFrame")
  # cmorph.SGDF       <- as(cmorph.day, "SpatialGridDataFrame")
  # era.interim.SGDF  <- as(era.interim.day, "SpatialGridDataFrame")
  # persiann.cdr.SGDF <- as(persiann.cdr.day, "SpatialGridDataFrame")
  # trmm.SGDF         <- as(trmm.day, "SpatialGridDataFrame")
  # 
  #chirps.SGDF$CHIRPS             <- chirps.SGDF
  # cmorph.SGDF$CMORPH             <- cmorph.SGDF
  # era.interim.SGDF$ERA_INTERIM   <- era.interim.SGDF
  # persiann.cdr.SGDF$PERSIANN_CDR <- persiann.cdr.SGDF
  # trmm.SGDF$TRMM_3B42v7          <- trmm.SGDF
  # 
  #SGDF              <- chirps.SGDF$CHIRPS
  # SGDF$CHIRPS       <- chirps.SGDF$CHIRPS
  # SGDF$CMORPH       <- cmorph.SGDF$CMORPH
  # SGDF$ERA_INTERIM  <- era.interim.SGDF$ERA_INTERIM
  # SGDF$PERSIANN_CDR <- persiann.cdr.SGDF$PERSIANN_CDR
  # SGDF$TRMM_3B42v7  <- trmm.SGDF$TRMM_3B42v7
  
  
  covar <- stack(chirps.day)
  names(covar) <-  c("CHIRPS")
  
  model <- gstat(NULL, id = "obs", formula = obs ~ CHIRPS,
                 locations = data.day, model = KED$var_model)
  
  ked.result <- interpolate(covar, model, xyOnly=FALSE)
  ked.result[ked.result < 0] <- 0
  
  writeRaster(ked.result, paste0(output, "/KED_CHIRPS_Jordan_", date, ".tif"), format = "GTiff", overwrite = TRUE)
  
}

stopCluster(cl)
