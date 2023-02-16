
dir<- "C:/Thesis/DATA/resampled/MSWEPv2.8"

library(ncdf4)
library(raster)
library(doParallel)
files<- list.files(dir, full.names = TRUE)
cores<- detectCores() - 1
registerDoParallel(cores)

output <- "C:/Thesis/DATA/resampled/out"

foreach(i= 1:length(files), .packages = c("raster")) %dopar% { 
  file<- stack(files[i])
  
  dates <- names(file)
  dates <- paste0 (substr(dates, 2, 5), "-", paste0 (substr(dates, 7, 8), "-", paste0 (substr(dates, 10, 11), "-"), paste0 (substr(dates, 13, 14))))
  names <- paste0 ("Jordan_daily_P_MSWEPv2.8", "-", dates, ".tif")
  
  for(j in 1:nlayers(file)){
    final.files<-file[[j]]*1
    writeRaster(final.files, file.path(output, names[[j]]), overwrite=TRUE)
    
  }
  
}
stopImplicitCluster(cores) 