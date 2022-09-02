#cluster on pcm
#pcm 
library(raster)
library(readxl)
setwd("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1")



PCM <- list()
PCM.r <- list()
PCM.df <- list()

pcm.box <- PCM.r[[1]]
for(i in 1:16) {
  PCM[[i]] <- read.csv(paste("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1/Data/Air Pollution/NO2 PCM 2005-2020/PCM ",2004 + i,".csv", sep = ""))[,-1]
  colnames(PCM[[i]]) <- c("x", "y", "PM2.5")
  PCM[[i]]$PM2.5 <- as.numeric(PCM[[i]]$PM2.5)
  
  PCM.r[[i]] <- rasterFromXYZ(PCM[[i]][,c("x", "y", "PM2.5")], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")
  PCM.df[[i]] <- as.data.frame(PCM.r[[i]], xy = TRUE)
}

PCM.r[[16]] <- crop(PCM.r[[16]], PCM.r[[15]])
PCM.r <- stack(PCM.r, layers=NULL)


extent(na.omit(PCM.r[[1]]))

writeRaster(PCM.r, filename="C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1/Data/PM25 PCM.tif", options="INTERLEAVE=BAND", overwrite=TRUE)


Pollution <- readRDS("Data/More Site Data.rds")
sites <- unique(Pollution[c("Easting", "Northing", "Site Name")])
n.sites <- as.numeric(nrow(sites))

pcm <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 1:16) {
  
  year <- rep(i + 2004, n.sites) 
  
  pcm.month <- PCM.r[[i]]
  pcm.month <- projectRaster(pcm.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  pcm.points <- raster::extract(pcm.month, sites[,c(1,2)])
  pcm.sites <- cbind(sites[,3], year, pcm.points)
  pcm <- rbind(pcm, pcm.sites)
}
names(pcm)[1] <- "Site Name"
pcm$year <- as.numeric(pcm$year)
names(pcm)[3] <- "NO2 PCM"
  
Pollution <- merge(Pollution, pcm, by = c("Site Name", "year"))

Pollution[,44] <- as.numeric(Pollution[,44])

saveRDS(Pollution, "Data/Even More Site Data.rds")



plot(PCM.r[[1]])

library(rgdal)
library(sp)
library(sf)
library(terra)

shp_GB <- readOGR("Data/UK Shapefile/GBR_adm/GBR_adm0.shp")
GB <- spTransform(shp_GB, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

library(ggplot2)
library(rasterVis)
ggplot() + 
    geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
    geom_raster(data = PCM.df, aes(x = x, y = y, fill = PM2.5, alpha = 0.9)) + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 11.3) +
    facet_wrap(~ Layer) +
    coord_equal()   



