library(raster)
library(dplyr)
library(ncdf4)
library(maptools)
library(fields)
library(gdalUtils)
library(rgdal)

satellitedata17 <- lapply(Sys.glob("Data/Air Pollution/MAIAC AOD/May 2018/MCD19A2.A2018*h17*.hdf"), get_subdatasets)
satellitedata18 <- lapply(Sys.glob("Data/Air Pollution/MAIAC AOD/May 2018/MCD19A2.A2018*h18*.hdf"), get_subdatasets)

satelliterasters17 = list()
satellitedata17[[1]][1]

for (i in 1:length(satellitedata17)) {
  dir.create("Data/Air Pollution/MAIAC AOD/May 2018/Tile17")
  gdal_translate(satellitedata17[[i]][1], dst_dataset = paste("Data/Air Pollution/MAIAC AOD/May 2018/Tile17/satellitetif17", i, ".tif"))
  satelliterasters17[[i]] <- raster(paste("Data/Air Pollution/MAIAC AOD/May 2018/Tile17/satellitetif17", i, ".tif"))
}

rasters17 <- stack(satelliterasters17)
rasters17 <- flip(rasters17, direction = 'y')
mean17 <- calc(rasters17, fun = mean, na.rm = T)
unlink("Data/Air Pollution/MAIAC AOD/May 2018/Tile17", recursive = TRUE)

satelliterasters18 = list()

for (i in 1:length(satellitedata18)) {
  dir.create("Data/Air Pollution/MAIAC AOD/May 2018/Tile18")
  gdal_translate(satellitedata18[[i]][1], dst_dataset = paste("Data/Air Pollution/MAIAC AOD/May 2018/Tile18/satellitetif18", i, ".tif"))
  satelliterasters18[[i]] <- raster(paste("Data/Air Pollution/MAIAC AOD/May 2018/Tile18/satellitetif18", i, ".tif"))
}

rasters18 <- stack(satelliterasters18)
rasters18 <- flip(rasters18, direction = 'y')
mean18 <- calc(rasters18, fun = mean, na.rm = T)
unlink("Data/Air Pollution/MAIAC AOD/May 2018/Tile18", recursive = TRUE)

AOD0518 <- mosaic(mean17, mean18, fun = mean)
extent(AOD0518)

setcrs <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"

plot(AOD0518)
crs(AOD0518.r) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
AOD0518.r <- projectRaster(AOD0518, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2"))
AOD0518.spdf <- as(AOD0518.r, "SpatialPixelsDataFrame")
AOD0518.df <- as.data.frame(AOD0518.spdf)

saveRDS(AOD0518.r, file = "Data/Air Pollution/MAIAC AOD/AOD0518.rds")

AOD0518.r <- readRDS(file = "Data/Air Pollution/MAIAC AOD/AOD0518.rds")
crs(AOD0518.r) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
AOD0518.r <- projectRaster(AOD0518.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
AOD0518.spdf <- as(AOD0518.r, "SpatialPixelsDataFrame")
AOD0518.df <- as.data.frame(AOD0518.spdf)

UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))


library(ggplot2)
ggplot() + 
  geom_tile(data = AOD0518.df, aes(x = x, y = y, fill=layer, alpha = 0.8)) +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) 

day14.17 <- flip(satelliterasters17[[14]], direction = 'y')
day14.18 <- flip(satelliterasters18[[14]], direction = 'y')
day14 <- mosaic(day14.17, day14.18, fun = mean)
plot(day14)

par(mfrow=c(2,2))
plot(satelliterasters17[[2]])
plot(satelliterasters17[[10]])
plot(satelliterasters17[[14]])
plot(satelliterasters17[[25]])
