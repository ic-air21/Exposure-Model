library(raster)
library(dplyr)
library(ncdf4)
library(maptools)
library(fields)
library(gdalUtils)
library(rgdal)
library(ggplot2)

UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
UK <- spTransform(UK, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp")
London <- spTransform(London, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
London <- spTransform(London, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")


AOD.r <- readRDS("Data/Air Pollution/MAIAC AOD/AOD0518.rds")
AOD.r <- projectRaster(AOD.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
plot(AOD.r)

aod_df <- as.data.frame(AOD.r, xy = TRUE)

hist(aod_df$layer)

ggplot() + 
  geom_raster(data = aod_df, aes(x = x, y = y, fill = layer)) + 
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.18) +
  scale_fill_viridis_c(option = "B", na.value = NA, alpha = 0.9) +
  theme(legend.position = "none") +
  coord_equal()
