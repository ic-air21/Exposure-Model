library(readxl)
library(ggplot2)
library(reshape2)
library(scico)
library(dplyr)
library(tidyr)
library(ggmap)
library(sf)
library(rgdal)
library(xts)

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)

library(bmstdr)
library(spBayes)
library(rstan)
library(zoo)
library(sp)
library(spacetime)
library(gstat)
library(ncdf4)
library(raster)
library(tiff)
library(gridExtra)
library(plotly)
library(GGally)

#observed ground monitoring

ObsAll <- readRDS("Data/LondonAll.rds")

ObsAll0518 <- subset(ObsAll, Year == 2018 & !is.na(PM25))

locs <- unique(ObsAll0518[c("Site Name", "Easting", "Northing")]) 
points <- unique(ObsAll0518[c("Easting", "Northing")])

ObsAll0518$`Environment Type`

ObsAll0518$industrial <- ifelse(grepl("Industrial", ObsAll0518$`Environment Type`, fixed = TRUE), 1, 0)
ObsAll0518$traffic <- ifelse(grepl("Traffic", ObsAll0518$`Environment Type`, fixed = TRUE), 1, 0)
ObsAll0518$suburban <- ifelse(grepl("Suburban", ObsAll0518$`Environment Type`, fixed = TRUE), 1, 0)

saveRDS(ObsAll0518, file = "Data/ObsAll0518.rds")

ObsAll0518 <- readRDS(file = "Data/ObsAll0518.rds")
#raster climate

tempnc <- nc_open("Data/Climate/tas_hadukgrid_uk_1km_mon_201801-201812.nc")
precipnc <- nc_open("Data/Climate/rainfall_hadukgrid_uk_1km_mon_201801-201812.nc")
humidnc <- nc_open("Data/Climate/hurs_hadukgrid_uk_1km_mon_201801-201812.nc")


lat <- ncvar_get(tempnc, "latitude")
long <- ncvar_get(tempnc, "longitude")
month <- ncvar_get(tempnc, "month_number")

temp <- ncvar_get(tempnc, "tas")
precip <- ncvar_get(precipnc, "rainfall")
humid <- ncvar_get(humidnc, "hurs")

temp[temp == fillvalue$value] <- NA
precip[precip == fillvalue$value] <- NA
humid[humid == fillvalue$value] <- NA

tempMay <- temp[, , 5] 
precipMay <- precip[, , 5]
humidMay <- humid[, , 5]

temp.r <- raster(t(tempMay), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
temp.r <- raster::flip(temp.r, direction='y')
temp.r <- projectRaster(temp.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
plot(temp.r)
saveRDS(temp.r, "Data/temp.rds")

precip.r <- raster(t(precipMay), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
precip.r <- raster::flip(precip.r, direction='y')
precip.r <- projectRaster(precip.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
plot(precip.r)

humid.r <- raster(t(humidMay), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
humid.r <- raster::flip(humid.r, direction='y')
humid.r <- projectRaster(humid.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
plot(humid.r)

London <- spTransform(London, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

ggplot() + 
  #geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(data = ObsAll0518, aes(x = Easting, y = Northing, color = PM25), group = 1) +
  geom_point(aes(x = 540000, y = 180000), color = "red", shape = 4, group = 2) +
  geom_line() +
  xlim(c(525000, 550000)) 

#add lines pls

temp.v <- cbind(extract(temp.r, points), points)
precip.v <- cbind(extract(precip.r, points), points)
humid.v <- cbind(extract(humid.r, points), points)


#pcm 

PCMPM25 <- read.csv("Data/Air Pollution/PCM 2018 PM2.5.csv")
PCM.r <- rasterFromXYZ(PCMPM25[,c("x", "y", "pm252018g")], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
plot(PCM.r)
#PCM.r <- projectRaster(PCM, crs = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
saveRDS(PCM.r, "Data/pcm.rds")


#aod 
AOD.r <- readRDS("Data/Air Pollution/MAIAC AOD/AOD0518.rds")
AOD.r <- projectRaster(AOD.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

#population
pop.r <- raster("Data/Population/gbr_ppp_2018_1km_Aggregated.tif")
pop.r <- projectRaster(pop.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
population <- as(pop.r ,'SpatialPolygonsDataFrame')
population <- st_as_sf(population, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

temp.r <- resample(temp.r, PCM.r, "bilinear")
precip.r <- resample(precip.r, PCM.r, "bilinear")
humid.r <- resample(humid.r, PCM.r, "bilinear")
aod.r <- resample(AOD.r, PCM.r, "bilinear")
pop.r <- resample(pop.r, PCM.r, "bilinear")

London@bbox

#cropping
temp.london <- crop(temp.r, London)
precip.london <- crop(precip.r, London)
humid.london <- crop(humid.r, London)
pcm.london <- crop(PCM.r, London)
aod.london <- crop(aod.r, London)
pop.london <- crop(pop.r, London)

temp_df <- as.data.frame(temp.london, xy = TRUE)
precip_df <- as.data.frame(precip.london, xy = TRUE)
humid_df <- as.data.frame(humid.london, xy = TRUE)
pcm_df <- as.data.frame(pcm.london, xy = TRUE)
aod_df <- as.data.frame(aod.london, xy = TRUE)
pop_df <- as.data.frame(pop.london, xy = TRUE)

gtemp <- ggplot() + 
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = temp_df, aes(x = x, y = y, fill = layer, alpha = 0.9)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 13.8) +
  coord_equal()

gprecip <- ggplot() + 
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = precip_df, aes(x = x, y = y, fill = layer, alpha = 0.9)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 60.8) +
  coord_equal()

ghumid <- ggplot() + 
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = humid_df, aes(x = x, y = y, fill = layer, alpha = 0.9)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 73.4) +
  coord_equal()

gpcm <- ggplot() + 
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = pcm_df, aes(x = x, y = y, fill = pm252018g, alpha = 0.9)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 11.3) +
  coord_equal()

gaod <- ggplot() + 
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = aod_df, aes(x = x, y = y, fill = layer, alpha = 0.9)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.13) +
  coord_equal()

gpop <- ggplot() + 
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = pop_df, aes(x = x, y = y, fill = gbr_ppp_2018_1km_Aggregated, alpha = 0.9)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 2075) +
  coord_equal()


grid.arrange(gtemp, gprecip, ghumid, gpcm, gaod, gpop)

climate <- merge(temp_df, precip_df, by = c("x","y"))
climate <- merge(climate, humid_df, by = c("x","y"))
colnames(climate) <- c("x", "y", "temp", "precip", "humid")
plot(climate)

climate.pcm <- merge(climate, pcm_df, by = c("x", "y"), all.x = TRUE, all.y = TRUE)
plot(climate.pcm)
climate.pcm

plot(climate$x, climate$y)
points(pcm_df$x, pcm_df$y, col = "red")

ggpairs(climate.pcm, columns = c("temp", "precip", "humid", "pm252018g"))



climate.london <- stack(temp.london, precip.london, humid.london, pcm.london, aod.london, pop.london)
climate.london <- as.data.frame(climate.london, xy = TRUE)
colnames(climate.london) <- c( "x", "y","temp", "precip", "humid", "pcm", "aod", "pop")
saveRDS(climate.london, "Data/London May 2018.rds")
fullclimate <- na.omit(climate.df)
plot(climate.london$x, climate.london$y)

obsdata <- cbind(ObsAll0518$PM25, ObsAll0518$NO2, raster::extract(climate.r, points), points)
colnames(obsdata) <- c("pm25", "no2","temp", "precip", "humid", "pcm", "aod", "pop", "x", "y")
fulldata


fit1 <- lm(pm25 ~ no2 + temp + precip + humid + pcm, data = fulldata)
fit1
summary(fit1)
scatter.smooth(x = fulldata$pcm, y = fulldata$pm25)

plot(fulldata$pcm, fulldata$pm25)
points(fulldata$pcm, predict(fit1), col = "red")

saveRDS(obsdata, "Data/London May 2018 Obs.rds")
