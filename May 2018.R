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
library(bmstdr)
library(spBayes)
library(rstan)
library(INLA)
library(inlabru)
library(zoo)
library(sp)
library(spacetime)
library(rgdal)
library(gstat)
library(ncdf4)
library(raster)
library(tiff)
library(gridExtra)
library(plotly)
library(GGally)

UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

#observed ground monitoring

All <- readRDS("Data/All.rds")

ObsAll0518 <- subset(All, Year == 2018 & Month == "05" & !is.na(PM25))

locs <- unique(ObsAll0518[c("Site Name", "Easting", "Northing")]) 
points <- unique(ObsAll0518[c("Easting", "Northing")])

ObsAll0518$`Environment Type`

ObsAll0518$industrial <- ifelse(grepl("Industrial", ObsAll0518$`Environment Type`, fixed = TRUE), 1, 0)
ObsAll0518$traffic <- ifelse(grepl("Traffic", ObsAll0518$`Environment Type`, fixed = TRUE), 1, 0)
ObsAll0518$suburban <- ifelse(grepl("Suburban", ObsAll0518$`Environment Type`, fixed = TRUE), 1, 0)
ObsAll0518$rural <- ifelse(grepl("Rurual", ObsAll0518$`Environment Type`, fixed = TRUE), 1, 0)

#raster climate

tempnc <- nc_open("Data/Climate/tas_hadukgrid_uk_1km_mon_201801-201812.nc")
precipnc <- nc_open("Data/Climate/rainfall_hadukgrid_uk_1km_mon_201801-201812.nc")
humidnc <- nc_open("Data/Climate/hurs_hadukgrid_uk_1km_mon_201801-201812.nc")


lat <- ncvar_get(tempnc, "latitude")
long <- ncvar_get(tempnc, "longitude")
month <- ncvar_get(tempnc, "month_number")

#tempnc[["var"]][["tas"]][["missval"]] <- NA
#precipnc[["var"]][["rainfall"]][["missval"]] <- NA
#humidnc[["var"]][["hurs"]][["missval"]] <- NA

temp <- ncvar_get(tempnc, "tas")
precip <- ncvar_get(precipnc, "rainfall")
humid <- ncvar_get(humidnc, "hurs")

tempMay <- temp[, , 5] 
precipMay <- precip[, , 5]
humidMay <- humid[, , 5]

temp.r <- raster(t(tempMay), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
temp.r <- flip(temp.r, direction='y')
temp.r <- projectRaster(temp.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
plot(temp.r)

precip.r <- raster(t(precipMay), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
precip.r <- flip(precip.r, direction='y')
precip.r <- projectRaster(precip.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
plot(precip.r)

humid.r <- raster(t(humidMay), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
humid.r <- flip(humid.r, direction='y')
humid.r <- projectRaster(humid.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
plot(humid.r)

UK <- spTransform(UK, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

ggplot() + 
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(data = ObsAll0518, aes(x = Easting, y = Northing, color = PM25))

temp.v <- cbind(extract(temp.r, points), points)
precip.v <- cbind(extract(precip.r, points), points)
humid.v <- cbind(extract(humid.r, points), points)


#pcm 

PCMPM25 <- read.csv("Data/Air Pollution/PCM 2018 PM2.5.csv")
PCM.r <- rasterFromXYZ(PCMPM25[,c("x", "y", "pm252018g")], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
plot(PCM.r)
#PCM.r <- projectRaster(PCM, crs = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

#aod 
AOD.r <- readRDS("Data/Air Pollution/MAIAC AOD/AOD0518.rds")
AOD.r <- projectRaster(AOD.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
plot(AOD.r)

temp.r <- resample(temp.r, PCM.r, "bilinear")
precip.r <- resample(precip.r, PCM.r, "bilinear")
humid.r <- resample(humid.r, PCM.r, "bilinear")
aod.r <- resample(AOD.r, PCM.r, "bilinear")
plot(aod.r)

#population
pop.r <- raster("Data/Population/gbr_ppp_2018_1km_Aggregated.tif")
pop.r <- projectRaster(pop.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
population <- as(pop.r ,'SpatialPolygonsDataFrame')
population <- st_as_sf(population, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

pop.r <- resample(pop.r, PCM.r, "bilinear")

temp_df <- as.data.frame(temp.r, xy = TRUE)
precip_df <- as.data.frame(precip.r, xy = TRUE)
humid_df <- as.data.frame(humid.r, xy = TRUE)
pcm_df <- as.data.frame(PCM.r, xy = TRUE)
aod_df <- as.data.frame(aod.r, xy = TRUE)
pop_df <- as.data.frame(pop.r, xy = TRUE)

aod.v <- cbind(extract(aod.r, points), points)

gtemp <- ggplot() + 
  geom_raster(data = temp_df, aes(x = x, y = y, fill = layer)) + 
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 13.8, na.value = NA) +
  scale_fill_viridis_c(option = "B", na.value = NA) +
  theme(legend.position = "none") +
  ggtitle("Temperature") +
  xlim(c(-50000,720000)) +
  ylim(c(-1000,1200000)) +
  coord_equal()


gprecip <- ggplot() + 
  geom_raster(data = precip_df, aes(x = x, y = y, fill = layer)) + 
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 60.8 , na.value = NA) +
  scale_fill_viridis_c(option = "B", na.value = NA) +
  theme(legend.position = "none") +
  ggtitle("Precipitation") +
  xlim(c(-50000,720000)) +
  ylim(c(-1000,1200000)) +
  coord_equal()

ghumid <- ggplot() + 
  geom_raster(data = humid_df, aes(x = x, y = y, fill = layer)) + 
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 73.4, na.value = NA) +
  scale_fill_viridis_c(option = "B", na.value = NA) +
  theme(legend.position = "none") +
  ggtitle("Humidity") +
  xlim(c(-50000,720000)) +
  ylim(c(-1000,1200000)) +
  coord_equal()


gpcm <- ggplot() + 
  geom_raster(data = pcm_df, aes(x = x, y = y, fill = pm252018g)) + 
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 73.4, na.value = NA) +
  scale_fill_viridis_c(option = "B", na.value = NA) +
  theme(legend.position = "none") +
  ggtitle("PCM") +
  xlim(c(-50000,720000)) +
  ylim(c(-1000,1200000)) +
  coord_equal()

gaod <- ggplot() + 
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = aod_df, aes(x = x, y = y, fill = layer, alpha = 0.9)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.18) +
  coord_equal()

gpop <- ggplot() + 
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = pop_df, aes(x = x, y = y, fill = layer, alpha = 0.9)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.18) +
  coord_equal()

grid.arrange(gtemp, gprecip, ghumid, nrow = 1)

#, gpcm, gaod, gpop)

climate <- merge(temp_df, precip_df, by = c("x","y"))
climate <- merge(climate, humid_df, by = c("x","y"))
colnames(climate) <- c("x", "y", "temp", "precip", "humid")

climate.pcm <- merge(climate, pcm_df, by = c("x", "y"), all.x = TRUE, all.y = TRUE)


plot(climate$x, climate$y, xlim = c(0,20000), ylim = c(0,200000))
points(pcm_df$x, pcm_df$y, col = "red")
points(aod_df$x, aod_df$y, col = "blue")
points(pop_df$x, pop_df$y, col = "yellow")

ggpairs(climate.pcm, columns = c("temp", "precip", "humid", "pm252018g"))

all <- merge(climate.pcm, aod_df, by = c("x", "y"), all.x = TRUE, all.y = TRUE)

ggpairs(all, columns = c("temp", "precip", "humid", "pm252018g", "layer"))

complete <- na.omit(all)
fitaod <- lm(pm252018g ~ poly(layer,3), data = complete)
summary(fitaod)
plot(complete$layer, complete$pm252018g)
points(complete$layer, predict(fitaod), col = "red")


all <- merge(all, pop_df, by = c("x", "y"), all.x = TRUE, all.y = TRUE)

saveRDS(all, file = "Data/May2018.rds")

#fit at monitoring sites
variables.r <- stack(temp.r, precip.r, humid.r, PCM.r, aod.r, pop.r)
obs.variables <- raster::extract(variables.r, points)

fulldata <- cbind(ObsAll0518$PM25, ObsAll0518$NO2, obs.variables, points)
colnames(fulldata) <- c("pm25", "no2","temp", "precip", "humid", "pcm", "aod", "pop", "x", "y")
fulldata

saveRDS(fulldata, file = "Data/ObsMay2018.rds")

fulldata <- readRDS("Data/ObsMay2018.rds")



