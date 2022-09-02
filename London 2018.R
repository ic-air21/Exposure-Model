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

ObsAll18 <- subset(ObsAll, Year == 2018 & !is.na(PM25))

locs <- unique(ObsAll18[c("Site Name", "Easting", "Northing")]) 
points <- unique(ObsAll18[c("Easting", "Northing")])

ObsAll18$`Environment Type`

ObsAll18$industrial <- ifelse(grepl("Industrial", ObsAll18$`Environment Type`, fixed = TRUE), 1, 0)
ObsAll18$traffic <- ifelse(grepl("Traffic", ObsAll18$`Environment Type`, fixed = TRUE), 1, 0)
ObsAll18$suburban <- ifelse(grepl("Suburban", ObsAll18$`Environment Type`, fixed = TRUE), 1, 0)

saveRDS(ObsAll18, file = "Data/ObsAll18.rds")

ObsAll18 <- readRDS(file = "Data/ObsAll18.rds")

temp.r <- stack("Data/Climate/temp2.tif")
humid.r <- stack("Data/Climate/humid2.tif")
precip.r <- stack("Data/Climate/precip2.tif")

temp2018.r <- temp.r[[61:72]]
humid2018.r <- humid.r[[61:72]]
precip2018.r <- precip.r[[61:72]]

#pcm 
PCMPM25 <- read.csv("Data/Air Pollution/PCM 2018 PM2.5.csv")
PCMPM25.r <- rasterFromXYZ(PCMPM25[,c("x", "y", "pm252018g")], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

PCMNO2 <- read.csv("Data/Air Pollution/NO2 PCM 2005-2020/PCM 2018.csv")
PCMNO2.r <- rasterFromXYZ(PCMNO2[,c("x", "y", "no22018")], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

#population
pop.r <- raster("Data/Population/gbr_ppp_2018_1km_Aggregated.tif")
pop.r <- projectRaster(pop.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
population <- as(pop.r ,'SpatialPolygonsDataFrame')
population <- st_as_sf(population, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

temp.r <- resample(temp2018.r, PCMPM25.r, "bilinear")
precip.r <- resample(precip2018.r, PCMPM25.r, "bilinear")
humid.r <- resample(humid2018.r, PCMPM25.r, "bilinear")

pop.r <- resample(pop.r, PCMPM25.r, "bilinear")

AOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD18.rds")
AOD.list <- list()
AOD.r <- list()
for (i in 1:12) {
  AOD.list[[i]] <- subset(AOD, month = i)
  AOD.r[[i]] <- rasterFromXYZ(AOD.list[[i]][,c("x", "y", "AOD")], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
  AOD.r[[i]] <- resample(AOD.r[[i]], PCMPM25.r, "bilinear")
}
AOD.r <- stack(AOD.r)

SNO2.r <- stack(lapply(Sys.glob("Z:/home/Satellite NO2/OMI_trno2_0.10x0.10_2018*_Col3_V4.tif"), raster))
proj4string(SNO2.r) <- crs("+proj=longlat +datum=WGS84 +no_defs")
SNO2.r <- projectRaster(SNO2.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
SNO2.r <- resample(SNO2.r, PCMPM25.r, "bilinear")
SNO2.r <- stack(SNO2.r)
London@bbox

#cropping
temp.london <- crop(temp.r, London)
precip.london <- crop(precip.r, London)
humid.london <- crop(humid.r, London)
pcmpm25.london <- crop(PCMPM25.r, London)
pcmno2.london <- crop(PCMNO2.r, London)
aod.london <- crop(AOD.r, London)
sno2.london <- crop(SNO2.r, London)
pop.london <- crop(pop.r, London)

library(reshape2)
temp_df <- as.data.frame(temp.london, xy = TRUE)
temp_df <- melt(temp_df, id.vars = 1:2, variable.name = "month")
temp_df$month <- as.numeric(as.factor(temp_df$month))
names(temp_df)[4] <- "temp"

precip_df <- as.data.frame(precip.london, xy = TRUE)
precip_df <- melt(precip_df, id.vars = 1:2, variable.name = "month")
precip_df$month <- as.numeric(as.factor(precip_df$month))
names(precip_df)[4] <- "precip"

humid_df <- as.data.frame(humid.london, xy = TRUE)
humid_df <- melt(humid_df, id.vars = 1:2, variable.name = "month")
humid_df$month <- as.numeric(as.factor(humid_df$month))
names(humid_df)[4] <- "humid"

pcmpm25_df <- as.data.frame(pcmpm25.london, xy = TRUE)
names(pcmpm25_df)[3] <- "pcmpm25"

pcmno2_df <- as.data.frame(pcmno2.london, xy = TRUE)
names(pcmno2_df)[3] <- "pcmno2"

aod_df <- as.data.frame(aod.london, xy = TRUE)
aod_df <- melt(aod_df, id.vars = 1:2, variable.name = "month")
aod_df$month <- as.numeric(as.factor(aod_df$month))
names(aod_df)[4] <- "aod"

sno2_df <- as.data.frame(sno2.london, xy = TRUE)
sno2_df <- melt(sno2_df, id.vars = 1:2, variable.name = "month")
sno2_df$month <- as.numeric(as.factor(sno2_df$month))
names(sno2_df)[4] <- "sno2"

pop_df <- as.data.frame(pop.london, xy = TRUE)
names(pop_df)[3] <- "pop"

climate <- merge(temp_df, precip_df, by = c("x","y","month"))
climate <- merge(climate, humid_df, by = c("x","y","month"))

pcm <- merge(pcmpm25_df, pcmno2_df, by = c("x","y"), all.x = TRUE, all.y = TRUE)
climate.pcm <- merge(climate, pcm, by = c("x", "y"), all.x = TRUE, all.y = TRUE)

satellite <- merge(aod_df, sno2_df, by = c("x", "y", "month"), all.x = TRUE, all.y = TRUE)
pop <- merge(satellite, pop_df, by = c("x","y"), all.x = TRUE, all.y = TRUE)

all <- merge(climate.pcm, pop, by = c("x", "y", "month"), all.x = TRUE, all.y = TRUE)

saveRDS(all, "Data/London 2018.rds")
