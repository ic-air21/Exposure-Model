library(readxl)
library(ggplot2)
library(reshape2)
library(scico)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)
library(raster)
library(rqdatatable)
library(mgcv)

setwd("Z:/home")

sno2.r <- lapply(Sys.glob("Satellite NO2/OMI_trno2_0.10x0.10_*_Col3_V4.tif"), stack)

setwd("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1")

Pollution <- readRDS("Data/All.rds")

Pollution$Month <- as.numeric(Pollution$Month)
Pollution$time <- (Pollution$Year - 2005)*12 + Pollution$Month

sites <- unique(Pollution[c("Easting", "Northing", "Site Name")])
n.sites <- as.numeric(nrow(sites))

sno2 <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 1:192) {
  
  time <- rep(i, n.sites)
  
  sno2.month <- sno2.r[[i]]
  sno2.month <- projectRaster(sno2.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  sno2.points <- raster::extract(sno2.month, sites[,c(1,2)])
  sno2.sites <- cbind(sites, time, sno2.points)
  sno2 <- rbind(sno2, sno2.sites)
}
sno2 <- sno2[,c(3,4,5)]

Climate.Pollution.Population.PCM.BLH <- readRDS("Data/All Site Data.rds")
Climate.Pollution.Population.PCM.BLH <- Climate.Pollution.Population.PCM.BLH[,c(1,2,4:14)]

Climate.Pollution.Population.PCM.BLH.SNO2 <- merge(Climate.Pollution.Population.PCM.BLH, sno2, by = c("Site Name", "time"))
Climate.Pollution.Population.PCM.BLH.SNO2 <- merge(Climate.Pollution.Population.PCM.BLH.SNO2, AURNSites, by = "Site Name")
names(Climate.Pollution.Population.PCM.BLH.SNO2)[10] <- "AOD47"
names(Climate.Pollution.Population.PCM.BLH.SNO2)[14] <- "Satellite NO2"

saveRDS(Climate.Pollution.Population.PCM.BLH.SNO2, "Data/All Site Data.rds")
