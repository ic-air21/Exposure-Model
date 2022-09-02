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

setwd("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1")
UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

Pollution <- readRDS("Data/All.rds")

Pollution$Month <- as.numeric(Pollution$Month)
Pollution$time <- (Pollution$Year - 2005)*12 + Pollution$Month

sites <- unique(Pollution[c("Easting", "Northing", "Site Name")])
n.sites <- as.numeric(nrow(sites))

aod2005 <- list()
#2005
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2005)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"05.tif"))[[2]]
  
  time <- rep(i, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2005[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2005[[1]], aod2005[[2]], aod2005[[3]], aod2005[[4]], aod2005[[5]], aod2005[[6]], aod2005[[7]], aod2005[[8]], aod2005[[9]], aod2005[[10]], aod2005[[11]], aod2005[[12]])

Pollution2005 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2006 <- list()
#2006
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2006)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"06.tif"))[[2]]
  
  time <- rep(i + 12, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2006[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2006[[1]], aod2006[[2]], aod2006[[3]], aod2006[[4]], aod2006[[5]], aod2006[[6]], aod2006[[7]], aod2006[[8]], aod2006[[9]], aod2006[[10]], aod2006[[11]], aod2006[[12]])

Pollution2006 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2007 <- list()
#2007
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2007)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"07.tif"))[[2]]
  
  time <- rep(i + 24, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2007[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2007[[1]], aod2007[[2]], aod2007[[3]], aod2007[[4]], aod2007[[5]], aod2007[[6]], aod2007[[7]], aod2007[[8]], aod2007[[9]], aod2007[[10]], aod2007[[11]], aod2007[[12]])

Pollution2007 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2008 <- list()
#2008
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2008)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"08.tif"))[[2]]
  
  time <- rep(i + 36, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2008[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2008[[1]], aod2008[[2]], aod2008[[3]], aod2008[[4]], aod2008[[5]], aod2008[[6]], aod2008[[7]], aod2008[[8]], aod2008[[9]], aod2008[[10]], aod2008[[11]], aod2008[[12]])

Pollution2008 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2009 <- list()
#2009
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2009)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"09.tif"))[[2]]
  
  time <- rep(i + 48, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2009[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2009[[1]], aod2009[[2]], aod2009[[3]], aod2009[[4]], aod2009[[5]], aod2009[[6]], aod2009[[7]], aod2009[[8]], aod2009[[9]], aod2009[[10]], aod2009[[11]], aod2009[[12]])

Pollution2009 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")


aod2010 <- list()
#2005
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2010)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"10.tif"))[[2]]
  
  time <- rep(i + 60, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2010[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2010[[1]], aod2010[[2]], aod2010[[3]], aod2010[[4]], aod2010[[5]], aod2010[[6]], aod2010[[7]], aod2010[[8]], aod2010[[9]], aod2010[[10]], aod2010[[11]], aod2010[[12]])

Pollution2010 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")


aod2011 <- list()
#2011
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2011)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"11.tif"))[[2]]
  
  time <- rep(i + 72, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2011[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2011[[1]], aod2011[[2]], aod2011[[3]], aod2011[[4]], aod2011[[5]], aod2011[[6]], aod2011[[7]], aod2011[[8]], aod2011[[9]], aod2011[[10]], aod2011[[11]], aod2011[[12]])

Pollution2011 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2012 <- list()
#2012
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2012)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"12.tif"))[[2]]
  
  time <- rep(i + 84, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2012[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2012[[1]], aod2012[[2]], aod2012[[3]], aod2012[[4]], aod2012[[5]], aod2012[[6]], aod2012[[7]], aod2012[[8]], aod2012[[9]], aod2012[[10]], aod2012[[11]], aod2012[[12]])

Pollution2012 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2013 <- list()
#2013
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2013)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"13.tif"))[[2]]
  
  time <- rep(i + 96, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2013[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2013[[1]], aod2013[[2]], aod2013[[3]], aod2013[[4]], aod2013[[5]], aod2013[[6]], aod2013[[7]], aod2013[[8]], aod2013[[9]], aod2013[[10]], aod2013[[11]], aod2013[[12]])

Pollution2013 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2014 <- list()
#2014
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2014)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"14.tif"))[[2]]
  
  time <- rep(i + 108, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2014[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2014[[1]], aod2014[[2]], aod2014[[3]], aod2014[[4]], aod2014[[5]], aod2014[[6]], aod2014[[7]], aod2014[[8]], aod2014[[9]], aod2014[[10]], aod2014[[11]], aod2014[[12]])

Pollution2014 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2015 <- list()
#2015
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2015)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"15.tif"))[[2]]
  
  time <- rep(i + 120, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2015[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2015[[1]], aod2015[[2]], aod2015[[3]], aod2015[[4]], aod2015[[5]], aod2015[[6]], aod2015[[7]], aod2015[[8]], aod2015[[9]], aod2015[[10]], aod2015[[11]], aod2015[[12]])

Pollution2015 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2016 <- list()
#2016
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2016)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"16.tif"))[[2]]
  
  time <- rep(i + 132, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2016[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2016[[1]], aod2016[[2]], aod2016[[3]], aod2016[[4]], aod2016[[5]], aod2016[[6]], aod2016[[7]], aod2016[[8]], aod2016[[9]], aod2016[[10]], aod2016[[11]], aod2016[[12]])

Pollution2016 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2017 <- list()
#2017
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2017)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"17.tif"))[[2]]
  
  time <- rep(i + 144, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2017[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2017[[1]], aod2017[[2]], aod2017[[3]], aod2017[[4]], aod2017[[5]], aod2017[[6]], aod2017[[7]], aod2017[[8]], aod2017[[9]], aod2017[[10]], aod2017[[11]], aod2017[[12]])

Pollution2017 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2018 <- list()
#2018
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2018)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"18.tif"))[[2]]
  
  time <- rep(i + 156, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2018[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2018[[1]], aod2018[[2]], aod2018[[3]], aod2018[[4]], aod2018[[5]], aod2018[[6]], aod2018[[7]], aod2018[[8]], aod2018[[9]], aod2018[[10]], aod2018[[11]], aod2018[[12]])

Pollution2018 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2019 <- list()
#2019
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2019)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"19.tif"))[[2]]
  
  time <- rep(i + 168, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2019[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2019[[1]], aod2019[[2]], aod2019[[3]], aod2019[[4]], aod2019[[5]], aod2019[[6]], aod2019[[7]], aod2019[[8]], aod2019[[9]], aod2019[[10]], aod2019[[11]], aod2019[[12]])

Pollution2019 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

aod2020 <- list()
#2020
setwd("Z:/home")
for (i in 1:12) {
  Pollution.subset <- subset(Pollution, Year == 2020)
  
  aod.points <- c(rep(NA, n.sites))
  
  aod <- list()
  aod.r <- stack(paste("Satellite AOD/AOD",i,"20.tif"))[[2]]
  
  time <- rep(i + 180, n.sites)
  
  aod.month <- aod.r
  aod.month <- projectRaster(aod.month, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
  
  aod.points <- raster::extract(aod.month, sites[,c(1,2)])
  aod2020[[i]] <- cbind(sites, time, aod.points)
}

aod.points <- rbind(aod2020[[1]], aod2020[[2]], aod2020[[3]], aod2020[[4]], aod2020[[5]], aod2020[[6]], aod2020[[7]], aod2020[[8]], aod2020[[9]], aod2020[[10]], aod2020[[11]], aod2020[[12]])

Pollution2020 <- natural_join(Pollution.subset, aod.points, by = c("Site Name", "time"), jointype = "FULL")

Pollution <- rbind(Pollution2005, Pollution2006, Pollution2007, Pollution2008, Pollution2009, Pollution2010, Pollution2011, Pollution2012, Pollution2013, Pollution2014, Pollution2015, Pollution2016, Pollution2017, Pollution2018, Pollution2019, Pollution2020)
saveRDS(Pollution, "Data/AOD 55.rds")


Climate.Pollution.Population.PCM.BLH.SNO2.AOD <- merge(Climate.Pollution.Population.PCM.BLH,SNO2, Pollution, by = c("Site Name", "time"))
Climate.Pollution.Population.PCM.BLH.SNO2.AOD <- merge(Climate.Pollution.Population.PCM.BLH.SNO2.AOD, AURNSites, by = "Site Name")
names(Climate.Pollution.Population.PCM.BLH.SNO2.AOD)[""] <- "aod.55"
saveRDS(Climate.Pollution.Population.PCM.BLH.SNO2.AOD, "Data/All Site Data.rds")