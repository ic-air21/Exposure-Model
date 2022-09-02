#cropping aod to greater london
# bbox x 503568.2 561957.5 y 155850.8 200933.9
library(raster)



bbox <- extent(503568.2,561957.5, 155850.8, 200933.9)

setwd("Z:/home")

AOD <- list()
LAOD <- list()
LAOD.df <- list()
LAOD.DF <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("x", "y", "AOD", "year", "month")
colnames(LAOD.DF) <- x

for (i in 1:12) {
  AOD[[i]] <- raster(paste("Satellite AOD/AOD", i, "15.tif"))
  AOD[[i]] <- projectRaster(AOD[[i]], crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
  LAOD[[i]] <- raster::crop(AOD[[i]], bbox)
  LAOD.df[[i]] <- as.data.frame(LAOD[[i]], xy = TRUE)
  LAOD.df[[i]]$year <- 2015
  LAOD.df[[i]]$month <- i
  names(LAOD.df[[i]])[3] <- "AOD"
  LAOD.DF <- rbind(LAOD.DF, LAOD.df[[i]])
}

setwd("~/PhD/R/Objective 1")
saveRDS(LAOD.DF, "Data/Air Pollution/MAIAC AOD/LAOD15.rds")
LAOD15 <- LAOD.DF

LAOD05 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD05.rds")
LAOD06 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD06.rds")
LAOD07 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD07.rds")
LAOD08 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD08.rds")
LAOD09 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD09.rds")
LAOD10 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD10.rds")
LAOD11 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD11.rds")
LAOD12 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD12.rds")
LAOD13 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD13.rds")
LAOD14 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD14.rds")
LAOD15 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD15.rds")
LAOD16 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD16.rds")
LAOD17 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD17.rds")
LAOD18 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD18.rds")
LAOD19 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD19.rds")
LAOD20 <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD20.rds")

LAOD <- rbind(LAOD05, LAOD06, LAOD07, LAOD08, LAOD09, LAOD10, LAOD11, LAOD12, LAOD13, LAOD14, LAOD15, LAOD16, LAOD17, LAOD18, LAOD19, LAOD20)
saveRDS(LAOD, "Data/Air Pollution/MAIAC AOD/LAOD.rds")
LAOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD.rds")
LAOD <- LAOD[,c(3,1,2,4,5)]

library(dplyr)
library(xts)
library(sp)

LAOD$dates <- paste(as.character(LAOD$year), LAOD$month, sep = "-")
LAOD$date <- LAOD$dates %>% as.yearmon()
time <- LAOD$date
time_part <- unique(time)
time_part <- sort(time_part)

locs <- unique(LAOD[c("x","y")]) 
spat_part <- SpatialPoints(coords = locs)


LAODst <- STFDF(sp=spat_part, time=time_part, data=LAOD)
saveRDS(LAODst, "Data/Air Pollution/MAIAC AOD/LAODst.rds")
