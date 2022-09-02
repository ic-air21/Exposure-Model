library(sf)
library(rgdal)
library(raster)
library(rqdatatable)

#Adjusted London AOD

sitedata <- readRDS("Data/London Site Data.rds")

sites <- unique(sitedata[c("Easting", "Northing")])
n.sites <- as.numeric(nrow(sites))

AOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD OK DF.rds")

AOD.r <- list()
for (i in 1:192) {
  AOD.month <- subset(AOD, time == i)
  AOD.month <- AOD.month[,c(1,2,4)]
  coordinates(AOD.month) <- ~ x + y
  gridded(AOD.month) <- TRUE
  AOD.r[[i]] <- raster(AOD.month)
}
AOD.r <- stack(AOD.r)
saveRDS(AOD.r, "Data/Air Pollution/MAIAC AOD/LAOD OK Raster.rds")
AOD.r <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD OK Raster.rds")

n <- nrow(sitedata)
aod.points <- c(rep(NA, n))

for (i in 1:192){  
  aod.month <- AOD.r[[i]]

  aod.points <- raster::extract(aod.month, sites)
  
  time <- rep(i, n.sites)
  aod.points <- cbind(sites, time, aod.points)
  
  sitedata <- natural_join(sitedata, aod.points, by = c("time", "Easting", "Northing"), jointype = "FULL")
}

names(sitedata)[5] <- "Inter AOD"

sitedata$adjusted.pm25 <- sitedata$PM25*(1- sitedata$humid/100)
sitedata$adjusted.intaod <- sitedata$`Inter AOD`/sitedata$blh
sitedata$adjusted.aod47 <- sitedata$AOD47/sitedata$blh
sitedata$adjusted.aod55 <- sitedata$AOD55/sitedata$blh

cor(sitedata[,c("adjusted.intaod", "adjusted.pm25")], use="pairwise.complete.obs")

sitedata$year <- floor(sitedata$time/12) + 2005
sitedata$month <- sitedata$time - (sitedata$year-2005)*12
saveRDS(sitedata, "Data/London Site Data.rds")

library(bmstdr)
sitedata <- readRDS("Data/London Site Data.rds")

sitedata$s.index <- as.integer(as.factor(sitedata$`Site Name`))
sitedate <- sitedata[order(sitedata$time),]
sitedate <- sitedata[order(sitedata$s.index),]

f <- adjusted.pm25 ~ `Altitude (m)` + background + humid + industrial + pcm + population + precip + temp + traffic + urban

M1 <- Bsptime(model = "lm", formula = f, data = sitedata, scale.transform = "SQRT", N = 5000)
M2 <- Bsptime(model = "separable", formula = f, data = sitedata ,scale.transform = "SQRT", coordtype="utm",coords=4:5, N=5000)

