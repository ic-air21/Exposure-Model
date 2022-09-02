library(raster)
library(dplyr)
library(ncdf4)
library(maptools)
library(fields)
library(gdalUtils)
library(rgdal)

blh.nc <- nc_open("Data/PBLH/adaptor.mars.internal-1656085299.940306-7998-8-a3ebb1ad-1c63-4cb6-b166-8a88859af47e.nc")

blh <- ncvar_get(blh.nc, "blh")
lat <- ncvar_get(blh.nc, "latitude")
long <- ncvar_get(blh.nc, "longitude")

fillvalue <- ncatt_get(blh.nc, "blh", "_FillValue")
blh[blh == fillvalue$value] <- NA



AURNSites <- read_excel("Data/Air Pollution/Monthly Means/AURN Sites.xlsx")

AURNSites$industrial <- ifelse(grepl("Industrial", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$traffic <- ifelse(grepl("Traffic", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$background <- ifelse(grepl("Background", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$urban <- ifelse(grepl("Urban", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$suburban <- ifelse(grepl("Suburban", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$rural <- ifelse(grepl("Rural", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)

sites <- unique(AURNSites[c("Easting", "Northing", "Site Name")])
n.sites <- as.numeric(nrow(sites))




blh.r <- list()
blh.sites <- list()
blh.df <- list()
blh.all <- data.frame(matrix(ncol = 3, nrow = 0))

for (i in 1:192) {
  blh.r[[i]] <- raster(t(blh[, , i]), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  blh.r[[i]] <- projectRaster(blh.r[[i]], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

  blh.sites <- raster::extract(blh.r[[i]], sites[,c(1,2)])
  
  time <- rep(i, n.sites)
  blh.points <- cbind(blh.sites, sites, time)
  blh.df[[i]] <- blh.points[,c("blh.sites", "Site Name", "time")]  
  blh.all <- rbind(blh.all, blh.df[[i]])
}

blh.r <- stack(blh.r)
names(blh.all)[1] <- "blh"


Climate.Pollution.Population.PCM.BLH <-merge(Climate.Pollution.Population.PCM, blh.all, by = c("Site Name", "time"))

Climate.Pollution.Population.PCM.BLH <- merge(Climate.Pollution.Population.PCM.BLH, AURNSites, by = "Site Name")

saveRDS(Climate.Pollution.Population.PCM.BLH, "Data/All Site Data.rds")
