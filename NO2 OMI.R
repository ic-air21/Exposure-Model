library(ggplot2)
library(sf)
library(rgdal)
library(raster)

setwd("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1")
UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=longlat +datum=WGS84 +no_defs"))
UK.bbox <- extent(UK)

setwd("Z:/home")

TRNO2 <- stack(lapply(Sys.glob("Z:/home/Satellite NO2/OMI_trno2_0.10x0.10*_Col3_V4.tif"), raster))
proj4string(TRNO2) <- crs("+proj=longlat +datum=WGS84 +no_defs")
bbox <- extent(503568.2,561957.5, 155850.8, 200933.9)

LTRNO2 <- list()
LTRNO2.df <- list()
LTRNO2.DF <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("x", "y", "TRNO2", "year", "month")
colnames(LTRNO2.DF) <- x

for (i in 1:192) {
  LTRNO2[[i]] <- raster::crop(TRNO2[[i]], UK.bbox)
  LTRNO2[[i]] <- projectRaster(LTRNO2[[i]], crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
  LTRNO2.df[[i]] <- as.data.frame(LTRNO2[[i]], xy = TRUE)
  LTRNO2.df[[i]]$time <- i
  names(LTRNO2.df[[i]])[3] <- "Trop NO2"
  LTRNO2.DF <- rbind(LTRNO2.DF, LTRNO2.df[[i]])
}

setwd("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1")
setwd("~/PhD/R/Objective 1")
LTRNO2 <- stack(LTRNO2)
writeRaster(LTRNO2, filename="LTRNO2.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
saveRDS(LTRNO2.DF, "Data/Air Pollution/MAIAC AOD/LTRNO2.rds")

