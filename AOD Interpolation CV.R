library(gstat)
library(sp)
library(raster)
library(rgdal)
library(sf)
library(automap)

AOD.df <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD.rds")
IDW.df <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD Spatial IDW DF.rds")
OK.df <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD OK DF.rds")
UK.df <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD UK DF.rds")

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
proj4string(London) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
gridL <- raster(extent(London), resolution = c(1000,1000), crs = proj4string(London))
gridL <- as(gridL, 'SpatialGrid')
proj4string(gridL) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
fullgrid(gridL) <- F
ind <- over(gridL, as(London,"SpatialPolygons"))
gridL <- gridL[!is.na(ind)]
grid <- st_as_stars(gridL, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

LAOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD.rds")
LAOD <- st_as_sf(LAOD, coords = c("x","y"), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
LAOD <- na.omit(LAOD)

cv1 <- list()
cv2 <- list()
for (i in 1:1) {
  AODmonth <- subset(LAOD, (year-2005)*12+month == i)
  g1 <- gstat(formula = AOD ~ 1, data = AODmonth)
  #z1 <- predict(g1, gridL)
  #cv1[[i]] <- gstat.cv(g1)
  
  v_emp_ok <- variogram(AOD ~ 1, AODmonth)
  v_mod_ok <- autofitVariogram(AOD ~ 1, as(AODmonth, "Spatial"))
  g2 <- gstat(formula = AOD ~ 1, model = v_mod_ok$var_model, data = AODmonth)
  #z2 <- predict(g2, gridL)
  cv2 <- krige.cv(AOD ~ 1, AODmonth, v_mod_ok$var_model)
}

saveRDS(cv2, "Data/AOD OK CV.rds")

cv2 <- readRDS("Data/AOD OK CV.rds")
sqrt(sum((cv1[[1]]$var1.pred - cv1[[1]]$observed)^2) / nrow(cv1[[1]])) #RMSE
sqrt(sum((cv2$var1.pred - cv2$observed)^2) / nrow(cv2)) #RMSE

plot(z1)
plot(z2)