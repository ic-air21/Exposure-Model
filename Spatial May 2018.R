#cropping to London May 2018

# need temp + precip + humid + PM25PCM + NO2PCM + AOD47 + AOD55 + SNO2 + population

library(ncdf4)
tempnc <- nc_open("Data/Climate/Temperature/tas_hadukgrid_uk_1km_mon_201801-201812.nc")
precipnc <- nc_open("Data/Climate/Rainfall/rainfall_hadukgrid_uk_1km_mon_201801-201812.nc")
humidnc <- nc_open("Data/Climate/Humidity/hurs_hadukgrid_uk_1km_mon_201801-201812.nc")

temp <- ncvar_get(tempnc, "tas")
precip <- ncvar_get(precipnc, "rainfall")
humid <- ncvar_get(humidnc, "hurs")

lat <- append(tempnc[["var"]][["latitude"]][["dim"]][[1]][["vals"]],tempnc[["var"]][["latitude"]][["dim"]][[2]][["vals"]])
long <- append(tempnc[["var"]][["longitude"]][["dim"]][[1]][["vals"]],tempnc[["var"]][["longitude"]][["dim"]][[2]][["vals"]])

temp.month <- temp[, , 5] 
temp.r <- raster(t(temp.month), xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
temp.r <- raster::flip(temp.r, direction='y')
temp.r <- projectRaster(temp.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

precip.month <- precip[, , 5] 
precip.r <- raster(t(precip.month), xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
precip.r <- raster::flip(precip.r, direction='y')
precip.r <- projectRaster(precip.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

humid.month <- humid[, , 5] 
humid.r <- raster(t(humid.month), xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
humid.r <- raster::flip(humid.r, direction='y')
humid.r <- projectRaster(humid.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

PM25PCM <- read.csv("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1/Data/Air Pollution/PM25 PCM 2005-2020/PCM 2018.csv")[,-1]
NO2PCM <- read.csv("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1/Data/Air Pollution/NO2 PCM 2005-2020/PCM 2018.csv")[-c(1:5),-1]
colnames(PM25PCM) <- c("x", "y", "PM25PCM")
colnames(NO2PCM) <- c("x", "y", "NO2PCM")
NO2PCM$x <- as.integer(NO2PCM$x)
NO2PCM$y <- as.integer(NO2PCM$y)
NO2PCM$NO2PCM <- as.integer(NO2PCM$NO2PCM)
PM25PCM$PM25PCM <- as.integer(PM25PCM$PM25PCM)

PCM <- merge(PM25PCM, NO2PCM, by = c("x", "y"))

PCM.r <- rasterFromXYZ(PCM[,c("x", "y", "PM25PCM", "NO2PCM")], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")
PCM.df <- as.data.frame(PCM.r, xy = TRUE)

"#probs need to resample SNO2"

AOD.r <- readRDS("Data/Air Pollution/MAIAC AOD/AOD0518.rds")
AOD.r <- projectRaster(AOD.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

SNO2.r <- raster("Data/OMI_trno2_0.10x0.10_201805_Col3_V4.tif")
SNO2.r <- projectRaster(SNO2.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

population.r <- raster("Data/Population/gbr_ppp_2018_1km_Aggregated.tif")
population.r <- projectRaster(population.r, crs = crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))


#temp.r, precip.r, humid.r, PCM.r, AOD.r, SNO2.r, population.r
plot(temp.r)
plot(precip.r)
plot(humid.r)
plot(PCM.r)
plot(AOD.r)
plot(population.r)

temp.r <- resample(temp.r, PCM.r, "bilinear")
precip.r <- resample(precip.r, PCM.r, "bilinear")
humid.r <- resample(humid.r, PCM.r, "bilinear")
AOD.r <- resample(AOD.r, PCM.r, "bilinear")
SNO2.r <- resample(SNO2.r, PCM.r, "bilinear")
population.r <- resample(population.r, PCM.r, "bilinear")

temp_df <- as.data.frame(temp.r, xy = TRUE)
precip_df <- as.data.frame(precip.r, xy = TRUE)
humid_df <- as.data.frame(humid.r, xy = TRUE)
pcm_df <- as.data.frame(PCM.r, xy = TRUE)
aod_df <- as.data.frame(AOD.r, xy = TRUE)
sno2_df <- as.data.frame(SNO2.r, xy = TRUE)
pop_df <- as.data.frame(population.r, xy = TRUE)

met <- merge(temp_df, precip_df, by = c("x","y"))
met <- merge(met, humid_df, by = c("x","y"))
colnames(met) <- c("x", "y", "temp", "precip", "humid")

sat <- merge(aod_df, sno2_df, by = c("x", "y"))
colnames(sat) <- c("x", "y", "AOD", "SNO2")

other <- merge(pcm_df, pop_df, by = c("x", "y"))
colnames(other)[5] <- "population"

met.sat <- merge(met, sat, by = c("x", "y"), all.x = TRUE, all.y = TRUE)

all.df <- merge(met.sat, other, by = c("x", "y"), all.x = TRUE, all.y = TRUE)
saveRDS(all.df, "Data/May 2018 Large Raster.rds")

all.r <- rasterFromXYZ(all, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
proj4string(London) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

london.r <- crop(all.r, London)
hist(london.r)

london.df <- as.data.frame(london.r, xy = TRUE)
saveRDS(london.df, "Data/All London May 2018.rds")
