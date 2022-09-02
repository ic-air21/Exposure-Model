library(ncdf4)
library(raster)
library(terra)

library(readxl)
library(ggplot2)
library(reshape2)
library(scico)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)

tempnc <- lapply(Sys.glob("Data/Climate/Temperature/tas_hadukgrid_uk_1km_mon_*.nc"), nc_open)
precipnc <- lapply(Sys.glob("Data/Climate/Rainfall/rainfall_hadukgrid_uk_1km_mon_*.nc"), nc_open)
humidnc <- lapply(Sys.glob("Data/Climate/Humidity/hurs_hadukgrid_uk_1km_mon_*.nc"), nc_open)

temp <- list()
precip <- list()
humid <- list()
lat <- list()
long <- list()
temp.r <- list()
precip.r <- list()
humid.r <- list()
met.r <- list()


temp[[1]] <- ncvar_get(tempnc[[1]], "tas")
lat <- append(tempnc[[1]][["var"]][["latitude"]][["dim"]][[1]][["vals"]],tempnc[[1]][["var"]][["latitude"]][["dim"]][[2]][["vals"]])
long <- append(tempnc[[1]][["var"]][["longitude"]][["dim"]][[1]][["vals"]],tempnc[[1]][["var"]][["longitude"]][["dim"]][[2]][["vals"]])
temp.month <- temp[[1]][, , 1] 

temp.r[[1]] <- raster(t(temp.month), xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
temp.r[[1]] <- raster::flip(temp.r[[1]], direction='y')
#temp.r[[1]] <- projectRaster(temp.r[[1]], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")
temp_df <- as.data.frame(temp.r[[1]], xy = TRUE)

plot(UK)
UK <- readOGR("Data/UK Shapefile/HAD UK Grid SF/ukcp18-uk-land-1km.shp")
proj4string(UK) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"
plot(UK)


ggplot() + 
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = temp_df, aes(x = x, y = y, fill = layer, alpha = 0.9)) + 
  #geom_point(data = climate, aes(x = Easting, y = Northing, color = NO2)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 5.3) +
  coord_equal()


for (i in 1:16) {
  temp[[i]] <- ncvar_get(tempnc[[i]], "tas")
  precip[[i]] <- ncvar_get(precipnc[[i]], "rainfall")
  humid[[i]] <- ncvar_get(humidnc[[i]], "hurs")
  
  lat <- append(tempnc[[i]][["var"]][["latitude"]][["dim"]][[1]][["vals"]],tempnc[[1]][["var"]][["latitude"]][["dim"]][[2]][["vals"]])
  long <- append(tempnc[[i]][["var"]][["longitude"]][["dim"]][[1]][["vals"]],tempnc[[1]][["var"]][["longitude"]][["dim"]][[2]][["vals"]])
  
  #lat[[i]] <- ncvar_get(tempnc[[i]], "latitude")
  #long[[i]] <- ncvar_get(tempnc[[i]], "longitude")
  
  for (j in 1:12) {
    temp.month <- temp[[i]][, , j] 
    temp.r[[j]] <- raster(t(temp.month), xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
    temp.r[[j]] <- raster::flip(temp.r[[j]], direction='y')
    temp.r[[j]] <- projectRaster(temp.r[[j]], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
    
    precip.month <- precip[[i]][, , j] 
    precip.r[[j]] <- raster(t(precip.month), xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
    precip.r[[j]] <- raster::flip(precip.r[[j]], direction='y')
    precip.r[[j]] <- projectRaster(precip.r[[j]], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
    
    humid.month <- humid[[i]][, , j] 
    humid.r[[j]] <- raster(t(humid.month), xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
    humid.r[[j]] <- raster::flip(humid.r[[j]], direction='y')
    humid.r[[j]] <- projectRaster(humid.r[[j]], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
    
  }
  year <- i + 2004
  temp.stack <- stack(temp.r)
  humid.stack <- stack(humid.r)
  precip.stack <- stack(precip.r)
  
  raster::writeRaster(temp.stack, filename=paste("Data/Climate/temp", year, ".tif"), overwrite = TRUE)
  raster::writeRaster(humid.stack, filename=paste("Data/Climate/humid", year, ".tif"), overwrite = TRUE)
  raster::writeRaster(precip.stack, filename=paste("Data/Climate/precip", year, ".tif"), overwrite = TRUE)
}




for (i in 9:16) {
  for (j in 1:12) {
    temp.month <- ncvar_get(tempnc[[i]], "tas")[,,j]
    precip.month <- ncvar_get(precipnc[[i]], "rainfall")[,,j]
    humid.month <- ncvar_get(humidnc[[i]], "hurs")[,,j]

    
    lat <- append(tempnc[[i]][["var"]][["latitude"]][["dim"]][[1]][["vals"]],tempnc[[1]][["var"]][["latitude"]][["dim"]][[2]][["vals"]])
    long <- append(tempnc[[i]][["var"]][["longitude"]][["dim"]][[1]][["vals"]],tempnc[[1]][["var"]][["longitude"]][["dim"]][[2]][["vals"]])

    temp.r[[(i-9)*12+j]] <- raster(t(temp.month),  xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
    temp.r[[(i-9)*12+j]] <- raster::flip(temp.r[[(i-9)*12+j]], direction='y')
    temp.r[[(i-9)*12+j]] <- projectRaster(temp.r[[(i-9)*12+j]], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

    precip.r[[(i-9)*12+j]] <- raster(t(precip.month),  xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
    precip.r[[(i-9)*12+j]] <- raster::flip(precip.r[[(i-9)*12+j]], direction='y')
    precip.r[[(i-9)*12+j]] <- projectRaster(precip.r[[(i-9)*12+j]], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
    
    humid.r[[(i-9)*12+j]] <- raster(t(humid.month),  xmn=min(long), xmx=700000 , ymn=min(lat), ymx=max(lat), crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))
    humid.r[[(i-9)*12+j]] <- raster::flip(humid.r[[(i-9)*12+j]], direction='y')
    humid.r[[(i-9)*12+j]] <- projectRaster(humid.r[[(i-9)*12+j]], crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
    
  }
}

temp.stack <- stack(temp.r)
humid.stack <- stack(humid.r)
precip.stack <- stack(precip.r)

raster::writeRaster(temp.stack, filename="Data/Climate/temp2.tif", overwrite = TRUE)
#raster::writeRaster(temp.stack[[97:192]], filename="Data/Climate/temp2.tif", overwrite = TRUE)

raster::writeRaster(humid.stack, filename="Data/Climate/humid2.tif", overwrite = TRUE)
#raster::writeRaster(humid.stack[[97:192]], filename="Data/Climate/humid2.tif", overwrite = TRUE)

raster::writeRaster(precip.stack, filename="Data/Climate/precip2.tif", overwrite = TRUE)
#raster::writeRaster(precip.stack[[97:192]], filename="Data/Climate/precip2.tif", overwrite = TRUE)






