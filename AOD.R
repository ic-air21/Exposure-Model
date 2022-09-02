library(raster)
library(dplyr)
library(ncdf4)
library(maptools)
library(fields)
library(gdalUtils)
library(rgdal)

DaysInMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
                 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
                 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

mean17 = list()
mean18 = list()

satellitedata17 <- lapply(Sys.glob("Z:/home/Satellite AOD/MCD19A2.A2018*h17*"), get_subdatasets)


satellitedata17 = list()
satellitedata18 = list()
for (j in 1:31) {
  satellitedata17[[j]] <- lapply(Sys.glob(paste("Z:/home/Satellite AOD/MCD19A2.A*",j,"h17*.hdf")), get_subdatasets)
  satellitedata18[[j]] <- lapply(Sys.glob(paste("Z:/home/Satellite AOD/MCD19A2.A*",j,"h18*.hdf")), get_subdatasets)
}

for (i in 1:2) {
  n <- DaysInMonth[i]
  satellitedata17 = list()
  satellitedata18 = list()
  for (j in 1:n) {
    satellitedata17[[j]] <- lapply(Sys.glob(paste("Z:/home/Satellite AOD/MCD19A2.A*",j,"h17*.hdf")), get_subdatasets)
    satellitedata18[[j]] <- lapply(Sys.glob(paste("Z:/home/Satellite AOD/MCD19A2.A*",j,"h18*.hdf")), get_subdatasets)
  }
  satelliterasters17 = list()
  satelliterasters18 = list()
  for (k in 1:n) {
    dir.create("Z:/home/Satellite AOD/Tile17")
    gdal_translate(satellitedata17[[k]][1], dst_dataset = paste("Z:/home/Satellite AOD/Tile17/satellitetif17", k, ".tif"))
    satelliterasters17[[k]] <- raster(paste("Z:/home/Satellite AOD/Tile17/satellitetif17", k, ".tif"))
    dir.create("Z:/home/Satellite AOD/Tile18")
    gdal_translate(satellitedata18[[k]][1], dst_dataset = paste("Z:/home/Satellite AOD/Tile18/satellitetif18", k, ".tif"))
    satelliterasters18[[k]] <- raster(paste("Z:/home/Satellite AOD/Tile18/satellitetif18", k, ".tif"))
  }
  rasters17 <- stack(satelliterasters17)
  rasters17 <- flip(rasters17, direction = 'y')
  mean17[i] <- calc(rasters17, fun = mean, na.rm = T)
  rasters18 <- stack(satelliterasters18)
  rasters18 <- flip(rasters18, direction = 'y')
  mean18[i] <- calc(rasters18, fun = mean, na.rm = T)
                    
  unlink("Z:/home/Satellite AOD/Tile17", recursive = TRUE)
  unlink("Z:/home/Satellite AOD/Tile18", recursive = TRUE)
  
  paste("AOD", i) <- mosaic(mean17, mean18, fun = mean)
  
  #rm("satellitedata17", "satellitedata18", "satelliterasters17", "satelliterasters18", "rasters17", "mean17", "rasters18", "mean18")
}






mydates <- c(seq(as.Date("2005-01-01"), as.Date("2020-12-31"), by = 1))
mydates[1:200]
list.files(pattern="home/Satellite AOD/MCD19A2.A*.h17v03.006.*.hdf")

file.rename(list.files(pattern="Z:/home/Satellite AOD/MCD19A2.A*.h17v03.006.*.hdf"), paste0("Z:/home/Satellite AOD/AOD17", mydates))

#by year
DaysInYear <- c(365, 365, 365, 366, 365, 365, 365, 366, 365, 365, 365, 366, 365, 365, 365, 366, 365, 365, 365, 366)




for (i in 2005:2005) {
  satellitedata17 <- lapply(Sys.glob(paste("Z:/home/Satellite AOD/MCD19A2.A",i,"*h17*.hdf")), get_subdatasets)
  satellitedata18 <- lapply(Sys.glob(paste("Z:/home/Satellite AOD/MCD19A2.A",i,"*h18*.hdf")), get_subdatasets)
}

#years
for (i in 2005:2005) {
  satellitedata17 <- lapply(Sys.glob(paste("Z:/home/Satellite AOD/MCD19A2.A",i,"*h17*.hdf")), get_subdatasets)
  satellitedata18 <- lapply(Sys.glob(paste("Z:/home/Satellite AOD/MCD19A2.A",i,"*h18*.hdf")), get_subdatasets)
  satelliterasters17 = list()
  satelliterasters18 = list()
  #months
  for (j in 1:12) {
    #days
    n <- DaysInMonth[(i - 2005)*12 + j]
    n
    for (k in 1:n) {
      dir.create("Z:/home/Satellite AOD/Tile17")
      gdal_translate(satellitedata17[[k]][1], dst_dataset = paste("Z:/home/Satellite AOD/Tile17/satellitetif17", k, ".tif"))
      satelliterasters17[[k]] <- raster(paste("Z:/home/Satellite AOD/Tile17/satellitetif17", k, ".tif"))
      dir.create("Z:/home/Satellite AOD/Tile18")
      gdal_translate(satellitedata18[[k]][1], dst_dataset = paste("Z:/home/Satellite AOD/Tile18/satellitetif18", k, ".tif"))
      satelliterasters18[[k]] <- raster(paste("Z:/home/Satellite AOD/Tile18/satellitetif18", k, ".tif"))
    }
    rasters17 <- stack(satelliterasters17)
    rasters17 <- flip(rasters17, direction = 'y')
    mean17[j] <- calc(rasters17, fun = mean, na.rm = T)
    rasters18 <- stack(satelliterasters18)
    rasters18 <- flip(rasters18, direction = 'y')
    mean18[j] <- calc(rasters18, fun = mean, na.rm = T)
    
    unlink("Z:/home/Satellite AOD/Tile17", recursive = TRUE)
    unlink("Z:/home/Satellite AOD/Tile18", recursive = TRUE)
    
    paste("AOD", i, j) <- mosaic(mean17, mean18, fun = mean)
  }
}  
  
  #rm("satellitedata17", "satellitedata18", "satelliterasters17", "satelliterasters18", "rasters17", "mean17", "rasters18", "mean18")




DaysinMonth <- c(#1,31,59,90,120,151,181,212,243,273,303,333,364)# day 276 missing
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, #
                 # 1,31,59,89,119,150,180,211,242,272,303,333,364)
                  #1,31,59,90,120,151,181,212,243,273,304,334,365) #2007
                #1, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
                 # 1, 31,60,91,121,152,182,213,244,274,305,335,366)
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                #1,31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31#
                #1,31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                 # 1, 31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,  
                #1, 31,60,91,121,152,182,213,244,274,305,335,366)
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,#
                #1, 31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, #
                 #1, 31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, #
                 # 1,31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, #
               # 1,31,60,91,121,152,182,213,244,274,305,335,366) #2016
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
                #1,31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
               # 1, 31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,#
                1,31,59,90,120,151,181,212,243,273,304,334,365)
                 #31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) #31,60,91,121,152,182,213,244,274,305,335,366

DaysInMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 30, 30, 31) #day 276 missing in October
DaysIn1 <-  c(31, 28, 30, 30, 31, 30, 31, 31, 30, 31, 30, 31) #day 88 missing, in March
DaysIn2 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
DaysIn3 <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
DaysIn4 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
DaysIn5 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
DaysIn6 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
DaysIn7 <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
DaysIn8 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
DaysIn9 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
DaysIn10 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
DaysIn11 <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
DaysIn12 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
DaysIn13 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
DaysIn14 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
DaysIn15 <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 30) #day 366 missing, december


satellitedata17 <- lapply(Sys.glob("Z:/home/Satellite AOD/MCD19A2.A2020*h17*.hdf"), get_subdatasets)
satellitedata18 <- lapply(Sys.glob("Z:/home/Satellite AOD/MCD19A2.A2020*h18*.hdf"), get_subdatasets)

#start
mean17.047 <- list()
mean17.055 <- list()
mean17.Uncertainity <- list()
mean18.047 <- list()
mean18.055 <- list()
mean18.Uncertainity <- list()
  
satelliterasters17.047 = list()
satelliterasters17.055 = list()
satelliterasters17.Uncertainity = list()

satelliterasters18.047 = list()
satelliterasters18.055 = list()
satelliterasters18.Uncertainity = list()
  
dir.create("Z:/home/Satellite AOD/Tile17")
dir.create("Z:/home/Satellite AOD/Tile18")
  
for (i in 1:length(satellitedata17)) {
  gdal_translate(satellitedata17[[i]][1], dst_dataset = paste("Z:/home/Satellite AOD/Tile17/satellitetif17.047", i, ".tif"))
  satelliterasters17.047[[i]] <- raster(paste("Z:/home/Satellite AOD/Tile17/satellitetif17.047", i, ".tif"))
    
  gdal_translate(satellitedata17[[i]][2], dst_dataset = paste("Z:/home/Satellite AOD/Tile17/satellitetif17.055", i, ".tif"))
  satelliterasters17.055[[i]] <- raster(paste("Z:/home/Satellite AOD/Tile17/satellitetif17.055", i, ".tif"))
    
  gdal_translate(satellitedata17[[i]][3], dst_dataset = paste("Z:/home/Satellite AOD/Tile17/satellitetif17.Uncertainity", i, ".tif"))
  satelliterasters17.Uncertainity[[i]] <- raster(paste("Z:/home/Satellite AOD/Tile17/satellitetif17.Uncertainity", i, ".tif"))
  
  gdal_translate(satellitedata18[[i]][1], dst_dataset = paste("Z:/home/Satellite AOD/Tile18/satellitetif18.047", i, ".tif"))
  satelliterasters18.047[[i]] <- raster(paste("Z:/home/Satellite AOD/Tile18/satellitetif18.047", i, ".tif"))

  gdal_translate(satellitedata18[[i]][2], dst_dataset = paste("Z:/home/Satellite AOD/Tile18/satellitetif18.055", i, ".tif"))
  satelliterasters18.055[[i]] <- raster(paste("Z:/home/Satellite AOD/Tile18/satellitetif18.055", i, ".tif"))
  
  gdal_translate(satellitedata18[[i]][3], dst_dataset = paste("Z:/home/Satellite AOD/Tile18/satellitetif18.Uncertainity", i, ".tif"))
  satelliterasters18.Uncertainity[[i]] <- raster(paste("Z:/home/Satellite AOD/Tile18/satellitetif18.Uncertainity", i, ".tif"))
}
  
rasters17.047 <- stack(satelliterasters17.047)

rasters17.055 <- stack(satelliterasters17.055)

rasters17.Uncertainity <- stack(satelliterasters17.Uncertainity)

rasters18.047 <- stack(satelliterasters18.047)

rasters18.055 <- stack(satelliterasters18.055)
  
rasters18.Uncertainity <- stack(satelliterasters18.Uncertainity)
  
for (j in 1:12){
  n <- DaysinMonth[j]
  m <- DaysinMonth[j+1]
  
  subset17.047 <- rasters17.047[[n:m]]
  subset17.055 <- rasters17.055[[n:m]]
  subset17.Uncertainity <- rasters17.Uncertainity[[n:m]]
  
  mean17.047 <- calc(subset17.047, fun = mean, na.rm = T)
  mean17.055 <- calc(subset17.055, fun = mean, na.rm = T)
  mean17.Uncertainity <- calc(subset17.Uncertainity, fun = mean, na.rm = T)
  
  subset18.047 <- rasters18.047[[n:m]]
  subset18.055 <- rasters18.055[[n:m]]
  subset18.Uncertainity <- rasters18.Uncertainity[[n:m]]
  
  mean18.047 <- calc(subset18.047, fun = mean, na.rm = T)   
  mean18.055 <- calc(subset18.055, fun = mean, na.rm = T)
  mean18.Uncertainity <- calc(subset18.Uncertainity, fun = mean, na.rm = T)
  
  AOD.047 <- mosaic(mean17.047, mean18.047, fun = mean)
  AOD.055 <- mosaic(mean17.055, mean18.055, fun = mean)
  AOD.Uncertainity <- mosaic(mean17.Uncertainity, mean18.Uncertainity, fun = mean)
  
  AOD <- stack(AOD.047, AOD.055, AOD.Uncertainity)
  writeRaster(AOD, filename=paste("Z:/home/Satellite AOD/AOD", j, "20.tif"), options="INTERLEAVE=BAND", overwrite=TRUE)
}
  
unlink("Z:/home/Satellite AOD/Tile17", recursive = TRUE)
unlink("Z:/home/Satellite AOD/Tile18", recursive = TRUE)
  
  



