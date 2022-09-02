library(spacetime)
library(sp)
library(gstat)
library(rgdal)
library(units)
library(spatstat)

#swap to days instead of months

LAOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD.rds")
hist(LAOD$AOD)
LAOD <- LAOD[,c(3,1,2,4,5)]

library(dplyr)
library(xts)
library(sp)

LAOD$dates <- (LAOD$year-2005)*12 + LAOD$month 
LAOD$date <- LAOD$dates %>% as.Date()
time <- LAOD$date
time_part <- unique(time)
time_part <- sort(time_part)

locs <- unique(LAOD[c("x","y")]) 
spat_part <- SpatialPoints(coords = locs)


LAODst <- STFDF(sp=spat_part, time=time_part, data=LAOD)
saveRDS(LAODst, "Data/Air Pollution/MAIAC AOD/LAODstdays.rds")

setwd("~/PhD/R/Objective 1")
#https://cran.r-project.org/web/packages/spacetime/vignettes/jss816.pdf

LAOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAODstdays.rds")
proj4string(LAOD) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
LAOD$AOD100 <- LAOD$AOD*100

empVgm <- variogramST(AOD100 ~ 1 , LAOD, tlags=0:6, tunit = "day")
plot(empVgm, wireframe=T, scales=list(arrows=F))
plot(empVgm)

saveRDS(empVgm, "Data/Air Pollution/MAIAC AOD/LAODempVgmDays.rds")
empVgm <- readRDS("Data/Air Pollution/MAIAC AOD/LAODempVgmDays.rds")
#stkrige document
# fit of theoretical purely spatial models #
############################################
spEmpVgm <- empVgm[empVgm$timelag == 0,]
class(spEmpVgm) <- c("gstatVariogram", "data.frame")
spEmpVgm <- spEmpVgm[-1,1:3]
spEmpVgm$dir.hor <- 0
spEmpVgm$dir.ver <- 0
spVgmMod <- fit.variogram(spEmpVgm, vgm(100,"Exp",3000,20))
plot(spEmpVgm, spVgmMod)

# fit of theoretical spatio-temporal models #
#############################################

linStAni <- estiStAni(empVgm, c(18000,19000)) #A spatio-temporal anisotropy; the number of space unitsequivalent to one time unit.

plot(gamma ~ dist, empVgm[empVgm$timelag == 0,], ylim=c(0,100), xlim=c(0,50000))
points(empVgm[empVgm$spacelag == 0,]$timelag*linStAni, empVgm[empVgm$spacelag == 0,]$gamma, col="red")


# rescale empVgm and linStAni to km for estimation
empVgm$dist  <- empVgm$dist/1000
empVgm$avgDist  <- empVgm$avgDist/1000
empVgm$spacelag <- empVgm$spacelag/1000

linStAni <- linStAni/1000

# sum-metric
# sumMetricModel <- sumMetricFromsimpleSumMetric(fitSimpleSumMetricModel)

sumMetricModel <- vgmST("sumMetric",
                        space = vgm(20, "Exp", 150, 1),
                        time = vgm(10, "Exp", 2, 0.5),
                        joint = vgm(80, "Exp", 1500, 2.5),
                        stAni = 120)
fitSumMetricModel <- fit.StVariogram(empVgm, sumMetricModel, fit.method = 7, stAni=linStAni,
                                     method = "L-BFGS-B", 
                                     lower = c(sill.s = 0,  range.s = 10,  nugget.s = 0,
                                               sill.t = 0,  range.t = 0.1,   nugget.t = 0,
                                               sill.st= 0, range.st = 10, nugget.st = 0, 
                                               anis = 40),
                                     upper = c(sill.s = 200,  range.s = 1E3,  nugget.s = 20,
                                               sill.t = 200,  range.t = 75,   nugget.t = 20,
                                               sill.st= 200, range.st = 5E3, nugget.st = 20,
                                               anis = 500),
                                     control = list(parscale = c(1,100,1,1,0.5,1,1,100,1,100),
                                                    maxit=1e4))
attr(fitSumMetricModel, "optim.output")$value
# Exp+Exp+Exp: 51635.59 Exp+Sph+Exp: 54144.7 Sph+Exp+Exp: 51295.34 Sph+Sph+Exp: 54066.9
# Exp+Exp+Sph: 364133.7 Exp+Sph+Sph: 258493 Sph+Exp+Sph: 364133.8 Sph+Sph+Sph: 355766.8
plot(empVgm, fitSumMetricModel, wireframe=T, all=T, scales=list(arrows=F), zlim=c(0,130))

saveRDS(fitSumMetricModel, "Data/Air Pollution/MAIAC AOD/LAODSumMetricModel.rds")
fitSumMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LAODSumMetricModel.rds")


#https://github.com/r-spatial/gstat/blob/main/demo/stkrige-prediction.R
#https://github.com/r-spatial/gstat/blob/main/demo/stkrige-crossvalidation.R

gridL <- SpatialGrid(GridTopology(LAOD@sp@bbox[,1]%/%10000*1000, c(1000,1000),
                                  cells.dim=ceiling(apply(LAOD@sp@bbox,1,diff)/1000)))

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
proj4string(London) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

library(raster)
gridL <- raster(extent(London), resolution = c(1000,1000), crs = proj4string(London))
gridL <- as(gridL, 'SpatialGrid')

proj4string(gridL) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
fullgrid(gridL) <- F

ind <- over(gridL, as(London,"SpatialPolygons"))
gridL <- gridL[!is.na(ind)]

plot(gridL)


# back scale vgms:
fitSumMetricModel$space$range <- fitSumMetricModel$space$range*1000
fitSumMetricModel$joint$range <- fitSumMetricModel$joint$range*1000
fitSumMetricModel$stAni <- fitSumMetricModel$stAni*1000

L_pred <- STF(as(gridL, "SpatialPoints"), LAOD@time)

LAOD <- na.omit(LAOD)
predL <- krigeST(AOD100 ~ 1, data = LAOD, modelList = fitSumMetricModel, newdata=L_pred) 
saveRDS(predL, "Data/Air Pollution/MAIAC AOD/LAOD OK Days.rds")

LAODOKDays <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD OK Days.rds")



#for full days?
pureSpPred <- matrix(NA, nrow = length(gridL), 192)

for (i in 1:26) {
  pureSpPred[,i] <- krige(AOD100 ~ 1, as(LAOD, "STSDF")[,i],
                            gridL, model = spVgmMod, nmax = 10)$var1.pred
}

#visualisation
setwd("~/PhD/R/Objective 1")

predL <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD OK Days.rds")

subset.predL <- predL[,index(predL@time[c(100:105)])]

stplot(subset.predL)

predL.df <- as.data.frame(predL)
predL.df <- predL.df[,c(1,2,6,7)]
names(predL.df) <- c("x", "y", "time", "AOD")
predL.df$AOD <- predL.df$AOD/100
  
saveRDS(predL.df, "Data/Air Pollution/MAIAC AOD/LAOD OK DF.rds")
predL.df <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD OK DF.rds")

library(ggplot2)
library(gganimate)

Pollution <- readRDS("Data/All.rds")
dates <- unique(Pollution$dates)

times <- as.numeric(c(seq(1,192,1)))
dates <- as.data.frame(cbind(times, dates))
dates$times <- as.integer(dates$times)
predL.df <- merge(predL.df, dates, x.by = time, y.by = V1)

predL.80 <- subset(predL.df, time == 80)
  
LOK.80 <- ggplot() + 
  geom_raster(data = predL.80, aes(x, y, fill = AOD)) +
  scale_fill_viridis_c(option = "inferno") +
  coord_equal() +
  theme_bw() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("OK AOD") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
LOK.80
anim_save("Ouputs Plots/OK GIF.gif") 

library(ggpubr)
ggarrange(LAOD.plot, LIDW.plot, LOK.80, 
          ncol = 3, nrow = 1)


###all variograms kriging

# separable
fitSepModel <- readRDS("Data/Air Pollution/MAIAC AOD/LAODSepModel.rds")
fitProdSumModel <- readRDS("Data/Air Pollution/MAIAC AOD/LAODProdSumModel.rds")
fitMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LAODMetricModel.rds")
fitSimpleSumMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LAODSimpleSumMetricModel.rds")
fitSumMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LAODSumMetricModel.rds")

fitSepModel$space$range <- fitSepModel$space$range*1000
fitProdSumModel$space$range <- fitProdSumModel$space$range*1000
fitMetricModel$joint$range <- fitMetricModel$joint$range*1000
fitMetricModel$stAni <- fitMetricModel$stAni*1000
fitSimpleSumMetricModel$space$range <- fitSimpleSumMetricModel$space$range*1000
fitSimpleSumMetricModel$joint$range <- fitSimpleSumMetricModel$joint$range*1000
fitSimpleSumMetricModel$stAni <- fitSimpleSumMetricModel$stAni*1000
fitSumMetricModel$space$range <- fitSumMetricModel$space$range*1000
fitSumMetricModel$joint$range <- fitSumMetricModel$joint$range*1000
fitSumMetricModel$stAni <- fitSumMetricModel$stAni*1000

sepPredL <- krigeST(AOD100 ~ 1, data = LAOD, modelList = fitSepModel, newdata=L_pred) 
prodPredL <- krigeST(AOD100 ~ 1, data = LAOD, modelList = fitProdSumModel, newdata=L_pred) 
metPredL <- krigeST(AOD100 ~ 1, data = LAOD, modelList = fitMetricModel, newdata=L_pred) 
simpSumPredL <- krigeST(AOD100 ~ 1, data = LAOD, modelList = fitSimpleSumMetricModel, newdata=L_pred) 
sumPredL <- krigeST(AOD100 ~ 1, data = LAOD, modelList = fitSumMetricModel, newdata=L_pred) 

saveRDS(sepPredL, "Data/Air Pollution/MAIAC AOD/LAOD OK Sep Days.rds")
saveRDS(prodPredL, "Data/Air Pollution/MAIAC AOD/LAOD OK Prod Days.rds")
saveRDS(metPredL, "Data/Air Pollution/MAIAC AOD/LAOD OK Met Days.rds")
saveRDS(simpSumPredL, "Data/Air Pollution/MAIAC AOD/LAOD OK SimpSum Days.rds")
saveRDS(sumPredL, "Data/Air Pollution/MAIAC AOD/LAOD OK Sum Days.rds")

## cross-validation
crossStat <- function(var1, var2="AOD100", STxDF=LAOD, digits=NA) {
  diff <- STxDF[,,var1,drop=F]@data[[1]] - STxDF[,,var2,drop=F]@data[[1]]
  RMSE <- sqrt(mean(diff^2))
  MAE <- mean(abs(diff))
  ME <- mean(diff)
  COR <- cor(STxDF[,,var1,drop=F]@data[[1]], STxDF[,,var2,drop=F]@data[[1]])
  res <- c(RMSE, MAE, ME, COR)
  names(res) <- c("RMSE", "MAE", "ME", "COR")
  if(is.na(digits))
    return(res)
  else
    return(round(res, digits))
}

target <- as(LAOD[,,"AOD100"],"STFDF")

res <- matrix(NA, length(LAOD@sp), 192)



res[loc,!is.na(target[loc,])[,"AOD100"]] <- sepPredL$var1.pred



LAOD@data$sepPredL <- res[!is.na(res)]


crossStat(sepPredL)


# Universal kriging: x + y, x + y + blh + humidity + temp???
