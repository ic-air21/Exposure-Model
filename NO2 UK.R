library(spacetime)
library(sp)
library(gstat)
library(rgdal)
library(units)
library(spatstat)
library(dplyr)
library(xts)
library(sp)

LNO2 <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2stdays.rds")
proj4string(LNO2) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
hist(LNO2$`Trop NO2`)
LNO2$NO2 <- LNO2$`Trop NO2`/100000000000000
hist(LNO2$NO2)

empVgm <- variogramST(NO2 ~ x + y, LNO2, tlags=0:6, tunit = "day")
plot(empVgm, wireframe=T, scales=list(arrows=F))
plot(empVgm)

saveRDS(empVgm, "Data/Air Pollution/MAIAC AOD/LNO2 UK empVgm.rds")
empVgm <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 UK empVgm.rds")


#stkrige document
# fit of theoretical purely spatial models #
############################################
spEmpVgm <- empVgm[empVgm$timelag == 0,]
class(spEmpVgm) <- c("gstatVariogram", "data.frame")
spEmpVgm <- spEmpVgm[-1,1:3]
spEmpVgm$dir.hor <- 0
spEmpVgm$dir.ver <- 0
spVgmMod <- fit.variogram(spEmpVgm, vgm(100,"Mat",300000,20))
plot(spEmpVgm, spVgmMod)

# fit of theoretical spatio-temporal models #
#############################################

linStAni <- estiStAni(empVgm, c(20000,25000)) #A spatio-temporal anisotropy; the number of space unitsequivalent to one time unit.

plot(gamma ~ dist, empVgm[empVgm$timelag == 0,], ylim=c(0,300), xlim=c(0,500000))
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
plot(empVgm, fitSumMetricModel, wireframe=T, all=T, scales=list(arrows=F), zlim=c(0,180))

saveRDS(fitSumMetricModel, "Data/Air Pollution/MAIAC AOD/LAODSumMetricModelUK.rds")
fitSumMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LAODSumMetricModelUK.rds")


#https://github.com/r-spatial/gstat/blob/main/demo/stkrige-prediction.R
#https://github.com/r-spatial/gstat/blob/main/demo/stkrige-crossvalidation.R

gridL <- SpatialGrid(GridTopology(LNO2@sp@bbox[,1]%/%10000*1000, c(1000,1000),
                                  cells.dim=ceiling(apply(LNO2@sp@bbox,1,diff)/1000)))

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
proj4string(London) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

library(raster)
gridL <- raster(extent(London), resolution = c(1000,1000), crs = proj4string(London))
gridL <- as(gridL, 'SpatialGrid')

proj4string(gridL) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
fullgrid(gridL) <- F

#ind <- over(gridL, as(London,"SpatialPolygons"))
#gridL <- gridL[!is.na(ind)]

plot(gridL)

LNO2@time
# back scale vgms:
fitSumMetricModel$space$range <- fitSumMetricModel$space$range*1000
fitSumMetricModel$joint$range <- fitSumMetricModel$joint$range*1000
fitSumMetricModel$stAni <- fitSumMetricModel$stAni*1000
LNO2@time

#n = 2610
# m = 26
length(gridL)
192 - 26*7
L_xy <- rbind(gridL@coords[,1], gridL@coords[,2])
L_xy <- t(cbind(L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy, L_xy, L_xy,L_xy, L_xy,L_xy, L_xy))
L_xy <- as.data.frame(L_xy, row.names = c("x","y"))
names(L_xy) <- c("x", "y")
L_pred <- STFDF(as(gridL, "SpatialPoints"), LNO2@time, L_xy)

nrow(L_xy)
length(as(gridL, "SpatialPoints"))
nrow(LNO2@time)
26*2610*2

data <- na.omit(LNO2@data)
omit <- attr(data, "na.action")
omit.v <- c(omit)
LNO2 <- LNO2[-omit.v]


LNO2 <- subset(LNO2, !is.na(LNO2@data$NO2))


predL <- krigeST(NO2 ~ x + y, data = LNO2, modelList = fitSumMetricModel, newdata=L_pred)

saveRDS(predL, "Data/Air Pollution/MAIAC AOD/LNO2 UK Days.rds")
#i dont get why its different here!
predL <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 UK Days.rds")


subset.predL <- predL[,index(predL@time[c(100:105)])]

stplot(subset.predL)

predL.df <- as.data.frame(predL)
predL.df <- predL.df[,c(1,2,6,7)]
names(predL.df) <- c("x", "y", "time", "NO2")

saveRDS(predL.df, "Data/Air Pollution/MAIAC AOD/LNO2 UK DF.rds")

predL.df <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 UK DF.rds")
predL.df2 <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 OK Days.rds")


library(ggplot2)
library(gganimate)

Pollution <- readRDS("Data/All.rds")
dates <- unique(Pollution$dates)

times <- as.numeric(c(seq(1,192,1)))
dates <- as.data.frame(cbind(times, dates))
dates$times <- as.integer(dates$times)
predL.df <- merge(predL.df, dates, x.by = time, y.by = V1)

predL.80 <- subset(predL.df, time == 80)
predL.802 <- subset(predL.df2, time == 80)

library(ggplot2)
LOK.80 <- ggplot() + 
  geom_raster(data = predL.80, aes(x, y, fill = NO2)) +
  scale_fill_viridis_c(option = "inferno") +
  coord_equal() +
  theme_bw() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("OK NO2") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
LUK.80
anim_save("Ouputs Plots/OK GIF.gif") 

library(ggpubr)
ggarrange(LNO2.plot, LIDW.plot, LOK.80, LUK.80, 
          ncol = 4, nrow = 1)


###all variograms kriging

# separable
fitSepModel <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2SepModel.rds")
fitProdSumModel <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2ProdSumModel.rds")
fitMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2MetricModel.rds")
fitSimpleSumMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2SimpleSumMetricModel.rds")
fitSumMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2SumMetricModel.rds")

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

sepPredL <- krigeST(NO2 ~ 1, data = LNO2, modelList = fitSepModel, newdata=L_pred) 
prodPredL <- krigeST(NO2 ~ 1, data = LNO2, modelList = fitProdSumModel, newdata=L_pred) 
metPredL <- krigeST(NO2 ~ 1, data = LNO2, modelList = fitMetricModel, newdata=L_pred) 
simpSumPredL <- krigeST(NO2 ~ 1, data = LNO2, modelList = fitSimpleSumMetricModel, newdata=L_pred) 
sumPredL <- krigeST(NO2 ~ 1, data = LNO2, modelList = fitSumMetricModel, newdata=L_pred) 

saveRDS(sepPredL, "Data/Air Pollution/MAIAC AOD/LNO2 OK Sep Days.rds")
saveRDS(prodPredL, "Data/Air Pollution/MAIAC AOD/LNO2 OK Prod Days.rds")
saveRDS(metPredL, "Data/Air Pollution/MAIAC AOD/LNO2 OK Met Days.rds")
saveRDS(simpSumPredL, "Data/Air Pollution/MAIAC AOD/LNO2 OK SimpSum Days.rds")
saveRDS(sumPredL, "Data/Air Pollution/MAIAC AOD/LNO2 OK Sum Days.rds")

## cross-validation
crossStat <- function(var1, var2="NO2", STxDF=LNO2, digits=NA) {
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

target <- as(LNO2[,,"NO2"],"STFDF")

res <- matrix(NA, length(LNO2@sp), 192)



res[loc,!is.na(target[loc,])[,"NO2"]] <- sepPredL$var1.pred



LNO2@data$sepPredL <- res[!is.na(res)]


crossStat(sepPredL)

LNO2.df <- as.data.frame(LNO2)
LNO2.df$NO2 <- LNO2.df$NO2*100000000000000

# Universal kriging: x + y, x + y + blh + humidity + temp???
g_NO2 <- gstat(id = "NO2", formula = NO2 ~ x + y, locations = ~ x + y, data = LNO2.df)
vcloud <- variogram(object = g_NO2, cutoff = Inf, cloud = TRUE)
vemp <- variogram(object = g_NO2, cutoff = max(vcloud$dist)/2, width = 10)
vfit <- fit.variogram(object = vemp, model = vgm("Mat"), fit.sills = TRUE, fit.ranges = TRUE, fit.method = 6)
g_NO2_up <- gstat(g_NO2, model = vfit, id = "NO2", formula = NO2 ~ x + y, locations = ~ x + y, data = LNO2.df)
p_NO2 <- krigeST(NO2 ~ x + y, data = LNO2, modelList = g_NO2_up, newdata = L_pred)
sumPredL <- krigeST(NO2 ~ 1, data = LNO2, modelList = fitSumMetricModel, newdata=L_pred) 