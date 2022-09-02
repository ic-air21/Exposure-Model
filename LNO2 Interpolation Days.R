library(spacetime)
library(gstat)
library(rgdal)
library(units)
library(spatstat)
library(dplyr)
library(xts)
library(sp)

setwd("~/PhD/R/Objective 1")
LNO2 <- readRDS("Data/Air Pollution/MAIAC AOD/LTRNO2.rds")
LNO2 <- LNO2[,c(3,1,2,4)]
LNO2 <- unique(LNO2)

LNO2$date <- LNO2$time %>% as.Date()
time <- LNO2$date
time_part <- unique(time)
time_part <- sort(time_part)

locs <- unique(LNO2[c("x","y")]) 
spat_part <- SpatialPoints(coords = locs)

LNO2st <- STFDF(sp=spat_part, time=time_part, data=LNO2)
saveRDS(LNO2st, "Data/Air Pollution/MAIAC AOD/LNO2stdays.rds")
LNO2st <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2stdays.rds")

setwd("~/PhD/R/Objective 1")
#https://cran.r-project.org/web/packages/spacetime/vignettes/jss816.pdf
LNO2 <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2stdays.rds")
proj4string(LNO2) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
hist(LNO2$`Trop NO2`)
LNO2$NO2 <- LNO2$`Trop NO2`/100000000000000
hist(LNO2$NO2)

empVgm <- variogramST(NO2 ~ 1, LNO2, tlags=0:6, tunit = "day")
plot(empVgm, wireframe=T, scales=list(arrows=F))
plot(empVgm)

saveRDS(empVgm, "Data/Air Pollution/MAIAC AOD/LNO2empVgmDays.rds")
empVgm <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2empVgmDays.rds")
#stkrige document
# fit of theoretical purely spatial models #
############################################
spEmpVgm <- empVgm[empVgm$timelag == 0,]
class(spEmpVgm) <- c("gstatVariogram", "data.frame")
spEmpVgm <- spEmpVgm[-1,1:3]
spEmpVgm$dir.hor <- 0
spEmpVgm$dir.ver <- 0
spVgmMod <- fit.variogram(spEmpVgm, vgm(100,"Exp",30000,20))
plot(spEmpVgm, spVgmMod)

# fit of theoretical spatio-temporal models #
#############################################

linStAni <- estiStAni(empVgm, c(100000,800000)) #A spatio-temporal anisotropy; the number of space unitsequivalent to one time unit.

plot(gamma ~ dist, empVgm[empVgm$timelag == 0,], ylim=c(0,400), xlim=c(0,800000))
points(empVgm[empVgm$spacelag == 0,]$timelag*linStAni, empVgm[empVgm$spacelag == 0,]$gamma, col="red")


# rescale empVgm and linStAni to km for estimation
empVgm$dist  <- empVgm$dist/1000
empVgm$avgDist  <- empVgm$avgDist/1000
empVgm$spacelag <- empVgm$spacelag/1000

linStAni <- linStAni/1000

# sum-metric
# sumMetricModel <- sumMetricFromsimpleSumMetric(fitSimpleSumMetricModel)

sumMetricModel <- vgmST("sumMetric",
                        space = vgm(20, "Sph", 150, 1),
                        time = vgm(10, "Sph", 2, 0.5),
                        joint = vgm(80, "Sph", 1500, 2.5),
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
# Exp+Exp+Exp: 910516.2 Exp+Sph+Exp: 910516.2 Sph+Exp+Exp: 659179.9 Sph+Sph+Exp: 659174.9
# Exp+Exp+Sph: 658496.1 Exp+Sph+Sph: 658496.1  Sph+Exp+Sph: 645632.1 Sph+Sph+Sph: 580390.9
plot(empVgm, fitSumMetricModel, wireframe=T, all=T, scales=list(arrows=F), zlim=c(0,300))

saveRDS(fitSumMetricModel, "Data/Air Pollution/MAIAC AOD/LNO2SumMetricModel.rds")
fitSumMetricModel <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2SumMetricModel.rds")

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

ind <- over(gridL, as(London,"SpatialPolygons"))
gridL <- gridL[!is.na(ind)]

plot(gridL)


# back scale vgms:
fitSumMetricModel$space$range <- fitSumMetricModel$space$range*1000
fitSumMetricModel$joint$range <- fitSumMetricModel$joint$range*1000
fitSumMetricModel$stAni <- fitSumMetricModel$stAni*1000

L_pred <- STF(as(gridL, "SpatialPoints"), LNO2@time)

LNO2 <- subset(LNO2, !is.na(LNO2@data$NO2))
LNO22 <- na.omit(LNO2)

predL <- krigeST(NO2 ~ 1, data = LNO2, modelList = fitSumMetricModel, newdata=L_pred) 
saveRDS(predL, "Data/Air Pollution/MAIAC AOD/LNO2 OK Days.rds")

LNO2OKDays <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 OK Days.rds")

#for full days?
pureSpPred <- matrix(NA, nrow = length(gridL), 192)

for (i in 1:26) {
  pureSpPred[,i] <- krige(NO2 ~ 1, as(LNO2, "STSDF")[,i],
                          gridL, model = spVgmMod, nmax = 10)$var1.pred
}

#visualisation
setwd("~/PhD/R/Objective 1")

predL <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 OK Days.rds")

subset.predL <- predL[,index(predL@time[c(100:105)])]

stplot(subset.predL)

predL.df <- as.data.frame(predL)
predL.df <- predL.df[,c(1,2,6,7)]
names(predL.df) <- c("x", "y", "time", "NO2")
predL.df$NO2 <- predL.df$NO2/100

saveRDS(predL.df, "Data/Air Pollution/MAIAC AOD/LNO2 OK DF.rds")
predL.df <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 OK DF.rds")

library(ggplot2)
library(gganimate)


ggplot() + 
  geom_raster(data = predL.df, aes(x, y, fill = NO2)) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  transition_time(time) +
  coord_equal() +
  theme_bw() +
  theme_void() +
  labs(title = 'Month: {frame_time}') +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

anim_save("Ouputs Plots/OK GIF.gif") 





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


# Universal kriging: x + y, x + y + blh + humidity + temp???


#idw
library(stars)
library(raster)
#IDW spatial interpolation
LNO2st <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2stdays.rds")
gridL <- SpatialGrid(GridTopology(LNO2st@sp@bbox[,1]%/%10000*1000, c(1000,1000),
                                  cells.dim=ceiling(apply(LNO2st@sp@bbox,1,diff)/1000)))
London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
proj4string(London) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
gridL <- raster(extent(London), resolution = c(1000,1000), crs = proj4string(London))

grid <- st_as_stars(gridL, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

LNO2 <- readRDS("Data/Air Pollution/MAIAC AOD/LTRNO2.rds")
LNO2 <- st_as_sf(LNO2, coords = c("x","y"), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
LNO2 <- na.omit(LNO2)

names(LNO2)[1] <- "NO2"

g <- gstat(formula = NO2 ~ 1, data = LNO2)

LIDW <- list()
for (i in 191:192) {
  LNO2month <- subset(LNO2, time == i)
  g <- gstat(formula = NO2 ~ 1, data = LNO2month)
  LIDW[[i]] <- predict(g, grid)
  LIDW[[i]] = LIDW[[i]]["var1.pred",,]
}
#whats wrong with 190???
r[[190]] <- r[[189]]
saveRDS(LIDW, "Data/Air Pollution/MAIAC AOD/LNO2 Spatial IDW.rds")
LIDW <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 Spatial IDW.rds")

b = seq(2000000000000000,15000000000000000, 1000000000000000)

LIDW.month <- LIDW[[80]]

plot(LIDW[[80]], breaks = b, col = hcl.colors(length(b)-1, "Spectral"), reset = FALSE)
plot(London, pch = 3, add = TRUE)
contour(LIDW[[24]], breaks = b, add = TRUE)

LIDW[[190]] <- LIDW[[189]]
r <- list()
for (i in 1:192){
  r[[i]] <-raster(LIDW[[i]][["var1.pred"]],
                  xmn=extent(gridL)[1], xmx=extent(gridL)[2],
                  ymn=extent(gridL)[3], ymx=extent(gridL)[4], 
                  crs=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
  )
} 
LIDW.stack <- stack(r)
library(dplyr)
LIDW.df <- as.data.frame(LIDW.stack, xy = TRUE) %>%
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'time', 
                      values_to = 'value')

saveRDS(LIDW.df, "Data/Air Pollution/MAIAC AOD/LNO2 Spatial IDW DF.rds" )
LIDW.df <- readRDS("Data/Air Pollution/MAIAC AOD/LNO2 Spatial IDW DF.rds")
library(ggplot2)
library(gganimate)

LIDW.df$times <- as.integer(as.factor(LIDW.df$time))
ggplot() + 
  geom_raster(data = LIDW.df, aes(x, y, fill = value)) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  transition_time(times) +
  coord_equal() +
  theme_bw() +
  theme_void() +
  labs(title = 'Month: {frame_time}') +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

anim_save("Ouputs Plots/NO2 IDW GIF.gif")

LNO2st$times <- as.integer(as.factor(LNO2st$time))

LNO2.df <- as.data.frame(LNO2st)
ggplot() + 
  geom_raster(data = LNO2.df, aes(x, y, fill = Trop.NO2)) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  transition_time(times) +
  coord_equal() +
  xlim(c(500000, 570000)) +
  ylim(c(140000, 210000)) +
  theme_bw() +
  theme_void() +
  labs(title = 'Month: {frame_time}') +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
max(LIDW.df$y)
anim_save("Ouputs Plots/NO2 London GIF.gif") 

LNO2.80 <- subset(LNO2.df, time == 80)
LNO2 <- ggplot() + 
  geom_raster(data = LNO2.80, aes(x, y, fill = Trop.NO2)) +
  scale_fill_viridis_c(option = "inferno") +
  #geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  coord_equal() +
  xlim(c(500000, 570000)) +
  ylim(c(140000, 210000)) +
  ggtitle("Raw Trop NO2") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
LNO2

min(LIDW.80$y)
LIDW.80 <- subset(LIDW.df, times == 80)
names(LIDW.80)[4] <- "IDW.NO2"
LIDW <- ggplot() + 
  geom_raster(data = LIDW.80, aes(x, y, fill = IDW.NO2)) +
  scale_fill_viridis_c(option = "inferno") +
  #geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  coord_equal() +
  #xlim(c(500000, 570000)) +
  #ylim(c(140000, 210000)) +
  ggtitle("IDW Trop NO2") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
LIDW

library(ggpubr)
ggarrange(LNO2, LIDW, 
          ncol = 2, nrow = 1)
