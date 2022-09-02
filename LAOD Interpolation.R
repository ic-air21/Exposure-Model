library(spacetime)
library(sp)
library(gstat)
library(rgdal)
library(units)
library(spatstat)

setwd("~/PhD/R/Objective 1")
#https://cran.r-project.org/web/packages/spacetime/vignettes/jss816.pdf

LAOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAODst.rds")
proj4string(LAOD) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
LAOD$AOD100 <- LAOD$AOD*100

LAOD.df <- as.data.frame(LAOD)
LAOD.80 <- subset(LAOD.df, timeIndex == 80)

LAOD.plot <- ggplot() + 
  geom_raster(data = LAOD.80, aes(x, y, fill = AOD)) +
  scale_fill_viridis_c(option = "inferno") +
  #transition_time(timeIndex) +
  coord_equal() +
  theme_bw() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Raw AOD") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
LAOD.plot

LIDW.80 <- subset(LIDW.df, time == "layer.80")
LIDW.plot <- ggplot() + 
  geom_raster(data = LIDW.80, aes(x, y, fill = value)) +
  scale_fill_viridis_c(option = "inferno") +
  coord_equal() +
  theme_bw() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("IDW AOD") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

LIDW.plot
ggplot() + 
  geom_raster(data = LAOD.df, aes(x, y, fill = AOD)) +
  scale_fill_viridis_c(option = "inferno") +
  transition_time(timeIndex) +
  coord_equal() +
  theme_bw() +
  theme_void() +
  labs(title = 'Month: {frame_time}') +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

anim_save("Ouputs Plots/AOD GIF.gif") 

LAOD.jan <- LAOD[,index(LAOD@time[c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,179)])]
LAOD.janfeb15 <- LAOD[,index(LAOD@time[c(121, 122)])]




stplot(LAOD.janfeb15)
stplot(LAOD.jan, mode = "tp")
stplot(LAOD.jan, mode = "xt")

empVgm <- variogramST(AOD100 ~ 1, LAOD, tlags=0:6, tunit = "month")
plot(empVgm, wireframe=T, scales=list(arrows=F))
plot(empVgm)

saveRDS(empVgm, "Data/Air Pollution/MAIAC AOD/LAODempVgm.rds")
empVgm <- readRDS("Data/Air Pollution/MAIAC AOD/LAODempVgm.rds")
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

linStAni <- estiStAni(empVgm, c(220000,230000)) #A spatio-temporal anisotropy; the number of space unitsequivalent to one time unit.

plot(gamma ~ dist, empVgm[empVgm$timelag == 0,], ylim=c(0,100), xlim=c(0,50000))
points(empVgm[empVgm$spacelag == 0,]$timelag*linStAni, empVgm[empVgm$spacelag == 0,]$gamma, col="red")


# rescale empVgm and linStAni to km for estimation
empVgm$dist  <- empVgm$dist/1000
empVgm$avgDist  <- empVgm$avgDist/1000
empVgm$spacelag <- empVgm$spacelag/1000

linStAni <- linStAni/1000

# separable
separableModel <- vgmST("separable", 
                        space = vgm(0.9, "Exp", 200, 0.1),
                        time = vgm(0.9, "Sph", 3.5, 0.1),
                        sill=120)
fitSepModel <- fit.StVariogram(empVgm, separableModel, fit.method = 7, 
                               stAni = linStAni, method = "L-BFGS-B", 
                               control = list(parscale=c(100,1,10,1,100)),
                               lower = c(10,0,.1,0,0.1), 
                               upper = c(2000,1,12,1,200))
attr(fitSepModel, "optim.output")$value
# Exp+Exp: 388933.8, Exp+Sph: 333600.6, Sph+Exp: 438552, Sph+Sph: 385890.7
plot(empVgm, fitSepModel, wireframe=T, all=T, scales=list(arrows=F), zlim=c(0,120))

saveRDS(fitSepModel, "Data/Air Pollution/MAIAC AOD/LAODSepModel.rds")

# product-sum
prodSumModel <- vgmST("productSum",
                      space=vgm(10, "Sph", 3, 1),
                      time= vgm(10, "Sph",  2, 1), 
                      k=2)
fitProdSumModel <- fit.StVariogram(empVgm, prodSumModel, fit.method = 7, 
                                   stAni = linStAni, method = "L-BFGS-B", 
                                   control = list(parscale = c(1,10,1,1,0.1,1,10)),
                                   lower = rep(0.0001,7))
attr(fitProdSumModel, "optim.output")$value
# Exp+Exp: 423085.9, Exp+Sph: 441943.8, Sph+Exp: 484667, Sph+Sph: 415961.3
plot(empVgm, fitProdSumModel, wireframe=T, all=T, scales=list(arrows=F), zlim=c(0,120))

saveRDS(fitProdSumModel, "Data/Air Pollution/MAIAC AOD/LAODProdSumModel.rds")

# metric
metricModel <- vgmST("metric",
                     joint = vgm(60, "Mat", 150, 10, kappa = 0.561),
                     stAni = 200)
fitMetricModel <- fit.StVariogram(empVgm, metricModel, fit.method = 7,
                                  stAni = linStAni, method = "L-BFGS-B",
                                  control = list(parscale = c(10,20,5,10)),
                                  lower = c(80,50,5,50),
                                  upper = c(200,1500,60,300))
attr(fitMetricModel, "optim.output")$value 
# Exp: 1014759, Sph: 1256262,
# Gau: 4385097, Mat 5: 25778902, Mat 2: 12332015, Mat 1: 4137330,
# Mat 3: 17648188, Mat 0.5: 1014759, Mat 0.6: 977475.7, Mat 0.55: 908296.6,
# Mat 0.57: 906605.8, Mat 0.565: 902857.8, Mat 0.561: 901821.7
plot(empVgm, fitMetricModel, wireframe=T, all=T, scales=list(arrows=F), zlim=c(0,120))

saveRDS(fitMetricModel, "Data/Air Pollution/MAIAC AOD/LAODMetricModel.rds")

# simplified sumMetric model?
sumMetricFromsimpleSumMetric <- function(vgm) {
  vgmST("sumMetric",
        space=vgm(vgm$space$psill, vgm$space$model, vgm$space$range, vgm$nugget/3),
        time =vgm(vgm$time$psill, vgm$time$model, vgm$time$range, vgm$nugget/3),
        joint=vgm(vgm$joint$psill, vgm$joint$model, vgm$joint$range, vgm$nugget/3),
        stAni=vgm$stAni)
}

simpleSumMetricModel <- vgmST("simpleSumMetric",
                              space=vgm(120,"Sph", 150),
                              time =vgm(120,"Exp", 10),
                              joint=vgm(120,"Exp", 150),
                              nugget=10, stAni=150)
fitSimpleSumMetricModel <- fit.StVariogram(empVgm, simpleSumMetricModel,
                                           fit.method = 7, stAni=linStAni,
                                           method = "L-BFGS-B",
                                           lower = c(sill.s = 0, range.s = 10,
                                                     sill.t = 0, range.t = 0.1,
                                                     sill.st= 0, range.st= 10,
                                                     nugget=0, anis = 40),
                                           upper = c(sill.s = 200,  range.s = 500,
                                                     sill.t = 200,  range.t = 20,
                                                     sill.st= 200, range.st = 5000,
                                                     nugget = 100, anis = 1000),
                                           control = list(parscale = c(1,10,1,1,1,100,1,10)))
attr(fitSimpleSumMetricModel, "optim.output")$value
# Exp+Exp+Exp: 323541.1 Exp+Sph+Exp: 334106.4 Sph+Exp+Exp: 117442.7 Sph+Sph+Exp: 288092.5
# Exp+Exp+Sph: 152166.3 Exp+Sph+Sph: 212315 Sph+Exp+Sph: 185127.5 Sph+Sph+Sph: 203099.7
plot(empVgm,fitSimpleSumMetricModel, wireframe = T, scales = list(arrows = F), all = T , zlim=c(0,130))

saveRDS(fitSimpleSumMetricModel, "Data/Air Pollution/MAIAC AOD/LAODSimpleSumMetricModel.rds")

# sum-metric
# sumMetricModel <- sumMetricFromsimpleSumMetric(fitSimpleSumMetricModel)

sumMetricModel <- vgmST("sumMetric",
                        space = vgm(20, "Exp", 150, 1),
                        time = vgm(10, "Exp", 2, 0.5),
                        joint = vgm(80, "Exp", 1500, 2.5),
                        stAni = 120, temporalUnit = "month")
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

######
plot(empVgm, list(fitSepModel, fitProdSumModel, fitMetricModel,
                  fitSumMetricModel, fitSimpleSumMetricModel), 
     wireframe=T, all=T, zlim=c(0,130), ylim=c(0,0.5), xlim=c(0,25),
     scales=list(arrows = F, cex=.8,
                 x=list(at=0:5*5), 
                 y=list(at=0:5*0.1, labels=c("0 ","","0.25","","","0.5 ")), 
                 z=list(at=0:5*25, labels=c("0  ","","50   ","","100    ",""))),
     at=0:100*1.4,
     xlab=list("space [km]", rot=27, cex=0.8), 
     ylab=list("time [months]", rot=-40, cex=0.8),
     zlab=list(NULL, rot=94, cex=0.8))

plot(empVgm, list(fitSepModel, fitProdSumModel, fitMetricModel,
                  fitSumMetricModel, fitSimpleSumMetricModel), 
     wireframe=T,  all=T, zlim=c(-10,10), ylim=c(0,0.5), xlim=c(0,25), 
     diff=TRUE,
     scales=list(arrows = F, cex=.8,
                 x=list(at=0:5*5), 
                 y=list(at=0:5*0.1, labels=c("0 ","","0.25","","","0.5 ")), 
                 z=list(at=-2:5*5, labels=c("-10  ","","0  ","","10  ","","20  ",""))),
     xlab=list("space [km]", rot=27, cex=0.8), 
     ylab=list("time [days]", rot=-40, cex=0.8),
     zlab=list(NULL, rot=94, cex=0.8))

#select summetric 
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

fitSumMetricModel$
L_pred <- STF(as(gridL, "SpatialPoints"), LAOD@time)
  
predL <- krigeST(AOD100 ~ 1, data = LAOD, modelList = fitSumMetricModel, newdata=L_pred) 

library(stars)
library(spacetime)
library(raster)
#IDW spatial interpolation
LAODst <- readRDS("Data/Air Pollution/MAIAC AOD/LAODst.rds")
gridL <- SpatialGrid(GridTopology(LAODst@sp@bbox[,1]%/%10000*1000, c(1000,1000),
                                  cells.dim=ceiling(apply(LAODst@sp@bbox,1,diff)/1000)))
London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
proj4string(London) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
gridL <- raster(extent(London), resolution = c(1000,1000), crs = proj4string(London))

grid <- st_as_stars(gridL, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

LAOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD.rds")
LAOD <- st_as_sf(LAOD, coords = c("x","y"), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
LAOD <- na.omit(LAOD)

g <- gstat(formula = AOD ~ 1, data = LAOD)

LIDW <- list()
for (i in 1:192) {
  LAODmonth <- subset(LAOD, (year-2005)*12+month == i)
  g <- gstat(formula = AOD ~ 1, data = LAODmonth)
  LIDW[[i]] <- predict(g, grid)
  LIDW[[i]] = LIDW[[i]]["var1.pred",,]
}

saveRDS(LIDW, "Data/Air Pollution/MAIAC AOD/LAOD Spatial IDW.rds")

LIDW <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD Spatial IDW.rds")

b = seq(0.01, 0.26, 0.01)

plot(LIDW[[1]], breaks = b, col = hcl.colors(length(b)-1, "Spectral"), reset = FALSE)
plot(London, pch = 3, add = TRUE)
contour(LIDW, breaks = b, add = TRUE)

library(raster)
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

saveRDS(LIDW.df, "Data/Air Pollution/MAIAC AOD/LAOD Spatial IDW DF.rds" )
LIDW.df <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD Spatial IDW DF.rds" )
library(ggplot2)
library(gganimate)


dates <- cbind(seq(1,192,1), dates)
LIDW.df <- merge(LIDW.df, dates, x.by = time, y.by = V1)

LAOD.80 <- subset(LAOD, time == 80)
LIDW.162 <- subset(LIDW.df, time == "layer.162")
LIDW.162$AOD <- LIDW.162$value
LIDW.80 <- subset(LIDW.df, time == "layer.80")
LIDW.80$AOD <- LIDW.80$value

predL.80 <- subset(predL.df, time == 80)

ggplot() + 
  geom_raster(data = predL.80, aes(x, y, fill = AOD)) +
  scale_fill_viridis_c(option = "inferno") +
  #scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  #transition_time(time) +
  coord_equal() +
  theme_bw() +
  theme_void() +
  #labs(title = '{dates}') +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

LIDW.df$times <- as.integer(as.factor(LIDW.df$time))
ggplot() + 
  geom_raster(data = LIDW.df, aes(x, y, fill = value)) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  transition_time(times) +
  coord_equal() +
  theme_bw() +
  theme_void() +
  labs(title = 'Month: {frame_time}') +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

anim_save("Ouputs Plots/IDW GIF.gif") 

#OK montly try 2
result = list()

LAOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD.rds")
LAOD <- na.omit(LAOD)

LAOD$time <- (LAOD$year-2005)*12 + LAOD$month

LAOD.162 <- subset(LAOD, time == 162)

xy <- LAOD[,c(1,2)]
LAOD.spdf <- SpatialPointsDataFrame(coords = xy, data = LAOD.df,
                                    proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

LAOD <- LAOD[,c("x","y","AOD","time")]
LAOD <- reshape(LAOD, idvar = c("x","y"), timevar = "time", direction = "wide")

times <- c()
for (i in 1:192){
  times <- append(times, paste("AOD.", i, sep = ""))
}

y = st_as_sf(LAOD, coords = c("x", "y"), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

library(automap)
for(i in times) {
  f = as.formula(paste0(i, " ~ 1"))
  v = autofitVariogram(f, as(y, "Spatial"))
  g = gstat(formula = f, model = v$var_model, data = LAOD)
  z = predict(g, grid, na.action = na.pass)
  z = z["var1.pred",,]
  result[[i]] = z
}
