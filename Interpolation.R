library(rgdal)     
library(sp)         
library(spBayes)                     
library(coda)      
library(ggmcmc)   
library(GGally)   
library(MBA) 
library(fields)
library(tidyverse)

allsites <- readRDS("Data/Even More Site Data.rds")
all.0518 <- subset(allsites, year == 2018 & month == 5)
saveRDS(all.0518, "Data/all May 2018.rds")

#all.0518 <- readRDS("Data/London May 2018 Obs.rds")
xy <- all.0518[,c("Easting", "Northing")]
all.sp <- SpatialPointsDataFrame(coords = xy,  data = all.0518, proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

all.sp <- subset(all.sp, !is.na(NO2))

bubble(all.sp, "NO2", maxsize = 2)
summary(all.sp$NO2)
hist(all.sp$NO2)
#ggpairs(all.sp[,5:16])

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
proj4string(London) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

bubble(all.sp, zcol="NO2", maxsize=2, sp.layout=list("sp.polygons", London, col="red"))

d.max <- max(iDist(coords))

par(mfrow=c(3,4))
hist(all.sp$PM25)
hist(all.sp$NO2)
hist(all.sp$temp)
hist(all.sp$precip)
hist(all.sp$humid)
hist(all.sp$`PM25 PCM`)
hist(all.sp$`NO2 PCM`)
hist(all.sp$AOD47)
hist(all.sp$AOD55)
hist(all.sp$`Satellite NO2`)
hist(all.sp$population)
hist(all.sp$`Altitude (m)`)

names(all.sp)[c(14,16,44)] <- c("PM25PCM", "SNO2", "NO2PCM")
all.sp <- all.sp[,c("temp", "precip", "humid", "PM25PCM", "NO2PCM", "AOD47", "AOD55", "SNO2", "population", "NO2", "Easting", "Northing")]
all.sp <- na.omit(all.sp)
coords <- as.matrix(cbind(all.sp@data$Easting, all.sp@data$Northing))
coords <- as.matrix(all.sp@coords)

priors <- list("beta.Flat", "tau.sq.IG"=c(2, 1), "sigma.sq.IG"=c(2, 1), "phi.Unif"=c(3, 30))
starting <- list("tau.sq"=1, "sigma.sq"=1, "phi"=6)
tuning <- list("phi"=0.1, "sigma.sq"=0.05,"tau.sq"=0.1)
n.samples <- 100000

m1 <- spLM(NO2 ~ temp + precip + humid + PM25PCM + NO2PCM + AOD47 + AOD55 + SNO2 + population, data = all.sp, coords=coords, starting=starting,
           tuning=tuning, priors=priors, cov.model="exponential",
           n.samples=100000)

thinning <- 10
burn.in <- 0.3*n.samples
m.1 <- spRecover(m1, start=burn.in, thin=thinning, verbose=FALSE) 
theta.samples <- m.1$p.theta.recover.samples

plot(theta.samples[,1:3], auto.layout=TRUE, density=TRUE)
autocorr.plot(theta.samples[,1:3])

beta.samples <- m.1$p.beta.recover.samples
plot(beta.samples[,1:10], auto.layout=TRUE, density=TRUE)
autocorr.plot(beta.samples[,1:10])

round(summary(m.1$p.beta.recover.samples)$quantiles[,c(3,1,5)],3)

w.samples <- m.1$p.w.recover.samples
w.hat.mu <- apply(w.samples,1,mean)
w.hat.sd <- apply(w.samples,1,sd)
surf.mu <- mba.surf(cbind(coords, w.hat.mu), no.X=40, no.Y=40, extend=FALSE)$xyz.est
z.lim <- range(surf.mu[[3]], na.rm=TRUE)
par(mfrow=c(1,1))
image.plot(surf.mu, xaxs = "r", yaxs = "r", zlim=z.lim, main="Mean spatial effects")




#predictive process
all.sp[c(5:16,28,32:38)]
cov(as.data.frame(all.sp[c(5:16,28,32:38)]))

library(cluster)

m <- 3
km.knots <- kmeans(coords, m)$centres
cl.knots <- clara(coords, m)$mediods
cd.knots <- cover.design(coords, nd = m)$design

par(mfrow=c(1,1))
plot(coords, pch=19, cex=0.5)
points(km.knots, pch=5, cex=1, col="blue")
points(cl.knots, pch=6, cex=1, col="green")
points(cd.knots, pch=7, cex=1, col="red")


#univariate 2
n.samples <- 20000
p <- 9
model2 <- NO2 ~ temp + precip + humid + PM25PCM + NO2PCM + AOD55 + SNO2 + population
starting2 <- list("phi" = 3/500, "sigma.sq" = 0.05, "tau.sq" = 0.05)
tuning2 <- list("phi" = 0.1, "sigma.sq" = 0.0001, "tau.sq" = 0.1)
priors2.1 <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                  "phi.Unif"=c(3/1000, 3/100), "sigma.sq.IG"=c(2, 2),
                  "tau.sq.IG"=c(2, 0.1))
priors2.2 <- list("beta.Flat", "phi.Unif"=c(3/1, 3/0.1),
                  "sigma.sq.IG"=c(2, 2), "tau.sq.IG"=c(2, 0.1))

all.spLM <- spLM(model2, coords = coords, knots = cd.knots, data = all.sp, 
                    starting=starting2,
                    tuning=tuning2,
                    priors=priors2.1, 
                    cov.model="exponential",
                    n.samples=n.samples, verbose=TRUE, n.report=1000)

burn.in <- floor(0.75*n.samples)
all.spLM <- spRecover(all.spLM, start=burn.in)
round(summary(mcmc(cbind(all.spLM$p.beta.recover.samples, all.spLM$p.theta.recover.samples)))$quantiles[,c(1,3,5)],3)

fitted2 <- spPredict(all.spLM, start=burn.in, thin=10, pred.covars=all.spLM$X, pred.coords=all.spLM$coords)

y.hat2 <- rowMeans(fitted2$p.y.predictive.samples)
NO22 <- y.hat2[seq(1,length(y.hat2),1)]


res <- 100


par(mfrow=c(1,1))
surf2 <- mba.surf(cbind(coords,NO22), no.X=res, no.Y=res, extend=FALSE)$xyz.est
image.plot(surf2, main="NO2 fitted values")
points(coords)


x.range <- range(coords[,1])
y.range <- range(coords[,2])

London <- spTransform(London, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
London.poly <- as.matrix(London@polygons[[1]]@Polygons[[1]]@coords)

London.grids <- readRDS("Data/All London May 2018.rds")
#London.grids <- na.omit(London.grids)
pred.covars <- London.grids[,c("temp", "precip", "humid", "PM25PCM", "NO2PCM", "AOD", "SNO2", "population")]


pred.covars <- cbind(rep(1,nrow(pred.covars)), pred.covars)
pred.covars <- as.matrix(pred.covars)

library(raster)
#gridL <- raster(extent(London), resolution = c(1000,1000), crs = proj4string(London))
#gridL <- as(gridL, 'SpatialGrid')

#proj4string(gridL) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
#fullgrid(gridL) <- F

#ind <- over(gridL, as(London,"SpatialPolygons"))

pred.coords <- London.grids[,c("x", "y")]

London.pred2 <- spPredict(London.spLM, start = burn.in, thin = 5, pred.coords = pred.coords, pred.covars = pred.covars)

y.pred.mu2 <- apply(London.pred2$p.y.predictive.samples, 1, mean)
y.pred.sd2 <- apply(London.pred2$p.y.predictive.samples, 1, sd)
NO2.pred.mu2 <- y.pred.mu2[seq(1,length(y.pred.mu2),1)]
NO2.pred.sd2 <- y.pred.sd2[seq(1,length(y.pred.sd2),1)]

NO2.pred2 <- as.data.frame(cbind(pred.coords, c(NO2.pred.mu2), c(NO2.pred.sd2)))
#PM25.pred <- subset(PM25.pred, V3 >= 0)


pred.grid2 <- as.data.frame(list(x=NO2.pred2[,1], y=NO2.pred2[,2], 
                                 NO2.mu = NO2.pred2[,3], NO2.sd = NO2.pred2[,4]))

coordinates(pred.grid2) <- c("x", "y")
gridded(pred.grid2) <- TRUE



toImage <- function(x){as.image.SpatialGridDataFrame(x)}
res <- 100
par(mfrow = c(1,2))
surf2 <- mba.surf(cbind(coords, london.sp$NO2), no.X = res, no.Y = res, extend = FALSE)$xyz.est
z.lim <- range(surf2[["z"]], na.rm = TRUE)
image.plot(surf2, xaxs = "r", yaxs = "r", main = "Observed NO2")
pred.grid2$NO2.scaled <- exp(pred.grid2$NO2.mu)
image.plot(toImage(pred.grid2["NO2.mu"]), xaxs = "r", yaxs = "r", main = "Predicted NO2")


predNO2 <- merge(London.grids, pred.grid2, x.by = c("Easting", "Northing"), y.by = c("x","y"))


pred.grid2 <- as.data.frame(list(x=NO2.pred2[,1], y=NO2.pred2[,2], 
                                 NO2.mu = NO2.pred2[,3], NO2.sd = NO2.pred2[,4]))

ggplot() + 
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = pred.grid2, aes(x = x, y = y, fill = NO2.mu, alpha = 0.9)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  coord_equal()

ggplot() + 
  geom_polygon(data = London, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_raster(data = pred.grid2, aes(x = x, y = y, fill = NO2.mu, alpha = 0.9)) +
  geom_point(data = london.0518, aes(x = Easting, y = Northing, color = NO2)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  coord_equal()


#cross validation???