library(spatstat)
library(sp)
library(maptools)
library(INLA)
library(gstat)
library(devtools)
library(INLA)
data(bei)

allsites <- readRDS("Data/Even More Site Data.rds")
#london <- subset(allsites, Zone == "Greater London Urban Area")
all.0518 <- subset(allsites, year == 2018 & month == 5)

#london.0518 <- readRDS("Data/London May 2018 Obs.rds")
xy <- all.0518[,c("Easting", "Northing")]
all.sp <- SpatialPointsDataFrame(coords = xy,  data = all.0518, proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

all.sp <- subset(all.sp, !is.na(NO2))

london.grid <- readRDS("Data/All London May 2018.rds")
coordinates(london.grid) <- c("x", "y")
gridded(london.grid) <- TRUE
proj4string(london.grid) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"

names(all.sp)[c(14,16,44)] <- c("PM25PCM", "SNO2", "NO2PCM")

vgm <- variogram(NO2 ~ NO2PCM + SNO2 + population, all.sp)
fit.vgm <- fit.variogram(vgm, vgm("Mat"), fit.kappa= TRUE)

krg <- krige(NO2 ~ NO2PCM + SNO2 + population, all.sp, london.grid, model = fit.vgm)
london.grid$no2.krg <- krg$var1.pred
london.grid$no2.krg.sd <- sqrt(krg$var1.var)

saveRDS(krg, "NO2 krg.rds")
library(ggplot2)
library(inlabru)
library(viridis)
ggplot() +
  gg(data = london.grid, aes(x = x, y = y, fill = no2.krg)) + 
  scale_fill_viridis(option = "C") +
  coord_equal()

data(meuse)
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
gridded(meuse.grid) = TRUE
meuse.bdy <- unionSpatialPolygons(
  as(meuse.grid, "SpatialPolygons"), rep (1, length(meuse.grid))
)
pts.meuse <- meuse.bdy@polygons[[1]]@Polygons[[1]]@coords
mesh.meuse <- inla.mesh.2d(loc.domain = pts, max.edge = c(150, 500),
                     offset = c(100, 250) )
plot(pts.meuse)
par(mar = c(0, 0, 0, 0))
plot(mesh.meuse, asp = 1, main = "")
lines(pts.meuse, col = 3)

london.bdy <- unionSpatialPolygons(as(london.grid, "SpatialPolygons"), rep(1, length(london.grid)))
pts <- london.bdy@polygons[[1]]@Polygons[[1]]@coords
plot(pts)

mesh <- inla.mesh.2d(loc.domain = pts, max.edge = c(1000,10000), offset = c(100,250))             

par(mar = c(0, 0, 0, 0))
plot(mesh, asp = 1, main = "")
lines(pts, col = 3)

all.spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
A.all <- inla.spde.make.A(mesh = mesh, loc = coordinates(all.sp))
s.index <- inla.spde.make.index(name = "spatial.field",
                                n.spde = all.spde$n.spde)

all.stack <- inla.stack(data  = list(NO2 = all.sp$NO2),
                          A = list(A.all, 1),
                          effects = list(c(s.index, list(Intercept = 1)),
                                         list(NO2PCM = all.sp$NO2PCM, SNO2 = all.sp$SNO2, population = all.sp$population)),
                          tag = "all.data")

A.pred <- inla.spde.make.A(mesh = mesh, loc = coordinates(london.grid))
london.stack.pred <- inla.stack(data = list(NO2 = NA),
                               A = list(A.pred, 1),
                               effects = list(c(s.index, list (Intercept = 1)),
                                              list(NO2PCM = london.grid$NO2PCM, SNO2 = london.grid$SNO2, population = london.grid$population)),
                               tag = "london.pred")
join.stack <- inla.stack(all.stack, london.stack.pred)
form <- NO2 ~ -1 + Intercept + NO2PCM + SNO2 + population + f(spatial.field, model = spde)

m1 <- inla(form, data = inla.stack.data(join.stack, spde = all.spde),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(join.stack), compute = TRUE),
           control.compute = list(cpo = TRUE, dic = TRUE))

saveRDS(m1, "Data/INLA NO2 Spatial Model 1")
#Summary of results
summary(m1)

#m2
all.stack2 <- inla.stack(data  = list(NO2 = all.sp$NO2),
                        A = list(A.all, 1),
                        effects = list(c(s.index, list(Intercept = 1)),
                                       list(NO2PCM = all.sp$NO2PCM, SNO2 = all.sp$SNO2, population = all.sp$population, temp = all.sp$temp, humid = all.sp$humid, precip = all.sp$precip)),
                        tag = "all.data2")

A.pred <- inla.spde.make.A(mesh = mesh, loc = coordinates(london.grid))
london.stack.pred2 <- inla.stack(data = list(NO2 = NA),
                                A = list(A.pred, 1),
                                effects = list(c(s.index, list (Intercept = 1)),
                                               list(NO2PCM = london.grid$NO2PCM, SNO2 = london.grid$SNO2, population = london.grid$population,
                                                    temp = london.grid$temp, humid = london.grid$humid, precip = london.grid$precip)),
                                tag = "london.pred2")
join.stack2 <- inla.stack(all.stack2, london.stack.pred2)
form2 <- NO2 ~ -1 + Intercept + NO2PCM + SNO2 + population + temp + humid + precip + f(spatial.field, model = spde)

m2 <- inla(form2, data = inla.stack.data(join.stack2, spde = all.spde),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(join.stack2), compute = TRUE),
           control.compute = list(cpo = TRUE, dic = TRUE))

saveRDS(m2, "Data/INLA NO2 Spatial Model 2")
#Summary of results
summary(m2)

form3 <- NO2 ~ -1 + Intercept + SNO2 + population + temp + humid + precip + f(spatial.field, model = spde)

m3 <- inla(form3, data = inla.stack.data(join.stack2, spde = all.spde),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(join.stack2), compute = TRUE),
           control.compute = list(cpo = TRUE, dic = TRUE))

saveRDS(m3, "Data/INLA NO2 Spatial Model 3")
#Summary of results
summary(m3)


index.pred <- inla.stack.index(join.stack, "london.pred")$data
index.pred2 <- inla.stack.index(join.stack2, "london.pred2")$data


london.grid$no2.spde <- m1$summary.fitted.values[index.pred, "mean"]
london.grid$no2.spde.sd <- m1$summary.fitted.values[index.pred, "sd"]

london.grid$no2.spde2 <- m2$summary.fitted.values[index.pred, "mean"]
london.grid$no2.spde.sd2 <- m2$summary.fitted.values[index.pred, "sd"]

london.grid$no2.spde3 <- m3$summary.fitted.values[index.pred, "mean"]
london.grid$no2.spde.sd3 <- m3$summary.fitted.values[index.pred, "sd"]

saveRDS(london.grid, "NO2 London krg spde x3.rds")
library(ggplot2)
krg <- ggplot() +
  gg(data = london.grid, aes(x = x, y = y, fill = no2.krg)) + 
  scale_fill_viridis(option = "C") +
  theme_bw() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("NO2 Kriged (PCM, Sat, Pop)") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.title = element_text(size = 15)
  ) +
  coord_equal()
spde <- ggplot() +
  gg(data = london.grid, aes(x = x, y = y, fill = no2.spde)) + 
  scale_fill_viridis(option = "C") +
  theme_bw() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("NO2 SPDE (PCM, Sat, Pop)") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.title = element_text(size = 15)
  ) +
  coord_equal()
spde2 <- ggplot() +
  gg(data = london.grid, aes(x = x, y = y, fill = no2.spde2)) + 
  scale_fill_viridis(option = "C") +
  theme_bw() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("NO2 SPDE (PCM, Sat, Pop, Met)") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.title = element_text(size = 15)
  ) +
  coord_equal()
spde3 <- ggplot() +
  gg(data = london.grid, aes(x = x, y = y, fill = no2.spde3)) + 
  scale_fill_viridis(option = "C") +
  theme_bw() +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("NO2 SPDE (Sat, Pop, Met)") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.title = element_text(size = 15)
  ) +
  coord_equal()

library(ggpubr)
ggarrange(spde, spde2, spde3, nrow = 1, ncol = 3)

spde.est <- inla.spde2.result(inla = m1, name = "spatial.field",
                              spde = all.spde, do.transf = TRUE)
inla.zmarginal(spde.est$marginals.variance.nominal[[1]])
inla.zmarginal(spde.est$marginals.range.nominal[[1]])
fit.vgm
1 / fit.vgm$psill[1]

spde.est2 <- inla.spde2.result(inla = m2, name = "spatial.field",
                              spde = all.spde, do.transf = TRUE)
inla.zmarginal(spde.est2$marginals.variance.nominal[[1]])
inla.zmarginal(spde.est2$marginals.range.nominal[[1]])


spde.est3 <- inla.spde2.result(inla = m3, name = "spatial.field",
                              spde = all.spde, do.transf = TRUE)
inla.zmarginal(spde.est3$marginals.variance.nominal[[1]])
inla.zmarginal(spde.est3$marginals.range.nominal[[1]])



#next book
allsites <- readRDS("Data/Even More Site Data.rds")
#london <- subset(allsites, Zone == "Greater London Urban Area")
all.0518 <- subset(allsites, year == 2018 & month == 5)

#london.0518 <- readRDS("Data/London May 2018 Obs.rds")
xy <- all.0518[,c("Easting", "Northing")]
all.sp <- SpatialPointsDataFrame(coords = xy,  data = all.0518, proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

all.sp <- subset(all.sp, !is.na(NO2))

london.grid <- readRDS("Data/All London May 2018.rds")
coordinates(london.grid) <- c("x", "y")
gridded(london.grid) <- TRUE
proj4string(london.grid) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"

names(all.sp)[c(14,16,44)] <- c("PM25PCM", "SNO2", "NO2PCM")

#153 sites, 100 for model, 53 for validation
index.est <- sample(1:153, 100, replace=FALSE)

est.coord <- all.sp@coords[index.est,]
est.data <- sqrt(all.sp$NO2[index.est])
hist(est.data)
est.temp <- all.sp$temp[index.est]
est.humid <- all.sp$humid[index.est]
est.precip <- all.sp$precip[index.est]
est.population <- all.sp$population[index.est]
est.sno2 <- all.sp$SNO2[index.est]
est.no2pcm <- all.sp$NO2PCM[index.est]

val.coord <- all.sp@coords[-index.est,]
val.data <- sqrt(all.sp$NO2[-index.est])
hist(val.data)
val.temp <- all.sp$temp[-index.est]
val.humid <- all.sp$humid[-index.est]
val.precip <- all.sp$precip[-index.est]
val.population <- all.sp$population[-index.est]
val.sno2 <- all.sp$SNO2[-index.est]
val.no2pcm <- all.sp$NO2PCM[-index.est]

#london.grid 
x.res <- london.grid@grid@cells.dim[1]
y.res <- london.grid@grid@cells.dim[2]
pred.temp <- as.matrix(london.grid@data$temp)                                   
pred.humid <- as.matrix(london.grid@data$humid)
pred.precip <- as.matrix(london.grid@data$precip)
pred.population <- as.matrix(london.grid@data$population)
pred.sno2 <- as.matrix(london.grid@data$SNO2)
pred.no2pcm <- as.matrix(london.grid@data$NO2PCM)
temp.grid <- matrix(pred.temp, nrow = x.res, ncol = y.res, byrow = F)
humid.grid <- matrix(pred.humid, nrow = x.res, ncol = y.res, byrow = F)
precip.grid <- matrix(pred.precip, nrow = x.res, ncol = y.res, byrow = F)
population.grid <- matrix(pred.population, nrow = x.res, ncol = y.res, byrow = F)
sno2.grid <- matrix(pred.sno2, nrow = x.res, ncol = y.res, byrow = F)
no2pcm.grid <- matrix(pred.no2pcm, nrow = x.res, ncol = y.res, byrow = F)

seq.x.grid <- seq(from = london.grid@bbox[1,1], to = london.grid@bbox[2,1], length = x.res)
seq.y.grid <- seq(from = london.grid@bbox[1,2], to = london.grid@bbox[2,2], length = y.res)
pred.grid <- as.matrix(expand.grid(x = seq.x.grid, y = seq.y.grid))

london.mesh <- inla.mesh.2d(loc.domain = london.grid, max.edge = c(1000,10000))

no2.spde <- inla.spde2.matern(mesh = london.mesh, alpha = 2)

A.est <- inla.spde.make.A(mesh = london.mesh, loc = est.coord)
A.val <- inla.spde.make.A(mesh = london.mesh, loc = val.coord)

s.index <- inla.spde.make.index(name="spatial.field", n.spde = no2.spde$n.spde)

stack.est <- inla.stack(data = list(no2 = est.data), A = list(A.est, 1),
                        effects = list(c(s.index, list(Intercept = 1)), 
                                       list(temp = est.temp, humid = est.humid, precip = est.precip,
                                            pop = est.population, sno2 = est.sno2, no2pcm = est.no2pcm)), tag = "est")

stack.val <- inla.stack(data = list(no2 = val.data), A = list(A.val, 1),
                        effects = list(c(s.index, list(Intercept = 1)), 
                                       list(temp = val.temp, humid = val.humid, precip = val.precip,
                                            pop = val.population, sno2 = val.sno2, no2pcm = val.no2pcm)), tag = "val")

join.stack <- inla.stack(stack.est, stack.val)

formula <- no2 ~ -1 + Intercept + temp + humid + precip + pop + sno2 + no2pcm + f(spatial.field, model = spde)

no2.output <- inla(formula, data = inla.stack.data(join.stack, spde = no2.spde),
                   family = "gaussian", control.predictor = list(A = inla.stack.A(join.stack), compute = TRUE),
                   control.compute = list(cpo = TRUE, dic = TRUE))

index.val <- inla.stack.index(join.stack, "val")$data

post.mean.val <- no2.output$summary.linear.predictor[index.val,"mean"]
post.sd.val <- no2.output$summary.linear.predictor[index.val,"sd"]

A.pred <- inla.spde.make.A(mesh=london.mesh, loc=pred.grid)
stack.pred <- inla.stack(data = list(no2 = NA), A = list(A.pred, 1),
                         effects = list(c(s.index, list(Intercept = 1)), 
                                        list(temp = pred.temp, humid = pred.humid, precip = pred.precip,
                                             pop = pred.population, sno2 = pred.sno2, no2pcm = pred.no2pcm)), tag = "pred")

  
join.stack <- inla.stack(stack.est, stack.pred)
no2.output.pred <-  inla(formula, data = inla.stack.data(join.stack, spde=no2.spde),family="gaussian",control.predictor=list(A=inla.stack.A(join.stack),compute=TRUE))

index.pred.new <- inla.stack.index(join.stack, "stack.pred")$data

post.mean.pred <- no2.output.pred$summary.linear.predictor[index.pred,"mean"]
post.sd.pred <- no2.output.pred$summary.linear.predictor[index.pred,"sd"]

proj.grid <- inla.mesh.projector(london.mesh, xlim=range(pred.grid[,1]), ylim=range(pred.grid[,2]),dims=c(x.res,y.res))

post.mean.pred.grid <- inla.mesh.project(proj.grid, post.mean.pred)
post.sd.pred.grid <- inla.mesh.project(proj.grid, post.sd.pred)

london.grid$post.mean.pred <- no2.output.pred$summary.linear.predictor[index.pred,"mean"]

spde.new <- ggplot() +
  gg(data = london.grid, aes(x = x, y = y, fill = post.mean.pred)) + 
  scale_fill_viridis(option = "C") +
  coord_equal()
spde.new

plot(post.mean.pred.grid)

#other covariates...
stack.est.no... <- inla.stack(data = list(no2 = est.data), A = list(A.est, 1),
                              effects = list(c(s.index, list(Intercept = 1)), 
                                             list(temp = est.temp, humid = est.humid, precip = est.precip,
                                                  pop = est.population, sno2 = est.sno2, no2pcm = est.no2pcm)), tag = "est")

stack.val.no... <- inla.stack(data = list(no2 = val.data), A = list(A.val, 1),
                              effects = list(c(s.index, list(Intercept = 1)), 
                                             list(temp = val.temp, humid = val.humid, precip = val.precip,
                                                  pop = val.population, sno2 = val.sno2, no2pcm = val.no2pcm)), tag = "val")

#compare with DIC???