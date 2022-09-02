library(readxl)
library(ggplot2)
library(ggmap)
library(rgdal)
library(bmstdr)
library(spBayes)
library(rstan)
library(sp)
library(rgdal)
library(gstat)


May18 <- readRDS("Data/ObsMay2018.rds")

library(tidyverse)
library(caret)

data <- na.omit(May18)

set.seed(123, sample.kind = "Rejection")
training.samples <- data$pm25 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]

model1 <- lm(pm25 ~ ., data = train.data)
predictions1 <- model1 %>% predict(test.data)
data.frame(R2 = R2(predictions1, test.data$pm25),
           RMSE = RMSE(predictions1, test.data$pm25),
           MAE = MAE(predictions1, test.data$pm25))
print(model1)
summary(model1)

train.control2 <- trainControl(method = "LOOCV")
model2 <- train(pm25 ~ ., data = data, method = "lm", trControl = train.control2)
print(model2)
summary(model2)

train.control3 <- trainControl(method = "cv", number = 10)
model3 <- train(pm25 ~ ., data = data, method = "lm", trControl = train.control3)
print(model3)
summary(model3)

train.control4 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model4 <- train(pm25 ~ ., data = data, method = "lm", trControl = train.control4)
print(model4)
summary(model4)

library(arm)
model5 <- train(pm25 ~ ., data = data, method = "bayesglm", trControl = train.control3)
print(model5)

library(gam)
model6 <- train(pm25 ~ ., data = data, method = "gam", trControl = train.control3)
print(model6)

library(monomvn)
model7 <- train(pm25 ~ ., data = data, method = "bridge", trControl = train.control3)
print(model7)

library(spikeslab)
library(plyr)
model8 <- train(pm25 ~ ., data = data, method = "spikeslab", vars = 3, trControl = train.control3)
print(model8)

library(kernlab)
model9 <- train(pm25 ~ ., data = data, method = "gaussprLinear", trControl = train.control3)
print(model9)

model10 <- train(pm25 ~ ., data = data, method = "gamSpline", df = 3, trControl = train.control3)
print(model10)

model11 <- train(pm25 ~ ., data = data, method = "blassoAveraged", trControl = train.control3)
print(model11)

library(bst)
model12 <- train(pm25 ~ ., data = data, method = "BstLm", trControl = train.control3)
print(model12)

library(leaps)
model13 <- train(pm25 ~ ., data = data, method = "leapBackward", trControl = train.control3)
print(model13)

model14 <- train(pm25 ~ ., data = data, method = "leapForward", trControl = train.control3)
print(model14)

library(MASS)
model15 <- train(pm25 ~ ., data = data, method = "lmStepAIC", trControl = train.control3)
print(model15)

library(penalized)
model16 <- train(pm25 ~ ., data = data, method = "penalized", trControl = train.control3)
print(model16)

library(pls)
model17 <- train(pm25 ~ ., data = data, method = "pcr", trControl = train.control3)
print(model17)

library(elasticnet)
model18 <- train(pm25 ~ ., data = data, method = "ridge", trControl = train.control3)
print(model18)

model19 <- train(pm25 ~ ., data = data, method = "rlm", trControl = train.control3)
print(model19)


library(spBayes)      # software for univariate and multivariate                               # spatial and spatio-temporal random effects                             # models for geostatistical data
library(coda)         # tools for convergence diagnostics 
library(ggmcmc)       # tools for MCMC diagnostics using ggplot
library(GGally)       # tools for plots (extend functionalities of
# ggplot2)
library(MBA)          # Functions to interpolate irregularly and 
# regularly spaced data 
library(fields)
coords <- cbind(data$x, data$y)
p = 9
priors <- list("beta.Flat",
               "phi.Unif"=c(3/(0.55*10000), 3/(0.01*10000)),
               "sigma.sq.IG"=c(2, 1),
               "tau.sq.IG"=c(2, 1))
starting <- list("phi" = 0.02, "sigma.sq" = 0.5, "tau.sq" = 0.05)
tuning <- list("phi" = 0.1, "sigma.sq" = 0.05,"tau.sq" = 0.1)
n.samples <- 20000
n.report <- 5000

sim.m <- spLM(pm25 ~ no2 + temp + precip + humid + pcm+ aod,
              data=data, coords=coords, 
              starting=starting,
              tuning=tuning,
              priors=priors, 
              cov.model="exponential",
              n.samples=n.samples,
              n.report=n.report)
round(summary(mcmc(sim.m$p.theta.samples))$quantiles,3)

burn.in <- 0.6*n.samples # we exclude a burn-in period
m.1 <- spRecover(sim.m, start=burn.in, thin = 100, verbose=FALSE)
theta.samples <- m.1$p.theta.recover.samples
plot(theta.samples[,1:3], auto.layout=TRUE, density=TRUE)
autocorr.plot(theta.samples[,1:3])
beta.samples <- m.1$p.beta.recover.samples
plot(beta.samples[,1:7], auto.layout=TRUE, density=TRUE)
autocorr.plot(beta.samples[,1:7])

beta.ggsamples <- ggs(m.1$p.beta.recover.samples)
beta.ggsamples %>% filter(Parameter == "aod") %>% 
  ggs_density() + theme_bw()

round(summary(m.1$p.theta.recover.samples)$quantiles[,c(3,1,5)],3)
round(summary(m.1$p.beta.recover.samples)$quantiles[,c(3,1,5)],3)

w.samples <- m.1$p.w.recover.samples
w.hat.mu <- apply(w.samples,1,mean) # posterior mean of the spatial 
# effects
w.hat.sd <- apply(w.samples,1,sd) # posterior sd of the spatial 
# effects

surf.mu <- mba.surf(cbind(coords, w.hat.mu), no.X=40, no.Y=40, extend=FALSE)$xyz.est
# note xyz: a nx3 matrix or data frame, 
# where n is the number of observed points. 
# The three columns correspond to point x, y, and z coordinates. 
# The z value is the response at the given x, y coordinates.
z.lim <- range(surf.mu[[3]], na.rm=TRUE)
par(mfrow = c(1,1))
image.plot(surf.mu, xaxs = "r", yaxs = "r", # style of x and y axis.
           zlim=z.lim, main="Mean spatial effects")

library(sf)
library(sp)
library(rgdal)
library(spBayes)                     
library(coda)      
library(ggmcmc)   
library(GGally)   
library(MBA) 
library(fields)
library(tidyverse)
library(raster)
UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))
UK <- st_as_sf(UK)

UK.grid <- readRDS("Data/May2018.rds")

pred.coords <- UK.grid[,c(1,2)]

pred.covars <- cbind(1, UK.grid[,c(3:8)])
pred.covars <- as.matrix(pred.covars)

UK.pred <- spPredict(m.1, start=burn.in, thin=20,
                        pred.coords=pred.coords,
                        pred.covars=pred.covars)

y.hat2 <- rowMeans(UK.pred$p.y.predictive.samples)
PM252 <- y.hat2[seq(1,length(y.hat2),1)]
res <- 100

coords <- as.matrix(data[,c("x","y")], nrow = 64)
par(mfrow=c(1,1))
surf2 <- mba.surf(cbind(pred.coords,PM252), no.X=res, no.Y=res, extend=TRUE)$xyz.est
unique(is.na(surf2))
image.plot(surf2, main="PM25 fitted values")
points(coords)


y.pred.mu <- apply(UK.pred$p.y.predictive.samples, 1, mean)
y.pred.sd <- apply(UK.pred$p.y.predictive.samples, 1, sd)
PM25.pred.mu <- y.pred.mu[seq(1,length(y.pred.mu),1)]
PM25.pred.sd <- y.pred.sd[seq(1,length(y.pred.sd),1)]

PM25.pred <- as.data.frame(cbind(pred.coords, c(PM25.pred.mu), c(PM25.pred.sd)))
#PM25.pred <- subset(PM25.pred, V3 >= 0)
pred.grid <- as.data.frame(list(x=PM25.pred[,1], y=PM25.pred[,2], 
                                 PM25.mu = PM25.pred[,3], PM25.sd = PM25.pred[,4]))

coordinates(pred.grid) <- c("x", "y")
gridded(pred.grid) <- TRUE

pred.grid$PM25.scaled <- log(-pred.grid$PM25.mu/500 +1.3)*5
hist(pred.grid$PM25.scaled)

toImage <- function(x){as.image.SpatialGridDataFrame(x)}
res <- 100
par(mfrow = c(1,2))
surf2 <- mba.surf(cbind(coords, data$pm25), no.X = res, no.Y = res, extend = FALSE)$xyz.est
z.lim <- range(surf2[["z"]], na.rm = TRUE)
image.plot(surf2, xaxs = "r", yaxs = "r", main = "Observed PM2.5")
image.plot(toImage(pred.grid["PM25.scaled"]), xaxs = "r", yaxs = "r", main = "Predicted PM2.5")
