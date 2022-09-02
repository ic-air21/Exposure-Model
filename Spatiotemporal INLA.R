library(spatstat)
library(sp)
library(maptools)
library(INLA)
library(gstat)
library(devtools)
library(INLA)

allsites <- readRDS("Data/Even More Site Data.rds")
sites2018 <- subset(allsites, year == 2018)

n_stations <- length(unique(sites2018[,"Site Name"]))
n_data <- nrow(sites2018)
n_months <- n_data/n_stations

coordinates.allyear <- as.matrix(cbind(sites2018[,"Easting"], sites2018[,"Northing"]))
dim(coordinates.allyear)

sites2018$logNO2 <- log(sites2018$NO2)

mean_covariates <- apply(sites2018[,c(5,6,7,13,16,44)], 2, mean)
sd_covariates <- apply(sites2018[,c(5,6,7,13,16,44)], 2, sd)
sites2018[,c(5,6,7,13,16,44)] <- scale(sites2018[,c(5,6,7,13,16,44)], center = mean_covariates, scale = sd_covariates)

library(rgdal) 
#GB <- readOGR("Data/UK Shapefile/GBR_adm/GBR_adm0.shp", stringsAsFactors = FALSE)
#proj4string(GB) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

#bdy <- bbox(GB)

#uk_mesh <- inla.mesh.2d(loc = coordinates.allyear, loc.domain = bdy, max.edge=c(1000,10000))                                 

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
proj4string(London) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

saveRDS(London, "Data/London.rds")

bdy_london <- bbox(London)

london_mesh <- inla.mesh.2d(loc = coordinates.allyear, loc.domain = bdy_london, max.edge=c(1000,10000))                                 

par(mar = c(0, 0, 0, 0))
plot(london_mesh, asp = 1, main = "")

london_spde <- inla.spde2.matern(mesh = london_mesh, alpha = 2)

A_est <- inla.spde.make.A(mesh = london_mesh, loc = coordinates.allyear,
                          group = sites2018$time, n.group = n_months)

s_index <- inla.spde.make.index(name = "spatial.field", n.spde = london_spde$n.spde,
                                n.group = n_months)

stack_est <- inla.stack(data = list(logNO2 = sites2018$logNO2),
                        A = list(A_est, 1),
                        effects = list(c(s_index, list(Intercept = 1)),
                                       list(sites2018[,c(5,6,7,13,16,44)])),
                        tag = "est")

#pred a month???
london.grid <- readRDS("Data/All London 2018.rds")
coordinates(london.grid) <- c("x", "y")
gridded(london.grid) <- TRUE
proj4string(london.grid) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"

A_pred <- inla.spde.make.A(mesh = london_mesh, loc = as.matrix(london.grid),
                           group = 1_month,)