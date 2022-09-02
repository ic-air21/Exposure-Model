library(easypackages)
easypackages::packages("sf",
                       "raster",
                       "stars",
                       "r5r",
                       "geobr",
                       "aopdata",
                       "gtfs2gps",
                       "ggplot2",
                       "osmdata",
                       "h3jsr",
                       "viridisLite",
                       "ggnewscale",
                       "dplyr",
                       "magrittr",
                       "rgdal",
                       prompt = FALSE
)

rotate_data <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x){ 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}
bbox <- bbox(London)

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
Londonmap <- st_as_sf(London, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

ObsAll0518 <- readRDS("Data/ObsAll0518.rds")
obs <- st_as_sf(x = ObsAll0518, coords = c("Easting","Northing"), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

box <- extent(Londonmap)

temp.r <- readRDS("Data/temp.rds")
climate <- as(temp.r, 'SpatialPolygonsDataFrame')
climate <- st_as_sf(climate, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
climate <- st_crop(climate, box)
plot(climate)

PCM.r <- readRDS("Data/pcm.rds")
model <- as(PCM.r, 'SpatialPolygonsDataFrame')
model <- st_as_sf(model, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
model <- st_crop(model, box)

AOD0518.r <- readRDS("Data/satellite.rds")
satellite <- as(AOD0518.r ,'SpatialPolygonsDataFrame')
satellite <- st_as_sf(satellite, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
satellite <- st_crop(satellite, box)

predL.df <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD OK DF.rds")
predL0518 <- subset(predL.df, time == 162)
coordinates(predL0518) <- ~ x + y
gridded(predL0518) <- TRUE
int.aod <- as(predL0518, 'SpatialPolygonsDataFrame')
int.aod <- st_as_sf(x = int.aod, coords = c("x", "y"))
satellite <- int.aod

pop.r <- raster("Data/Population/gbr_ppp_2018_1km_Aggregated.tif")
pop.r <- projectRaster(pop.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
population <- as(pop.r ,'SpatialPolygonsDataFrame')
population <- st_as_sf(population, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
population <- st_crop(population, box)
plot(population, border = "transparent")

london.tilted <- Londonmap %>% rotate_data()
obs.tilted <- obs %>% rotate_data(y_add = 150000)
climate.tilted <- climate %>% rotate_data(y_add = 60000)
model.tilted <- model %>% rotate_data(y_add = 90000)
satellite.tilted <- satellite %>% rotate_data(y_add = 120000)
population.tilted <- population %>% rotate_data(y_add = 30000)


color = "#0E207E"
color2 = rgb(0, 0, 255, max = 255, alpha = 200)

obs.tilted$PM252 <- scale(obs.tilted$PM25)
model.tilted$pm252018g2 <- scale(model.tilted$pm252018g)
satellite.tilted$layer2 <- scale(satellite.tilted$layer)


layerplot <- ggplot() + 
  geom_sf(data = london.tilted , colour = "black", fill = NA) +
  annotate("text", label='London Map', x=1380000, y= -20000, hjust = 0, color=color, size = 6) +
  scale_color_viridis_c(option = "A") +
  
  geom_sf(data = population.tilted, aes(fill = gbr_ppp_2018_1km_Aggregated)) +
  annotate("text", label='Population', x=1380000, y= 10000, hjust = 0, color=color, size = 6) +
  scale_fill_gradient2(low = "yellow", high = "red", midpoint = 133) +
  
  new_scale("fill") +
  geom_sf(data = climate.tilted, aes(fill = layer)) +
  annotate("text", label='Climate Variables', x=1380000, y= 40000, hjust = 0, color=color, size = 6) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 13.8) +
  
  new_scale("fill") +
  scale_fill_viridis_c(option = "A") +
  
  geom_sf(data = obs.tilted, aes(color = PM252), size = 2) +
  annotate("text", label='Ground Observations', x=1380000, y= 130000, hjust = 0, color=color, size = 6) +
  
  geom_sf(data = model.tilted, aes(fill = pm252018g2)) +
  annotate("text", label='Underlying Model', x=1380000, y= 70000, hjust = 0, color=color, size = 6) +
  
  geom_sf(data = satellite.tilted, aes(fill = layer2)) +
  annotate("text", label='Satellite AOD', x=1380000, y= 100000, hjust = 0, color=color, size = 6) +
  
  xlim(c(1200000,1500000)) +
  theme_void() +
  theme(legend.position="none") 

layerplot

satellite.tilted$AOD100 <- satellite.tilted$AOD*40
layerplot2 <- ggplot() + 
  geom_sf(data = london.tilted , colour = "black", fill = NA) +
  annotate("text", label='London Map', x=1380000, y= -20000, hjust = 0, color=color, size = 4) +
  scale_color_viridis_c(option = "A") +
  
  new_scale("color") +
  geom_sf(data = population.tilted, aes(fill = gbr_ppp_2018_1km_Aggregated, color = gbr_ppp_2018_1km_Aggregated)) +
  annotate("text", label='Population', x=1380000, y= 10000, hjust = 0, color=color, size = 4) +
  scale_fill_gradient2(low = "orange", high = "red", midpoint = 133) +
  scale_color_gradient2(low = "orange", high = "red", midpoint = 133) +
  
  new_scale("fill") + 
  new_scale("color") +
  geom_sf(data = climate.tilted, aes(fill = layer, color = layer)) +
  annotate("text", label='Climate Variables', x=1380000, y= 40000, hjust = 0, color=color, size = 4) +
  scale_fill_gradient2(low = "green", mid = "orange", high = "red", midpoint = 13.8) +
  scale_color_gradient2(low = "green", mid = "orange", high = "red", midpoint = 13.8) +
  
  new_scale("fill") +
  new_scale("color") +
  scale_fill_viridis_c(option = "A") +
  scale_color_viridis_c(option = "A") +
  
  geom_sf(data = obs.tilted, aes(color = PM252), size = 1) +
  annotate("text", label='Ground Observations', x=1380000, y= 130000, hjust = 0, color=color, size = 4) +
  
  geom_sf(data = model.tilted, aes(fill = pm252018g2, color = pm252018g2)) +
  annotate("text", label='Underlying Model', x=1380000, y= 70000, hjust = 0, color=color, size = 4) +
  
  geom_sf(data = satellite.tilted, aes(fill = AOD100, color = AOD100)) +
  annotate("text", label='Satellite AOD', x=1380000, y= 100000, hjust = 0, color=color, size = 4) +
  
  xlim(c(1200000,1500000)) +
  theme_void() +
  theme(legend.position="none") 

layerplot2
ggsave(plot = layerplot2, filename = 'map_layers2.png')


fullgrid <- model

partials <- round(c(runif(500, min = 0, max = 2714)))
partialgrid <- fullgrid[partials,]
plot(partialgrid)

library(truncnorm)
x.points <- c(rtruncnorm(60, a=503568.2, b=561957.5, mean = 520000, sd = 16000))
y.points <- c(rtruncnorm(60, a=155850.8, b=200933.9, mean = 178000, sd = 10000))

value.points <- c(rpois(3600, 15))

points.matrix <- cbind(value.points, x.points, y.points)

points.df <- as.data.frame(points.matrix)
obs <- st_as_sf(x = points.df, coords = c("x.points", "y.points"))

full.tilted <- fullgrid %>% rotate_data()
partial.tilted <- partialgrid %>% rotate_data(y_add = 50000)
obs.tilted <- obs %>% rotate_data(y_add = 100000)


library(ggthemes)

levels <- ggplot() + 
  scale_color_viridis_c(option = "B") +
  scale_fill_viridis_c(option = "B") +
  
  geom_sf(data = full.tilted, aes(fill = pm252018g, color = pm252018g)) +
  
  geom_sf(data = partial.tilted, aes(fill = pm252018g, color = pm252018g)) +
  
  geom_sf(data = obs.tilted, aes(color = value.points), size = 1.5) +
  
  theme_void() +
  theme(legend.position="none")

ggsave(plot = levels, filename = '3_levels.png')
levels

level3 <- ggplot() + 
  scale_color_viridis_c(option = "A") +
  scale_fill_viridis_c(option = "A") +
  
  geom_sf(data = full.tilted, aes(fill = pm252018g)) +
  
  theme_void() +
  theme(legend.position="none")

level2 <- ggplot() + 
  scale_color_viridis_c(option = "A") +
  scale_fill_viridis_c(option = "A") +
  
  geom_sf(data = partial.tilted, aes(fill = pm252018g)) +
  
  theme_void() +
  theme(legend.position="none")

level1 <- ggplot() + 
  scale_color_viridis_c(option = "A") +
  scale_fill_viridis_c(option = "A") +

  geom_sf(data = obs.tilted, aes(color = value.points), size = 1.5) +
  
  theme_void() +
  theme(legend.position="none")

ggsave(plot = level1, filename = 'level1.png')
ggsave(plot = level2, filename = 'level2.png')
ggsave(plot = level3, filename = 'level3.png')

library(raster)
LAOD <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD.rds")
LAOD0518 <- subset(LAOD, year == 2018 & month == 6)
coordinates(LAOD0518) <- ~ x + y
gridded(LAOD0518) <- TRUE
raw.aod <- as(LAOD0518 ,'SpatialPolygonsDataFrame')
raw.aod <- st_as_sf(x = raw.aod, coords = c("x", "y"))

predL.df <- readRDS("Data/Air Pollution/MAIAC AOD/LAOD OK DF.rds")
predL0518 <- subset(predL.df, time == 162)
coordinates(predL0518) <- ~ x + y
gridded(predL0518) <- TRUE
int.aod <- as(predL0518, 'SpatialPolygonsDataFrame')
int.aod <- st_as_sf(x = int.aod, coords = c("x", "y"))

raw.tilted <- raw.aod %>% rotate_data(y_add = 50000)
int.tilted <- int.aod %>% rotate_data()

library(viridis)
interpolation <- ggplot() + 
  scale_fill_viridis_c(option = "B") +
  scale_color_viridis_c(option = "B") +
  
  geom_sf(data = raw.tilted, aes(fill = AOD, color = AOD)) +
  
  geom_sf(data = int.tilted, aes(fill = AOD, color = AOD)) +
  
  theme_void() +
  theme(legend.position="none")

interpolation
ggsave(plot = interpolation, filename = 'LondonAODInterpolation.png')

#trying different stack
rotate_data2 <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function(){ matrix(c(2, 0.4, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x){ 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geometry = .$geometry * shear_matrix()* rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

London <- readOGR("Data/UK Shapefile/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward.shp", stringsAsFactors = FALSE)
Londonmap <- st_as_sf(London, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

ObsAll0518 <- readRDS("Data/ObsAll0518.rds")
obs <- st_as_sf(x = ObsAll0518, coords = c("Easting","Northing"), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

box <- extent(Londonmap)

temp.r <- readRDS("Data/temp.rds")
climate <- as(temp.r, 'SpatialPolygonsDataFrame')
climate <- st_as_sf(climate, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
climate <- st_crop(climate, box)

PCM.r <- readRDS("Data/pcm.rds")
model <- as(PCM.r, 'SpatialPolygonsDataFrame')
model <- st_as_sf(model, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
model <- st_crop(model, box)

AOD0518.r <- readRDS("Data/satellite.rds")
satellite <- as(AOD0518.r ,'SpatialPolygonsDataFrame')
satellite <- st_as_sf(satellite, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
satellite <- st_crop(satellite, box)

pop.r <- raster("Data/Population/gbr_ppp_2018_1km_Aggregated.tif")
pop.r <- projectRaster(pop.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
population <- as(pop.r ,'SpatialPolygonsDataFrame')
population <- st_as_sf(population, coords = c(x, y), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
population <- st_crop(population, box)
plot(population, border = "transparent")


obs.tilted <- obs %>% rotate_data2(x_add = 150000)
climate.tilted <- climate %>% rotate_data2(x_add = 60000)
model.tilted <- model %>% rotate_data(y_add = 90000)
satellite.tilted <- satellite %>% rotate_data(x_add = 120000)
population.tilted <- population %>% rotate_data(x_add = 30000)


color = "#0E207E"
color2 = rgb(0, 0, 255, max = 255, alpha = 200)

layerplot <- ggplot() + 
  scale_color_viridis_c(option = "A") +
  
  geom_sf(data = population.tilted, aes(fill = gbr_ppp_2018_1km_Aggregated)) +
  scale_fill_gradient2(low = "yellow", high = "red", midpoint = 133) +
  
  new_scale("fill") +
  geom_sf(data = climate.tilted, aes(fill = layer)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 13.8) +
  
  new_scale("fill") +
  scale_fill_viridis_c(option = "A") +
  
  geom_sf(data = obs.tilted, aes(color = PM252), size = 2) +

  geom_sf(data = model.tilted, aes(fill = pm252018g2)) +

  geom_sf(data = satellite.tilted, aes(fill = layer2)) +
  xlim(c(1200000,1500000)) +
  theme_void() +
  theme(legend.position="none") 


model.tilted <- model %>% rotate_data2()

ggplot() + 
  geom_sf(data = model.tilted, aes(fill = pm252018g, color = pm252018g)) +
  scale_color_viridis_c(option = "B") +
  scale_fill_viridis_c(option = "B") +
  theme_void() +
  theme(legend.position="none")
