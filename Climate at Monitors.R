library(readxl)
library(ggplot2)
library(reshape2)
library(scico)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)
library(raster)
library(rqdatatable)

UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

Pollution <- readRDS("Data/All.rds")
dates <- unique(Pollution$dates)
Pollution$Month <- as.numeric(Pollution$Month)
Pollution$time <- (Pollution$Year - 2005)*12 + Pollution$Month

LondonPoll <- subset(Pollution, Zone == "Greater London Urban Area" & time <= 60)
ggplot(LondonPoll, aes(x = time, y = NO2)) +
  geom_line(aes(color = `Site Name`)) +
  geom_vline(xintercept = 12)

sites <- unique(Pollution[c("Easting", "Northing")])
n.sites <- as.numeric(nrow(sites))

temp.points <- c()
humid.points <- c()
precip.points <- c()
temp <- list()
humid <- list()
precip <- list()



temp.r <- stack("Data/Climate/temp1.tif")
humid.r <- stack("Data/Climate/humid1.tif")
precip.r <- stack("Data/Climate/precip1.tif")

n <- nrow(Pollution)
temp.points <- c(rep(NA, n))
humid.points <- c(rep(NA, n))
precip.points <- c(rep(NA, n))
climate <- cbind(Pollution, temp.points, humid.points, precip.points)

for (i in 1:96){  
  temp.month <- temp.r[[i]]
  humid.month <- humid.r[[i]]
  precip.month <- precip.r[[i]]
    
  temp.points <- raster::extract(temp.month, sites)
  humid.points <- raster::extract(humid.month, sites)
  precip.points <- raster::extract(precip.month, sites)
  
  time <- rep(i, n.sites)
  met.points <- cbind(sites, time, temp.points, humid.points, precip.points)
  
  climate <- natural_join(climate, met.points, by = c("time", "Easting", "Northing"), jointype = "FULL")
}

temp.r <- stack("Data/Climate/temp2.tif")
humid.r <- stack("Data/Climate/humid2.tif")
precip.r <- stack("Data/Climate/precip2.tif")

for (i in 1:96){  
  temp.month <- temp.r[[i]]
  humid.month <- humid.r[[i]]
  precip.month <- precip.r[[i]]
  
  temp.points <- raster::extract(temp.month, sites)
  humid.points <- raster::extract(humid.month, sites)
  precip.points <- raster::extract(precip.month, sites)
  time <- rep(i+96, n.sites)
  met.points <- cbind(sites, time, temp.points, humid.points, precip.points)
  
  climate <- natural_join(climate, met.points, by = c("time", "Easting", "Northing"), jointype = "FULL")

}
saveRDS(climate, file = "Data/MonitorVars")

climate <- readRDS(file = "Data/MonitorVars")

AURNSites <- read_excel("Data/Air Pollution/Monthly Means/AURN Sites.xlsx")
AURNSites$industrial <- ifelse(grepl("Industrial", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$traffic <- ifelse(grepl("Traffic", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$background <- ifelse(grepl("Background", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$urban <- ifelse(grepl("Urban", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$suburban <- ifelse(grepl("Suburban", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)
AURNSites$rural <- ifelse(grepl("Rural", AURNSites$`Environment Type`, fixed = TRUE), 1, 0)

pop.r <- raster("Data/Population/gbr_ppp_2018_1km_Aggregated.tif")
pop.r <- projectRaster(pop.r, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
pop.points <- merge(raster::extract(pop.r, sites), sites)

AURNSites <- merge(AURNSites, pop.points, by = c("Easting", "Northing"))
names(AURNSites)[23] <- "Population"

climate <- climate[,c("Easting", "Northing","time", "PM25", "NO2", "O3", "SO2", "temp.points", "humid.points", "precip.points")]

climate <- merge(climate, AURNSites, by = c("Easting", "Northing"), all.x = TRUE)
climate <- climate[order(climate$time),]
names(climate)[c(8:10)] <- c("temp", "humid", "precip")

saveRDS(climate, "Data/climatefinal.rds")

climate <- readRDS(file = "Data/climatefinal.rds")

climatesmall <- climate[4:10]
saveRDS(climatesmall, "Data/climatesmall.rds")

met <- climate[,c(8,9,10)]
pol <- climate[,c(4,5,6,7)]
cor(climate[,c(4:10)], use="pairwise.complete.obs")
cor(met, pol, use="complete.obs")
pairs(climatesmall)

hist(climate$temp)

marylebone <- subset(climate, `Site Name` == "London Marylebone Road")
plot(marylebone$time, marylebone$temp, type = "l", ylim = c(0,40))
lines(marylebone$time, marylebone$PM25, col = "red")
cor(marylebone$temp, marylebone$PM25, use="complete.obs")

london <- subset(climate, Zone == "Greater London Urban Area")
plot(london$time, london$temp, type = "l", ylim = c(0,100))
lines(london$time, london$NO2, col = "red")
cor(london$temp, london$PM25, use="complete.obs")

backgroundclimate <- subset(climate, background == 1)
cor(backgroundclimate[,4:10], use="complete.obs")
plot(backgroundclimate$time, backgroundclimate$temp,type = "l", ylim = c(0,40))
lines(backgroundclimate$time, backgroundclimate$PM25, col = "red")

backgroundclimate <- subset(climate, background == 1)
cor(backgroundclimate[,4:10], use="complete.obs")
plot(backgroundclimate$time, backgroundclimate$temp,type = "l", ylim = c(0,40))
lines(backgroundclimate$time, backgroundclimate$NO2, col = "red")

urban <- subset(climate, urban == 1)
cor(urban[,4:10], use = "complete.obs")


ggplot() + 
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #geom_point(data = sites, aes(x = Easting, y = Northing), color = "red") +
  geom_point(data = climatesubset, aes(x = Easting, y = Northing, color = NO2), alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 13.8) +
  facet_wrap(~time) +
  coord_equal()



library(corrplot)
library(viridis)
col4 <- colorRampPalette(c("#0E207E", "lightskyblue", "orange3", "red3"))(20)
corrplot(cor(climate[,c(4,8,9,10)], use="pairwise.complete.obs"),
         method = "number", type = "upper", col = inferno(40), number.cex = 2, tl.cex = 2, cl.pos = "n", tl.col = "#0E207E")

corrplot(cor(climate[,c(4:7)], use="pairwise.complete.obs"),
         method = "number", type = "upper" )

hist(climate$humid.points)
hist(climate$NO2)
hist(climate$O3)
hist(climate$PM25)
hist(climate$precip.points)
hist(climate$temp.points)
hist(climate$SO2)

climate$cubhumid <- (climate$humid)^3
hist(climate$cubhumid)

climate$logNO2 <- log(climate$NO2)
hist(climate$logNO2)

#climate$logO3 <- log(climate$O3)
#hist(climate$logO3)

climate$logPM25 <- log(climate$PM25)
hist(climate$logPM25)

climate$logprecip <- log(climate$precip)
hist(climate$logprecip)

pairs(climate[,c("logNO2", "logPM25", "O3", "SO2", "cubhumid", "logprecip", "temp")])
corrplot(cor(climate[,c("logPM25","logNO2", "SO2", "O3", "temp", "logprecip","cubhumid")], use="complete.obs"),
         method = "number", type = "upper", col = inferno(40), number.cex = 1.5, tl.cex = 1.5, cl.pos = "n", tl.col = "#0E207E" )

corrplot(cor(climate[,c("logPM25", "cubhumid", "logprecip", "temp.points")], use="complete.obs"),
         method = "number", type = "upper" )


climate.partial <- climate[,c("time", "Site Name", "logNO2", "logPM25", "cubhumid", "logprecip", "temp.points")]
climate.full <- na.omit(climate.partial)

climate.subset <- subset(climate.full, climate.full$time >= 168 & climate.full$time <= 174)

z <- climate.subset[,-c(1,2)]
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)
distance <- dist(nor)


mydata.hclust = hclust(distance)
plot(mydata.hclust, hang = -1)
plot(mydata.hclust,labels=climate.full$`Site Name`,main='Default from hclust')
plot(mydata.hclust,hang=-1, labels=mydata$Company,main='Default from hclust')

library(useful)
km <- kmeans(nor, centers = 5)
plot(km, data=nor, labels = climate.full$`Site Name`)

#time series

ggplot(climate, aes(x = time, y = PM25)) +
  geom_line()

climatemeans <- aggregate(cbind(PM25, NO2, SO2, O3, temp, humid, precip) ~ time, data = climate, FUN = mean, na.rm = TRUE)

ggplot(climatemeans) +
  geom_line(aes(x = time, y = PM25), color = "black") +
  geom_line(aes(x = time, y = NO2), color = "red") +
  geom_line(aes(x = time, y = SO2), color = "green") +
  geom_line(aes(x = time, y = O3), color = "blue") +
  geom_line(aes(x = time, y = temp), color = "yellow") +
  geom_line(aes(x = time, y = humid), color = "purple") +
  geom_line(aes(x = time, y = precip), color = "orange") 

library(viridis)
AllData <- readRDS("Data/Even More Site Data.rds")
met <- AllData[,c(5,6,7)]
pol <- AllData[,c(8,9,10,11)]
sat <- AllData[,c(12,16,38)]
pcm <- AllData[,c(14,44)]
corrplot(cor(AllData[,c(5,6,7,8,9,10,11,14,12,16,38)], use="pairwise.complete.obs"),
         method = "number", type = "upper", col = inferno(40), number.cex = 1, tl.cex = 1, cl.pos = "n", tl.col = "#0E207E")

corrplot(cor(AllData[,c(8,10,5,6,7)], use="pairwise.complete.obs"),
         method = "number", type = "upper", col = inferno(40), number.cex = 2, tl.cex = 2, cl.pos = "n", tl.col = "#0E207E")


cor(pol, pcm, use="complete.obs")

