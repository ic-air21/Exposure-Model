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
library(mgcv)

setwd("C:/Users/air21/OneDrive - Imperial College London/Documents/PhD/R/Objective 1")
UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

sitedata <- readRDS("Data/All Site Data.rds")

unique(is.na(sitedata[,c("PM25","AOD55")]))

cor(sitedata[,c("AOD55", "PM25")], use="pairwise.complete.obs")
pairs(sitedata[,c("AOD47", "AOD55", "PM25")])

cor(sitedata[,c("AOD47", "AOD55")], use="pairwise.complete.obs")


plot(sitedata$AOD55, sitedata$PM25)
abline(glm(sitedata$PM25 ~ sitedata$AOD55), col = "red")

fit <- glm(sitedata$PM25 ~ sitedata$AOD55)
summary(fit)

sitedataMarylebone <- subset(sitedata, `Site Name` == "London Marylebone Road")
plot(sitedataMarylebone$AOD55, sitedataMarylebone$PM25)
abline(glm(sitedataMarylebone$PM25 ~ sitedataMarylebone$AOD55), col = "red")

ggplot(sitedata) +
  geom_point(aes(PM25, AOD55)) +
  facet_wrap(vars(background, traffic, industrial, urban, rural)) +
  geom_smooth(aes(PM25, AOD55), color = "red")

ruralsitedata <- subset(sitedata, rural == 1)
cor(trafficsitedata[,c("AOD55", "PM25")], use="pairwise.complete.obs")

sitedataLondon <- subset(sitedata, Zone == "Greater London Urban Area")

ggplot(sitedataMarylebone) +
  geom_point(aes(x = time, y = scaled.pm25)) +
  geom_smooth(aes(x = time, y = scaled.pm25), color = "blue")# +
#geom_point(aes(x = time, y = scaled.aod), color = "red") +
#geom_smooth(aes(x = time, y = scaled.aod), color = "green")


plot(sitedata$time, sitedata$scaled.aod)
lines(predict(lm(sitedata$scaled.aod ~ sitedata$time)),col='green')
points(sitedata$time, sitedata$scaled.pm25, col = "red")

ggplot() +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = sitedata, aes(x = Easting, y = Northing, color = Zone)) +
  xlim(c(-600000,700000))





sitedata$adjusted.pm25 <- sitedata$PM25*(1- sitedata$humid/100)
sitedata$adjusted.aod47 <- sitedata$AOD47/sitedata$blh
sitedata$adjusted.aod55 <- sitedata$AOD55/sitedata$blh

cor(sitedata[,c("adjusted.aod47", "adjusted.pm25")], use="pairwise.complete.obs")
cor(sitedata[,c("adjusted.aod55", "adjusted.pm25")], use="pairwise.complete.obs")


fit <- lm(adjusted.pm25 ~ adjusted.aod47, data = sitedata)
summary(fit)

ggplot(sitedata) +
  geom_point(aes(adjusted.pm25, adjusted.aod)) +
  facet_wrap(vars(background, traffic, industrial, urban, rural)) +
  geom_smooth(aes(adjusted.pm25, adjusted.aod), color = "red")

ggplot(sitedata) +
  geom_point(aes(time, adjusted.aod47, color = Zone)) +
  facet_wrap(vars(Zone)) #+
  #geom_smooth(aes(adjusted.pm25, adjusted.aod), color = "red")

backgroundsites <- subset(sitedata, background == 1)
trafficsites <- subset(sitedata, traffic == 1)
industrialsites <- subset(sitedata, industrial == 1)

urbansites <- subset(sitedata, urban == 1)
ruralsites <- subset(sitedata, rural == 1)

cor(backgroundsites[,c("adjusted.pm25", "adjusted.aod47")], use="pairwise.complete.obs")
cor(trafficsites[,c("adjusted.pm25", "adjusted.aod47")], use="pairwise.complete.obs")
cor(industrialsites[,c("adjusted.pm25", "adjusted.aod47")], use="pairwise.complete.obs")

cor(urbansites[,c("adjusted.pm25", "adjusted.aod47")], use="pairwise.complete.obs")
cor(ruralsites[,c("adjusted.pm25", "adjusted.aod47")], use="pairwise.complete.obs")


cor(backgroundsites[,c("adjusted.pm25", "adjusted.aod55")], use="pairwise.complete.obs")
cor(trafficsites[,c("adjusted.pm25", "adjusted.aod55")], use="pairwise.complete.obs")
cor(industrialsites[,c("adjusted.pm25", "adjusted.aod55")], use="pairwise.complete.obs")

cor(urbansites[,c("adjusted.pm25", "adjusted.aod55")], use="pairwise.complete.obs")
cor(ruralsites[,c("adjusted.pm25", "adjusted.aod55")], use="pairwise.complete.obs")

fit <- lm(adjusted.pm25 ~ adjusted.aod55, data = backgroundsites)
summary(fit)

unique(sitedata$`Site Name`)
