library(readxl)
library(ggplot2)
library(reshape2)
library(scico)
library(dplyr)
library(tidyr)


NO2 <- read_excel("Data/Air Pollution/Monthly Means/Monthly Mean NO2.xlsx")
PM25 <- read_excel("Data/Air Pollution/Monthly Means/Monthly Mean PM2.5.xlsx")
O3 <- read_excel("Data/Air Pollution/Monthly Means/Monthly Mean O3.xlsx")
SO2 <- read_excel("Data/Air Pollution/Monthly Means/Monthly Mean SO2.xlsx")

AURNSites <- read_excel("Data/Air Pollution/Monthly Means/AURN Sites.xlsx")

NO2 <- NO2 %>%
  pivot_longer(!`Site Name`:Year, names_to = "Month", values_to = "Monthly Mean")
PM25 <- PM25 %>%
  pivot_longer(!`Site Name`:Year, names_to = "Month", values_to = "Monthly Mean")
O3 <- O3 %>%
  pivot_longer(!`Site Name`:Year, names_to = "Month", values_to = "Monthly Mean")
SO2 <- SO2 %>%
  pivot_longer(!`Site Name`:Year, names_to = "Month", values_to = "Monthly Mean")

NO2.PM25 <- merge(NO2, PM25, by =  c("Year","Month", "Site Name"), all.x = TRUE, all.y = TRUE)
SO2.O3 <- merge(SO2, O3, by = c("Year", "Month", "Site Name"), all.x = TRUE, all.y = TRUE)
All <- merge(NO2.PM25, SO2.O3, by = c("Year", "Month", "Site Name"), all.x = TRUE, all.y = TRUE)

colnames(All)[4] <- "NO2"
colnames(All)[5] <- "PM25"
colnames(All)[6] <- "SO2"
colnames(All)[7] <- "O3"

All$NO2 <- as.numeric(All$NO2)
All$PM25 <- as.numeric(All$PM25)
All$O3 <- as.numeric(All$O3)
All$SO2 <- as.numeric(All$SO2)

All <- merge(All, AURNSites, by = "Site Name")

UK <- readOGR("Data/UK Shapefile/GBR_adm_shp/GBR_adm0.shp")
UK <- spTransform(UK, crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

library(ggmap)
library(sf)
library(rgdal)
library(xts)

All$Month[All$Month == "Jan"] <- "01"
All$Month[All$Month == "Feb"] <- "02"
All$Month[All$Month == "Mar"] <- "03"
All$Month[All$Month == "Apr"] <- "04"
All$Month[All$Month == "May"] <- "05"
All$Month[All$Month == "Jun"] <- "06"
All$Month[All$Month == "Jul"] <- "07"
All$Month[All$Month == "Aug"] <- "08"
All$Month[All$Month == "Sep"] <- "09"
All$Month[All$Month == "Oct"] <- "10"
All$Month[All$Month == "Nov"] <- "11"
All$Month[All$Month == "Dec"] <- "12"
All$dates <- paste(as.character(All$Year), All$Month, sep = "-")
All$date <- All$date %>% as.yearmon()

library(bmstdr)
library(spBayes)
library(rstan)
library(INLA)
library(inlabru)
library(zoo)
library(sp)
library(spacetime)

unique(All$`Environment Type`)

All$industrial <- ifelse(grepl("Industrial", All$`Environment Type`, fixed = TRUE), 1, 0)
All$traffic <- ifelse(grepl("Traffic", All$`Environment Type`, fixed = TRUE), 1, 0)

All <- All[order(All$date),]
All <- All[order(All$`Site Name`),]

saveRDS(All, file = "Data/All.rds")

time <- All$date
time_part <- unique(time)
time_part <- sort(time_part)

locs <- unique(All[c("Site Name", "Easting", "Northing")]) 

spat_part <- SpatialPoints(coords = locs[,c("Easting", "Northing")])

UKdf <- data.frame(NO2 = All$NO2, PM25 = All$PM25, O3 = All$O3, SO2 = All$SO2, traffic = All$traffic, industrial = All$industrial, id =  All$`Site Name`, Year =  All$Year, month =  All$Month, day=rep(1, 20724))
UKCompdf <- complete(UKdf, id, Year, month)
all(unique(UKCompdf$id)==unique(UK$`Site Name`))

#check
length(spat_part)*length(time_part)

UKCompdf[is.na(UKCompdf)] = mean(UKCompdf$NO2)
UKst <- STFDF(sp=spat_part, time=time_part, data=UKCompdf)


UKst$Year <- as.integer(UKst$Year)
UKst$NO2 <- as.integer(UKst$NO2)
UKst$traffic <- as.integer(UKst$traffic)
UKst$industrial <- as.integer(UKst$industrial)
UKst$day <- as.integer(UKst$day)



saveRDS(UKst, file = "Data/UKAllst.rds")

