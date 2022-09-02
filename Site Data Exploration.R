library(ggplot2)
library(GGally)
library(ggpubr)
setwd("~/PhD/R/Objective 1")
sitedata <- readRDS("Data/Even More Site Data.rds")

g1 <- ggplot(sitedata, aes(x = time, y = PM25, color = `Site Name`)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ `Environment Type`)
g2 <- ggplot(sitedata, aes(x = time, y = `Daily Monthly Mean`, color = `Site Name`)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ `Environment Type`)
g3 <- ggplot(sitedata, aes(x = time, y = `Hourly Monthly Mean`, color = `Site Name`)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ `Environment Type`)

ggarrange(g1,g2,g3)

sitedata$year <- ceiling(sitedata$time/12) + 2004
unique(sitedata$year)
sitedata$month <- sitedata$time - (sitedata$year-2005)*12
unique(sitedata$month)

unique(sitedata$`Environment Type`)

rurback <- subset(sitedata, `Environment Type` == "Rural Background")
urbback <- subset(sitedata, `Environment Type` == "Urban Background")  
urbind <- subset(sitedata, `Environment Type` == "Urban Industrial")
urbtraff <- subset(sitedata, `Environment Type` == "Urban Traffic")
subback <- subset(sitedata, `Environment Type` == "Suburban Background") 
subind <- subset(sitedata, `Environment Type` == "Suburban Industrial") 

sitedataPM25 <- sitedata[(!is.na(sitedata$PM25)) > 0, ]
unique(sitedataPM25$`Site Name`)

sitedataPM25Vol <- sitedata[(!is.na(sitedata$`Vol Monthly Mean`)) > 0, ]
unique(sitedataPM25Vol$`Site Name`)

sitedataNO2 <- sitedata[(!is.na(sitedata$NO2)) > 0, ]
unique(sitedataNO2$`Site Name`)

ggplot(sitedataPM25, aes(x = time, y = PM25, color = `Site Name`)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ `Environment Type`)

ggplot(sitedataPM25, aes(x = `Environment Type`, y = PM25)) +
  ylab("Ground Monitored PM2.5") +
  geom_boxplot() 

ggplot(sitedataNO2, aes(x = time, y = NO2, color = `Site Name`)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ `Environment Type`)

ggplot(sitedataNO2, aes(x = `Environment Type`, y = NO2)) +
  ylab("Ground Monitored NO2") +
  geom_boxplot() 

library(ggfortify)
library(zoo)
library(tseries)
library(astsa)
library(forecast)

sites <- unique(sitedata[c("Site Name", "Easting", "Northing")])
n.sites <- as.numeric(nrow(sites))

pm25.data <- list()
pm25.ts <- list()
no2.data <- list()
no2.ts <- list()
for (i in 1:n.sites){
  pm25.data[[i]] <- subset(sitedata, site == i)$PM25
  no2.data[[i]] <- subset(sitedata, site == i)$NO2
  pm25.ts[[i]] <- ts(pm25.data[[i]], start=c(2005, 1), end=c(2020, 12), frequency=12)
  no2.ts[[i]] <- ts(no2.data[[i]], start=c(2005, 1), end=c(2020, 12), frequency=12)
}

a <- c(6,30,34,57,93,101,128,129,133,149,150,165,224)
pm25.data2 <- cbind(pm25.data[[6]], pm25.data[[30]], pm25.data[[34]], pm25.data[[57]],
                    pm25.data[[93]], pm25.data[[101]], pm25.data[[128]], pm25.data[[129]], 
                    pm25.data[[133]], pm25.data[[149]], pm25.data[[150]], pm25.data[[165]], pm25.data[[224]])

pm25.ts <- ts(pm25.data2, start=c(2005, 1), end=c(2020, 12), frequency=12)
saveRDS(pm25.ts, "Data/PM25 Sites ts.rds")


no2.data2 <- no2.data[[1]]
for (i in 2:227){
  no2.data2 <- cbind(no2.data2, no2.data[[i]])
}

no2.ts <- ts(no2.data2, start=c(2005, 1), end=c(2020, 12), frequency=12)
saveRDS(no2.ts, "Data/NO2 Sites ts.rds")

all.names <- sites[,1]
colnames(no2.ts) <- all.names

a <- c(6,30,34,57,93,101,128,129,133,149,150,165,224)
a.names <- sites[a,1]

colnames(pm25.ts) <- a.names
plot(pm25.ts[,c(1:3,5:7)], main = "Site-specfic Time Series")
plot(pm25.ts[,8:13], main = "Site-specfic Time Series")

full.pm25.ts <- pm25.ts[,-a]

plot(no2.ts[,c(1:5,7:8,10:12)], main = "Site-specfic Time Series")
plot(no2.ts[,c(13,15:23)], main = "Site-specfic Time Series")
plot(no2.ts[,c(24:28,30:32,34:35)], main = "Site-specfic Time Series")
plot(no2.ts[,c(36:45)], main = "Site-specfic Time Series")
plot(no2.ts[,c(46:55)], main = "Site-specfic Time Series")
plot(no2.ts[,c(56:65)], main = "Site-specfic Time Series")
plot(no2.ts[,c(66:75)], main = "Site-specfic Time Series")
plot(no2.ts[,c(76:85)], main = "Site-specfic Time Series")
plot(no2.ts[,c(86:87, 89:96)], main = "Site-specfic Time Series")
plot(no2.ts[,c(97:106)], main = "Site-specfic Time Series")
plot(no2.ts[,c(107:110,112:117)], main = "Site-specfic Time Series")
plot(no2.ts[,c(118:127)], main = "Site-specfic Time Series")
plot(no2.ts[,c(128:133, 135, 138:140)], main = "Site-specfic Time Series")
plot(no2.ts[,c(141,150)], main = "Site-specfic Time Series")
plot(no2.ts[,c(151:160)], main = "Site-specfic Time Series")
plot(no2.ts[,c(161:170)], main = "Site-specfic Time Series")
plot(no2.ts[,c(171:180)], main = "Site-specfic Time Series")
plot(no2.ts[,c(181,182,184:191)], main = "Site-specfic Time Series")
plot(no2.ts[,c(192, 194:200, 203:204)], main = "Site-specfic Time Series")
plot(no2.ts[,c(205:214)], main = "Site-specfic Time Series")
plot(no2.ts[,c(215:216, 218:225)], main = "Site-specfic Time Series")
plot(no2.ts[,c(226:227)], main = "Site-specfic Time Series")

plot.ts(no2.ts[,c(65,210,76,145,12,175)], main = "Site−specfic Time Series")
plot.ts(no2.ts[,c(112,73,63,73,132,46)], main = "Site−specfic Time Series")


b <- c(6,9,14,29,33,88,111,134,136, 137, 183, 193, 201, 202, 217)

full.no2.ts <- no2.ts[,-b]


site1.ts <- sites.ts[,1]

library(zoo)
approx.site3 <- na.approx(site3.ts, rule=2)
plot(approx.site3)

library(forecast)
approx2.site3 <- na.interp(
  site3.ts,
  lambda = NULL,
  linear = (frequency(site3.ts) <= 1 | sum(!is.na(site3.ts)) <= 2 * frequency(site3.ts))
)
par(mfrow=c(1,1))
plot(approx2.site3)
abline(v = 2006)
abline(v = 2007)
abline(v = 2008)
abline(v = 2009)
abline(v = 2010)

#for all sites
inter.sites <- na.interp(
  site1.ts,
  lambda = NULL,
  linear = (frequency(site1.ts) <= 1 | sum(!is.na(site1.ts)) <= 2 * frequency(site1.ts))
)
for (i in 2:13) {
  inter.site <- na.interp(
    sites.ts[,i],
    lambda = NULL,
    linear = (frequency(sites.ts[,i]) <= 1 | sum(!is.na(sites.ts[,i])) <= 2 * frequency(sites.ts[,i]))
  )
  inter.sites <- cbind(inter.sites, inter.site)
}

colnames(inter.sites) <- a.names
saveRDS(inter.sites, "Data/Sites Interpolated ts.rds")
full.no2.ts <- full.no2.ts[,-145]

no2.site1.ts <- full.no2.ts[,1]
#for all no2 sites
inter.sites <- na.interp(
  no2.site1.ts,
  lambda = NULL,
  linear = (frequency(no2.site1.ts) <= 1 | sum(!is.na(no2.site1.ts)) <= 2 * frequency(no2.site1.ts))
)
for (i in 2:211) {
  inter.site <- na.interp(
    full.no2.ts[,i],
    lambda = NULL,
    linear = (frequency(full.no2.ts[,i]) <= 1 | sum(!is.na(full.no2.ts[,i])) <= 2 * frequency(full.no2.ts[,i]))
  )
  inter.sites <- cbind(inter.sites, inter.site)
}

b.names <- sites[-b,1]
b.names <- b.names[-145]

colnames(inter.sites) <- b.names
saveRDS(inter.sites, "Data/Sites Interpolated NO2 ts.rds")

plot.ts(inter.sites[,1:10])
plot.ts(inter.sites[,11:20])
plot.ts(inter.sites[,21:30])
plot.ts(inter.sites[,31:40])
plot.ts(inter.sites[,41:50])
plot.ts(inter.sites[,51:60])
plot.ts(inter.sites[,61:70])
plot.ts(inter.sites[,71:80])
plot.ts(inter.sites[,81:90])
plot.ts(inter.sites[,91:100])

plot.ts(inter.sites[,c(65,92,76,145,12,175)], main = "Site−specfic Time Series")
plot.ts(inter.sites[,c(112,73,63,73,132,46)], main = "Site−specfic Time Series")


#for no2
#2, 13, 58, 99
site2.comps <- decompose(inter.sites[,2])
plot(site2.comps) #"Aberdeen Wellington Road"
abline(v = 2005.0833)
site13.comps <- decompose(inter.sites[,13])
plot(site13.comps) #"Billingham"
abline(v = 2005.0833)
site58.comps <- decompose(inter.sites[,58])
plot(site58.comps) #"Cwmbran Crownbridge"
site99.comps <- decompose(inter.sites[,99])
plot(site99.comps) #"Leeds Centre"

#for pm25
site3.comps <- decompose(inter.sites[,3])
plot(site3.comps) #"Brighton Preston Park" 
site7.comps <- decompose(inter.sites[,7])
plot(site7.comps) #"London Marylebone Road"
site9.comps <- decompose(inter.sites[,9])
plot(site9.comps) #"London Westminster" 
site13.comps <- decompose(inter.sites[,13])
plot(site13.comps) #"Wrexham"  


site1.sa <- inter.sites[,1] - site1.comps$seasonal
plot(site1.sa)

site1.forecast1 <- HoltWinters(inter.sites[,1], beta=FALSE, gamma=FALSE)
plot(site1.forecast1)
site1.forecast2 <- HoltWinters(inter.sites[,1], beta=TRUE, gamma=FALSE)
plot(site1.forecast2)

#aggregated
mean.site <- aggregate(cbind(temp, humid, precip, NO2, O3, PM25, SO2, AOD47, population, `PM25 PCM`, AOD55, `NO2 PCM`) ~  time, data = sitedata, mean)

ggplot(mean.site) +
  #geom_line(aes(x = time, y = PM25), color = "red") + 
  geom_line(aes(x = time, y = NO2), color = "blue") + 
  geom_line(aes(x = time, y = temp), color = "green") #+
  #geom_line(aes(x = time, y = humid), color = "orange") +
  #geom_line(aes(x = time, y = precip), color = "light blue")

mean.ts <- ts(mean.site, start=c(2008, 2), end=c(2019, 10), frequency=12)
plot(mean.ts[,c(2,4,5,7)])
mean.comp <- decompose(mean.ts[,7])
plot(mean.comp)

seasonal <- ts(mean.comp$seasonal, start=c(2008, 2), end=c(2019, 10), frequency=12)
trend <- ts(mean.comp$trend, start=c(2008, 2), end=c(2019, 10), frequency=12)

no2.ts <- mean.ts[,c(2,4,5)]
no2.ts <- cbind(no2.ts, seasonal)
no2.ts <- cbind(no2.ts, trend)
plot(no2.ts)


london.site <- subset(sitedata, Zone == "Greater London Urban Area")
mean.london <- aggregate(cbind(temp, humid, precip, NO2, O3, PM25, SO2, AOD47, population, `PM25 PCM`, AOD55, `NO2 PCM`) ~  time, data = london.site, mean)

london.ts <- ts(london.site, start=c(2008, 5), end=c(2019, 8), frequency=12)
plot(mean.ts[,c(2,4,5,7)])
no2.comp <- decompose(mean.ts[,5])
pm25.comp <-  decompose(mean.ts[,7])
temp.comp <-  decompose(mean.ts[,2])
precip.comp <-  decompose(mean.ts[,4])
plot(precip.comp)

no2.seasonal <-  ts(no2.comp$seasonal, start=c(2008, 5), end=c(2019, 8), frequency=12)
no2.trend <- ts(no2.comp$trend, start=c(2008, 5), end=c(2019, 8), frequency=12)
pm25.seasonal <- ts(pm25.comp$seasonal, start=c(2008, 5), end=c(2019, 8), frequency=12)
pm25.trend <- ts(pm25.comp$trend, start=c(2008, 5), end=c(2019, 8), frequency=12)
temp.seasonal <- ts(temp.comp$seasonal, start=c(2008, 5), end=c(2019, 8), frequency=12)
temp.trend <- ts(temp.comp$trend, start=c(2008, 5), end=c(2019, 8), frequency=12)
precip.seasonal <- ts(precip.comp$seasonal, start=c(2008, 5), end=c(2019, 8), frequency=12)
precip.trend <- ts(precip.comp$trend, start=c(2008, 5), end=c(2019, 8), frequency=12)

seasonal <- ts(mean.comp$seasonal, start=c(2008, 5), end=c(2019, 8), frequency=12)
trend <- ts(mean.comp$trend, start=c(2008, 5), end=c(2019, 8), frequency=12)

no2 <- cbind(no2.seasonal, no2.trend)
pm25 <- cbind(pm25.seasonal, pm25.trend)
temp <- cbind(temp.seasonal, temp.trend)
precip <- cbind(precip.seasonal, precip.trend)

pollution <- cbind(no2, pm25)
met <- cbind(temp, precip)

all <- cbind(pollution, met)
plot(all)

time <- mean.london$time
all <- cbind(time, all)
all.df <- as.data.frame(all)
names(all.df) <- c("time", "NO2 Seasonality", "NO2 Trend", "PM2.5 Seasonality", "PM2.5 Trend", "Temp Seasonality", "Temp Trend", "Precip Seasonality", "Precip Trend")
all.df$`NO2 Seasonality` <- scale(all.df$`NO2 Seasonality`)
all.df$`PM2.5 Seasonality` <- scale(all.df$`PM2.5 Seasonality`)
all.df$`Temp Seasonality` <- scale(all.df$`Temp Seasonality`)
all.df$`Precip Seasonality` <- scale(all.df$`Precip Seasonality`)

year.df <- subset(all.df, time > 48 & time < 62)
year <- ts(year.df, start=c(2009, 1), end=c(2009,12), frequency=12)

colnames(all) <- c("Time", "NO2 Seasonal", "Overall NO2", "PM2.5 Seasonal", "Overal PM2.5", "Temp Seasonal", "Overall Temp", "Precip Seasonal", "Overall Precip")
plot(all[,c(3,2,6,8)])

colors <- c("NO2" = "#65156e", "PM2.5" = "#9f2a63",  "Temp" = "#d44842", "Precip" = "#fac228")


ggplot(all.df) +
  #geom_line(aes(x = time, y = `NO2 Trend`, color = "NO2")) +
  #geom_line(aes(x = time, y = `PM2.5 Trend`, color = "PM2.5")) +
  geom_line(aes(x = time, y = `Temp Seasonality`, color = "Temp")) +
  geom_line(aes(x = time, y = `Precip Seasonality`, color = "Precip")) +
  geom_line(aes(x = time, y = `NO2 Seasonality`, color = "NO2")) +
  geom_line(aes(x = time, y = `PM2.5 Seasonality`, color = "PM2.5")) +
  labs(x = "Month", y = "", color = "Legend", title = "NO2 Trend and Seasonality") +
  xlim(c(45,180)) +
  scale_color_manual(values = colors)

