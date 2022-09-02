library(readxl)
library(ggplot2)

aurnpm25 <- read_excel("Data/Air Pollution/AURN Monitoring Sites/All AURN MS.xlsx", sheet = 1)
aurnso2 <- read_excel("Data/Air Pollution/AURN Monitoring Sites/All AURN MS.xlsx", sheet = 2)
aurnnox <- read_excel("Data/Air Pollution/AURN Monitoring Sites/All AURN MS.xlsx", sheet = 3)
aurno3 <- read_excel("Data/Air Pollution/AURN Monitoring Sites/All AURN MS.xlsx", sheet = 4)

aurnnox$`NOx Start Date` <- as.numeric(aurnnox$`NOx Start Date`)
aurnnox$`NOx Start Date` <-  as.POSIXct.Date(as.Date(aurnnox$`NOx Start Date`-2, origin = "1900-01-01"))

#aurnpm25 <- aurnpm25[order(aurnpm25$`PM2.5 Start Date`),]
#aurnso2 <- aurnso2[order(aurnso2$`SO2 Start Date`),]
#aurnnox <- aurnnox[order(aurnnox$`NOx Start Date`),]
#aurno3 <- aurno3[order(aurno3$`O3 Start Date`),]

aurnpm25[,9][is.na(aurnpm25[,9])] <- as.Date("2022-02-25")
aurnso2[,8][is.na(aurnso2[,8])] <- as.Date("2022-02-25")
aurnnox[,8][is.na(aurnnox[,8])] <- as.Date("2022-02-25")
aurno3[,8][is.na(aurno3[,8])] <- as.Date("2022-02-25")

#aurnpm25 <- aurnpm25[,c(4,6,7,8)]
aurnso2 <- aurnso2[,c(4,6,7,8)]
aurnnox <- aurnnox[,c(4,6,7,8)]
aurno3 <- aurno3[,c(4,6,7,8)]

aurn12 <- merge(aurnpm25, aurnso2, by = c("Site Name", "Zone"), all = TRUE)
aurn34 <- merge(aurnnox, aurno3, by = c("Site Name", "Zone"), all = TRUE)
aurn <- merge(aurn12, aurn34, by = c("Site Name", "Zone"), all = TRUE)

library(dplyr)
library(viridis)
#aurn <- aurn[order(aurn$`PM2.5 Start Date`),]

aurnpm25 <- subset(aurnpm25, Area != "Ireland")
aurnpm25$Area <- as.factor(aurnpm25$Area)
aurnpm25 <- aurnpm25[order(aurnpm25$`PM2.5 Start Date`),]
aurnpm25$`Environment Type` <- as.factor(aurnpm25$`Environment Type`)


aurnpm25$pm25openfor <- as.numeric(aurnpm25$`PM2.5 End Date`- aurnpm25$`PM2.5 Start Date`)/365
aurnpm25$pm25openpart <- c(rep(0,106,1))
for (i in 1:106) {
  aurnpm25$pm25openpart[i] <- min(aurnpm25$pm25openfor[i]/16, 1)*100
}



startdate <- as.POSIXct(as.Date("2005-01-01", origin = "1900-01-01"))
enddate <- as.POSIXct(as.Date("2020-01-31", origin = "1900-01-01"))
breakdate <- c(seq(startdate, enddate, by = "year"))
lims <- as.POSIXct(strptime(c("2005-01-01", "2020-12-31"), 
                            format = "%Y-%m-%d"))
labels <- c("2005","","","","","2010","","","","","2015","","","","","2020")

devtools::install_github("m-clark/NineteenEightyR")
library("NineteenEightyR")

ggplot(aurnpm25) +
  geom_linerange(aes(xmin = `PM2.5 Start Date`, xmax = `PM2.5 End Date`, y = `Site Name`, color = `Environment Type`), size = 1.5, alpha = 0.8) +
  #scale_colour_viridis_d(option = "B", name = "Enivornment Type") +
  scale_fill_manual(values = c("#7e03a8", "#f0f921", "#cc4778", "#0d0887", "#f89540"), aesthetics = c("colour")) +
  facet_wrap(~ Area, scales = "free_y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.spacing.x = unit(1, "lines")) +
  scale_x_datetime(breaks = breakdate, labels = labels) +
  coord_cartesian(xlim = c(startdate, enddate)) +
  scale_y_discrete(limits=rev) 
 
aurn$Zone <- as.factor(aurn$Zone)

aurn$pm25openfor<- as.numeric(aurn$`PM2.5 End Date`- aurn$`PM2.5 Start Date`)

ggplot(aurn) +
  geom_linerange(aes(xmin = `PM2.5 Start Date`, xmax = `PM2.5 End Date`, y = `Site Name`, color = pm25openfor), size = 1.5, alpha = 0.8) +
  scale_colour_viridis(option = "A") +
  facet_wrap(~ Zone, scales = "free_y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")


ggplot(aurn) +
  geom_linerange(aes(xmin = `PM2.5 Start Date`, xmax = `PM2.5 End Date`, y = `Site Name`), color = "blue", size = 2, alpha = 0.7, position = position_dodge(width = 0.2)) +
  geom_linerange(aes(xmin = `NOx Start Date`, xmax = `NOx End Date`, y = `Site Name`), color = "red", size = 2, alpha = 0.7, position = position_dodge(width = 0.2)) +
  geom_linerange(aes(xmin = `SO2 Start Date`, xmax = `SO2 End Date`, y = `Site Name`), color = "green",size = 2, alpha = 0.7, position = position_dodge(width = 0.2)) + 
  geom_linerange(aes(xmin = `O3 Start Date`, xmax = `O3 End Date`, y = `Site Name`), color = "yellow", size = 2, alpha = 0.7, position = position_dodge(width = 0.2)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")  
