library(readr)
library(tidyr)

PM25.Daily <- read_csv("Data/Air Pollution/Monthly Means AURN/PM2.5 (Daily).csv")
PM25.Hourly <- read_csv("Data/Air Pollution/Monthly Means AURN/PM2.5 (Hourly).csv")
PM25.NonVol<- read_csv("Data/Air Pollution/Monthly Means AURN/PM2.5 Non-Vol (Hourly).csv")
PM25.Vol <- read_csv("Data/Air Pollution/Monthly Means AURN/PM2.5 Vol (Hourly).csv")

PM25.Daily <- PM25.Daily %>%
  pivot_longer(!`Site Name`:Year, names_to = "Month", values_to = "Daily Monthly Mean")
PM25.Hourly <- PM25.Hourly %>%
  pivot_longer(!`Site Name`:Year, names_to = "Month", values_to = "Hourly Monthly Mean")
PM25.NonVol <- PM25.NonVol %>%
  pivot_longer(!`Site Name`:Year, names_to = "Month", values_to = "Non-Vol Monthly Mean")
PM25.Vol <- PM25.Vol %>%
  pivot_longer(!`Site Name`:Year, names_to = "Month", values_to = "Vol Monthly Mean")

PM25.Daily$`Daily Monthly Mean` <- as.numeric(PM25.Daily$`Daily Monthly Mean`)
PM25.Hourly$`Hourly Monthly Mean` <- as.numeric(PM25.Hourly$`Hourly Monthly Mean`)
PM25.NonVol$`Non-Vol Monthly Mean` <- as.numeric(PM25.NonVol$`Non-Vol Monthly Mean`)
PM25.Vol$`Vol Monthly Mean` <- as.numeric(PM25.Vol$`Vol Monthly Mean`)


PM25.Daily.Hourly <- merge(PM25.Daily, PM25.Hourly, by = c("Site Name", "Year", "Month"))
PM25.Vol.NonVol <- merge(PM25.Vol, PM25.NonVol, by = c("Site Name", "Year", "Month"))
All <- merge(PM25.Daily.Hourly, PM25.Vol.NonVol, by = c("Site Name", "Year", "Month"))

All$Month[All$Month == "Jan"] <- 1
All$Month[All$Month == "Feb"] <- 2
All$Month[All$Month == "Mar"] <- 3
All$Month[All$Month == "Apr"] <- 4
All$Month[All$Month == "May"] <- 5
All$Month[All$Month == "Jun"] <- 6
All$Month[All$Month == "Jul"] <- 7
All$Month[All$Month == "Aug"] <- 8
All$Month[All$Month == "Sep"] <- 9
All$Month[All$Month == "Oct"] <- 10
All$Month[All$Month == "Nov"] <- 11
All$Month[All$Month == "Dec"] <- 12

current.site.data <- readRDS("Data/Site Data All.rds")

All$Month <- as.numeric(All$Month)

more.site.data <- merge(current.site.data, All, by.x = c("Site Name", "year", "month"), by.y = c("Site Name", "Year", "Month"), all = TRUE)

unique(more.site.data$PM25 == more.site.data$`Hourly Monthly Mean`)
sum(is.na(more.site.data$`Daily Monthly Mean`))

saveRDS(more.site.data, "Data/More Site Data.rds")
