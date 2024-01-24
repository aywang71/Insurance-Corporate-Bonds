# clean up and setup
rm(list=ls()) # clean up any old stuff in R

library(tidyverse)
library(dplyr)
library(lubridate)
library(gridExtra)

setwd("C:/Users/hyper/OneDrive/Documents/GitHub/Insurance-Corporate-Bonds")

df <- readRDS('insurers_transactions.rds')
brokers <- read.csv('brokers.csv')

#Basic filtering conducted on the read-in data 
df <- df[df$trade_cost > 0, ]

#TODO: may need to do some kind of filtering on outliers.. i.e Loews Corp with 26,117,500,000 max
#cannot determine which ones should have 10^-3 factor multiplied to bring it back to thousands

agg <- df %>% group_by(report_date, ATS) %>% summarise(
  vol = sum(trade_cost)
) %>% as.data.frame()
agg$ATS <- as.factor(agg$ATS)

agg2 <- agg
dailyVolume <- ggplot(data = agg2, aes(x = report_date, y = vol, color = ATS)) + geom_point() + xlab("Time") + ylab("Volume (thousands USD)") + ggtitle("Daily aggregation: Volume over time")

#5297 total unique days, only 2775 dates have ATS == 1 reporting
agg <- pivot_wider(agg, names_from = ATS, values_from = vol)
agg$`1` <- ifelse(is.na(agg$`1`), 0, agg$`1`)
agg$prop <- agg$`1` / agg$`0`

dailyProp <- ggplot(data = agg, aes(x = report_date,y =prop)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Proportion of e-traded volume") + ggtitle("Daily aggregation: Proportion over time")
dailyZero <- ggplot(data = agg, aes(x = report_date, y = `0`)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Volume of OTC Brokers") + ggtitle("Daily aggregation: Volume over time")
dailyOne <- ggplot(data = agg, aes(x = report_date, y = `1`)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Volume of E-Trades") + ggtitle("Daily aggregation: Volume over time")
#Dropoff in trades at end of 2020 likely due to inflation concerns regarding Fed raising rates (i.e corporate bonds will be worth less)

#-------------------------[Upscale onto a weekly basis]-------------------------
agg <- agg2
agg$month <- month(agg$report_date)
agg$year <- year(agg$report_date)
agg$week <- week(agg$report_date)

agg <- agg %>% group_by(year, month, week, ATS) %>% summarise(
  vol = sum(vol)
) %>% as.data.frame()
agg$report_date <- as.Date(paste(agg$year, agg$month, "01", sep = "-"))

agg2 <- agg
weeklyVolume <- ggplot(data = agg2, aes(x = report_date, y = vol, color = ATS)) + geom_point() + xlab("Time") + ylab("Volume (thousands USD)") + ggtitle("Weekly aggregation: Volume over time")

agg <- pivot_wider(agg, names_from = ATS, values_from = vol)
agg$`1` <- ifelse(is.na(agg$`1`), 0, agg$`1`)
agg$prop <- agg$`1` / agg$`0`

weeklyProp <- ggplot(data = agg, aes(x = report_date,y = prop)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Proportion of e-traded volume") + ggtitle("Weekly aggregation: Proportion over time")
weeklyZero <- ggplot(data = agg, aes(x = report_date, y = `0`)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Volume of OTC Brokers") + ggtitle("Weekly aggregation: Volume over time")
weeklyOne <- ggplot(data = agg, aes(x = report_date, y = `1`)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Volume of E-Trades") + ggtitle("Weekly aggregation: Volume over time")

#-------------------------[Upscale onto a monthly basis]-------------------------
agg <- agg2

agg <- agg %>% group_by(year, month, ATS) %>% summarise(
  vol = sum(vol)
) %>% as.data.frame()
agg$report_date <- as.Date(paste(agg$year, agg$month, "01", sep = "-"))

agg2 <- agg
monthlyVolume <- ggplot(data = agg2, aes(x = report_date, y = vol, color = ATS)) + geom_point() + xlab("Time") + ylab("Volume (thousands USD)") + ggtitle("Monthly aggregation: Volume over time")

agg <- pivot_wider(agg, names_from = ATS, values_from = vol)
agg$`1` <- ifelse(is.na(agg$`1`), 0, agg$`1`)
agg$prop <- agg$`1` / agg$`0`

monthlyProp <- ggplot(data = agg, aes(x = report_date,y = prop)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Proportion of e-traded volume") + ggtitle("Monthly aggregation: Proportion over time")
monthlyZero <- ggplot(data = agg, aes(x = report_date, y = `0`)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Volume of OTC Brokers") + ggtitle("Monthly aggregation: Volume over time")
monthlyOne <- ggplot(data = agg, aes(x = report_date, y = `1`)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Volume of E-Trades") + ggtitle("Monthly aggregation: Volume over time")

#-------------------------[Upscale onto a yearly basis]-------------------------
agg <- agg2

agg <- agg %>% group_by(year, ATS) %>% summarise(
  vol = sum(vol)
) %>% as.data.frame()
agg$report_date <- as.Date(paste(agg$year, "01", "01", sep = "-"))

agg2 <- agg
yearlyVolume <- ggplot(data = agg2, aes(x = report_date, y = vol, color = ATS)) + geom_point() + xlab("Time") + ylab("Volume (thousands USD)") + ggtitle("Yearly aggregation: Volume over time")

agg <- pivot_wider(agg, names_from = ATS, values_from = vol)
agg$`1` <- ifelse(is.na(agg$`1`), 0, agg$`1`)
agg$prop <- agg$`1` / agg$`0`

yearlyProp <- ggplot(data = agg, aes(x = report_date,y = prop)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Proportion of e-traded volume") + ggtitle("Yearly aggregation: Proportion over time")
yearlyZero <- ggplot(data = agg, aes(x = report_date, y = `0`)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Volume of OTC Brokers") + ggtitle("Yearly aggregation: Volume over time")
yearlyOne <- ggplot(data = agg, aes(x = report_date, y = `1`)) + geom_point() + geom_smooth(colour="red") + xlab("Time") + ylab("Volume of E-Trades") + ggtitle("Yearly aggregation: Volume over time")

#arrangements
volumeGrid <- grid.arrange(dailyVolume, weeklyVolume, monthlyVolume, yearlyVolume, ncol = 2)
propGrid <- grid.arrange(dailyProp, weeklyProp, monthlyProp, yearlyProp, ncol = 2)
zeroGrid <- grid.arrange(dailyZero, weeklyZero, monthlyZero, yearlyZero, ncol = 2)
oneGrid <- grid.arrange(dailyOne, weeklyOne, monthlyOne, yearlyOne, ncol = 2)