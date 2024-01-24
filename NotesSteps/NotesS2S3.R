# clean up and setup
rm(list=ls()) # clean up any old stuff in R

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)

setwd("C:/Users/hyper/OneDrive/Documents/GitHub/Insurance-Corporate-Bonds")

df <- readRDS('insurers_transactions.rds')
brokers <- read.csv('brokers.csv')

#Basic filtering conducted on the read-in data 
df <- df[df$trade_cost > 0, ]

#2. Lets compute some simple statistics (we always think in terms of quarters, meaning q1, q2, q3, q4 for each year):

#need to define quarter
df$quarter <- quarters(df$report_date)
df$year <- year(df$report_date)
df$Time <- as.yearqtr(paste(df$year, df$quarter))

#(a) Quarterly number of investors (fund id)
investors <- df %>% group_by(Time) %>% summarise(numInvestors = length(unique(fund_id)))
ggplot(investors, aes(Time, numInvestors)) + 
  geom_point() + 
  geom_line() +
  scale_x_yearqtr() + ggtitle("Unique investors by quarter")

#(b) Quarterly number of dealers (brokid)–we should exclude EP (elec pltf) and ATS
dealersNon <- df %>% filter(elec_platf == 0) %>% filter(ATS == 0) %>% group_by(Time) %>% summarise(numBrokers = length(unique(brok_id)))
ggplot(dealersNon, aes(Time, numBrokers)) + 
  geom_point() + 
  geom_line() +
  scale_x_yearqtr() + ggtitle("Unique dealers (non EP/ATS) by quarter")

#(c) Quarterly number of EP and ATS 
#Note: All elec_platf are automatically in ATS
dealersEP <- df %>% filter(elec_platf == 1) %>% filter(ATS == 1) %>% group_by(Time) %>% summarise(EPATS = length(unique(brok_id)))
ggplot(dealersEP, aes(Time, EPATS)) + 
  geom_point() + 
  geom_line() +
  scale_x_yearqtr() + ggtitle("EP/ATS dealers by quarter")

#3. Aggregate trading volume of dealers, EP and ATS per quarter
dealersNonVol <- df %>% filter(elec_platf == 0) %>% filter(ATS == 0) %>% group_by(Time) %>% summarise(trns_amountNon = sum(trns_amount), trade_costNon = sum(trade_cost))
ggplot(dealersNonVol, aes(Time, trade_costNon)) + 
  geom_point() + 
  geom_line() +
  scale_x_yearqtr() + ggtitle("non EP/ATS Trade Costs by quarter")

dealersEPVol <- df %>% filter(elec_platf == 1) %>% filter(ATS == 1) %>% group_by(Time) %>% summarise(trns_amountEP = sum(trns_amount), trade_costEP = sum(trade_cost))
ggplot(dealersEPVol, aes(Time, trade_costEP)) + 
  geom_point() + 
  geom_line() +
  scale_x_yearqtr() + ggtitle("EP/ATS Trade Costs by quarter")

output <- merge(investors, dealersNon, by = "Time")
output <- merge(output, dealersNonVol, by = "Time")
output <- left_join(output, dealersEP, by = "Time")
output <- left_join(output, dealersEPVol, by = "Time")
