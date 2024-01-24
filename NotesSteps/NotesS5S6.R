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
df$quarter <- quarters(df$report_date)
df$year <- year(df$report_date)
df$Time <- as.yearqtr(paste(df$year, df$quarter))

###----------------------------------[Step 5]------------------------------------###