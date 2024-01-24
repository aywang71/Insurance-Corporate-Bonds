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

###----------------------------------[Step 4]------------------------------------###
#Should be calculated based on a grouping by quarter and year (i.e quarterly)
#Should be an HHI value for each quarter

makeHHI <- function(df) {
  df$trns_amount <- abs(df$trns_amount)
  hhi1 <- df %>% group_by(Time, brok_id) %>% transmute(
    vol = sum(trns_amount)
  ) %>% as.data.frame() %>% unique() 
  
  hhi1Meta <- hhi1 %>% group_by(Time) %>% transmute(
    num = n(), 
    quartVol = sum(vol)
  ) %>% as.data.frame() %>% unique()
  hhi1 <- merge(hhi1, hhi1Meta, all.x = TRUE)
  hhi1$s <- (hhi1$vol / hhi1$quartVol)^2
  hhi1 <- hhi1 %>% group_by(Time) %>% transmute(
    index = (sum(s) - (1/num)) / (1 - 1/num)
  ) %>% as.data.frame() %>% unique()
  return(hhi1)
}

#4.1 among dealers only - grouping brok_id
subset <- df %>% filter(elec_platf == 0) %>% filter(ATS == 0)
hhi1 <- makeHHI(subset)

#4.2 among dealers and ep/ats
hhi2 <- makeHHI(df)

#4.3 among investors 
hhi3 <- df %>% group_by(Time, fund_id) %>% transmute(
  vol = sum(trns_amount)
) %>% as.data.frame() %>% unique() 

hhi3Meta <- hhi3 %>% group_by(Time) %>% transmute(
  num = n(), 
  quartVol = sum(vol)
) %>% as.data.frame() %>% unique()
hhi3 <- merge(hhi3, hhi3Meta, all.x = TRUE)
hhi3$s <- (hhi3$vol / hhi3$quartVol)^2
hhi3 <- hhi3 %>% group_by(Time) %>% transmute(
  index = (sum(s) - (1/num)) / (1 - 1/num)
) %>% as.data.frame() %>% unique()
remove(hhi3Meta)
gc()

hhi1$i <- hhi1$index
hhi1$index <- NULL
hhi2$ii <- hhi2$index
hhi2$index <- NULL
hhi3$iii <- hhi3$index
hhi3$index <- NULL
out <- merge(hhi1, hhi2)
out <- merge(out, hhi3)
write.csv(out, "HHI-indexes.csv")

out2 <- out %>%
  pivot_longer(c(i, ii, iii)) 

ggplot(out2, aes(Time, value, color = name)) + geom_point() + geom_line() + scale_x_yearqtr() + ggtitle("HHI indexes by quarter") + ylab("Proportion")
