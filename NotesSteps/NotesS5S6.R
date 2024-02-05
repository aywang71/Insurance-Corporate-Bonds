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
df$trns_amount <- abs(df$trns_amount)

###----------------------------------[Step 5]------------------------------------###
df <- df %>% group_by(Time, brok_id) %>% transmute(
  quartVol = sum(trns_amount)
) %>% as.data.frame %>% unique()
df$Time <- as.POSIXct(df$Time)

#Add all pairwise combinations of rows 
dates <- unique(df$Time)
brok_ids <- unique(df$brok_id)
pairs <- expand.grid(Time = dates, brok_id = brok_ids, stringsAsFactors = F)
pairs$quartVol <- 0

df <- rbind(df, pairs)
df <- df %>% group_by(Time, brok_id) %>% transmute(
  quartVol = sum(quartVol)
) %>% as.data.frame %>% unique()
remove(pairs)
remove(dates)
remove(brok_ids)
remove(brokers)
gc()
#Another way to do it would be to do a strict 4 row look forward by pairwise adding 0 rows for each time and brok_id pair 

# subset <- df[df$brok_id == 11405, ]
# subset$brok_id <- as.numeric(subset$brok_id)
# subset2 <- subset %>% arrange(Time) %>%
#   mutate(
#     act4q = sapply(Time, function(t) {
#       sum(filter(., Time >= t - years(1) & Time < t)$quartVol, na.rm = TRUE)
#     }), 
#     act2q = sapply(Time, function(t) {
#       sum(filter(., Time >= t - days(185) & Time < t)$quartVol, na.rm = TRUE)
#     })
#   )
# 
# subset2$act4q <- ifelse(subset$Time < as.POSIXct("2005-12-31"), 0, subset2$act4q)
# subset2$act2q <- ifelse(subset$Time < as.POSIXct("2005-06-06"), 0, subset2$act2q)
# df <- df %>%
#   # ungroup()
#   mutate(
#     act4q = sapply(1:n(), function(i) {
#       sum(filter(., brok_id == brok_id[i] & Time >= Time[i] - years(1) & Time < Time[i])$quartVol, na.rm = TRUE)
#     }),
#     act2q = sapply(1:n(), function(i) {
#       sum(filter(., brok_id == brok_id[i] & Time >= Time[i] - days(185) & Time < Time[i])$quartVol, na.rm = TRUE)
#     })
#   ) %>% ungroup()

df <- df %>%
  group_by(brok_id) %>%
  arrange(Time) %>%
  mutate(
    act4q = lag(quartVol, 4) + lag(quartVol, 3) + lag(quartVol, 2) + lag(quartVol, 1), 
    act2q = lag(quartVol, 2) + lag(quartVol, 1)
  )

df$act4q <- ifelse(df$Time < as.POSIXct("2005-12-31"), 0, df$act4q)
df$act2q <- ifelse(df$Time < as.POSIXct("2005-06-06"), 0, df$act2q)

#aggregate to count entrants and exits
df$entry <- ifelse(df$quartVol != 0 & df$act4q == 0, 1, 0)
df$exit <- ifelse(df$act4q > 0 & df$act2q == 0, 1, 0)

df <- df %>% mutate(
  exit = ifelse(lag(exit) == 1 & exit == 1, 0, exit)
)

out <- df %>% group_by(Time) %>% transmute(
  entry = sum(entry), 
  exit = sum(exit)
) %>% as.data.frame() %>% unique()
out <- out %>% filter(Time >= as.POSIXct("2006-01-01"))
out$Time <- as.yearqtr(out$Time)

out2 <- out %>%
  pivot_longer(c(entry, exit)) 

fundPlot <- ggplot(out2, aes(Time, value, color = name)) + geom_point() + geom_line() + scale_x_yearqtr(n = 10) + ggtitle("brok_id entry and exit by quarter") + ylab("Count")
