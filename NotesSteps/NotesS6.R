# clean up and setup
rm(list=ls()) # clean up any old stuff in R

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(stargazer)

setwd("C:/Users/hyper/OneDrive/Documents/GitHub/Insurance-Corporate-Bonds")

df <- readRDS('insurers_transactions.rds')
brokers <- read.csv('brokers.csv')

#Basic filtering conducted on the read-in data 
df <- df[df$trade_cost > 0, ]
df$quarter <- quarters(df$report_date)
df$year <- year(df$report_date)
df$Time <- as.yearqtr(paste(df$year, df$quarter))
df$trns_amount <- abs(df$trns_amount)

###----------------------------------[Step 6]------------------------------------###
#Add dep var dummy
df$epats <- ifelse(df$elec_platf == 1, 1, 0)
df$epats <- ifelse(df$ATS == 1, 1, df$epats)

#Add indep var dummy
#Need to create al entry exit points, then join on combinations of fund_id and yearqtr to pull forward entry variables while avoiding artifically inflating dataset
###Pull forward methodology for identifying lag from step 5

# ###----------------------------------[Step 5]------------------------------------###
# s5 <- df %>% group_by(Time, fund_id) %>% transmute(
#   quartVol = sum(trns_amount)
# ) %>% as.data.frame %>% unique()
# s5$Time <- as.POSIXct(s5$Time)
#
# #Add all pairwise combinations of rows
# dates <- unique(s5$Time)
# fund_ids <- unique(s5$fund_id)
# pairs <- expand.grid(Time = dates, fund_id = fund_ids, stringsAsFactors = F)
# pairs$quartVol <- 0
#
# s5 <- rbind(s5, pairs)
# s5 <- s5 %>% group_by(Time, fund_id) %>% transmute(
#   quartVol = sum(quartVol)
# ) %>% as.data.frame %>% unique()
#
# remove(pairs)
# remove(dates)
# remove(fund_ids)
# remove(brokers)
# gc()
#
#
# s5 <- s5 %>%
#   group_by(fund_id) %>%
#   arrange(Time) %>%
#   mutate(
#     act4q = lag(quartVol, 4) + lag(quartVol, 3) + lag(quartVol, 2) + lag(quartVol, 1),
#     act2q = lag(quartVol, 2) + lag(quartVol, 1),
#     act8q = lag(quartVol, 4) + lag(quartVol, 3) + lag(quartVol, 2) + lag(quartVol, 1) +
#       lag(quartVol, 8) + lag(quartVol, 7) + lag(quartVol, 6) + lag(quartVol, 5)
#   )
#
# s5$act4q <- ifelse(s5$Time < as.POSIXct("2005-12-31"), 0, s5$act4q)
# s5$act8q <- ifelse(s5$Time < as.POSIXct("2006-12-31"), 0, s5$act8q)
# s5$act2q <- ifelse(s5$Time < as.POSIXct("2005-06-06"), 0, s5$act2q)
#
# #aggregate to count entrants and exits
# s5$entry <- ifelse(s5$quartVol != 0 & s5$act4q == 0, 1, 0)
# s5$exit <- ifelse(s5$act4q > 0 & s5$act2q == 0, 1, 0)
#
# #s5 here written out
# write.csv(s5, file = "step5Calculations.csv")
s5 <- read.csv("step5Calculations.csv")

# #add indep var which is entry value lagged
# sub <- s5 %>%
#   group_by(fund_id) %>%
#   arrange(Time) %>%
#   mutate(
#     indep1 = ifelse(lag(entry,1) == 1, 1, 0),
#     indep2 = ifelse(lag(entry,2) == 1, 1, 0),
#     indep3 = ifelse(lag(entry,3) == 1, 1, 0),
#     indep4 = ifelse(lag(entry,4) == 1, 1, 0),
#     indep5 = ifelse(lag(entry,5) == 1, 1, 0),
#     indep6 = ifelse(lag(entry,6) == 1, 1, 0),
#     indep7 = ifelse(lag(entry,7) == 1, 1, 0),
#     indep8 = ifelse(lag(entry,8) == 1, 1, 0),
#     exit = ifelse(lag(exit) == 1 & exit == 1, 0, exit)
#   )
#
# #sub here written out
# write.csv(sub, "indepVarCalc.csv")
sub <- read.csv("indepVarCalc.csv")  

#Then need to rejoin data onto df for a by-transaction basis with quarterly data
df$Time <- as.POSIXct(df$Time)
df2 <- merge(df, sub, by = c('fund_id', 'Time'))

#With data set up, now want to run regressions
library(lfe)
df2$Time <- as.factor(df2$Time)
df2$cusip <- as.factor(df2$cusip)
#FER <- felm(epats ~ indep1| Time * cusip | 0 | 0, data = df2)
library(fixest)
df2$Time_cusip <- interaction(df2$Time, df2$cusip)
logit_fe_model2 <- feglm(epats ~ indep1 | Time + cusip, family = binomial(link = "logit"), data = df2)

models <- list (
  felm(epats ~ indep1| Time + cusip | 0 | 0, data = df2),
  felm(epats ~ indep2| Time + cusip | 0 | 0, data = df2),
  felm(epats ~ indep3| Time + cusip | 0 | 0, data = df2),
  felm(epats ~ indep4| Time + cusip | 0 | 0, data = df2),
  felm(epats ~ indep5| Time + cusip | 0 | 0, data = df2),
  felm(epats ~ indep6| Time + cusip | 0 | 0, data = df2),
  felm(epats ~ indep7| Time + cusip | 0 | 0, data = df2),
  felm(epats ~ indep8| Time + cusip | 0 | 0, data = df2)
)

# results <- map(models, ~{
#   model_summary <- summary(.x)
#   data.frame(
#     coef = model_summary$coefficients[, 1],
#     s_error = model_summary$coefficients[, 2],
#     t_stat = model_summary$coefficients[, 3],
#     p_value = model_summary$coefficients[, 4],
#     stringsAsFactors = FALSE
#   )
# }) %>% bind_rows(.id = "FixedEffectModel")


stargazer(FER, type = 'text', digits = NA,
          add.lines=list(
            c('Firm \\times Time FE','A','X','X','X','X','X'),
            c('Controls','','','X','X','X','X'),
            c('Call/Put FE','A','','','X','','X'), 
            c('Seniority FE','','','','','X','X')
          ))

