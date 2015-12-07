# Coding by Rob Schick, December 7, 2015
# Goal is to make the tally table for Amy's paper & SMM Presentation

rm(list=ls())
library(knitr)
library(lubridate)
library(dplyr)
source('/Users/rob/Documents/code/rss10/rightwhales/makeYearmon.r')
setwd('/Users/rob/Rdev/tangled')
load(file = 'data/eg_2015_newData_JUVTRUE__50000_wkspc.rdata')
load(file="data/egAmyEntData.rdata") # egAmyEntData.rdata contains tangleOut, tangRepro, tangNonRepro, so use the repro flag

# nind in each class
nind <- tangleOut %>% 
  group_by(gearInj) %>% 
  summarise(n = n())
nind

# months in each class
nmon <- tangleOut %>% 
  mutate(elapsed = EndDateWindow - StartDateWindow) %>% 
  group_by(gearInj) %>% 
    summarise(maxDur = max(elapsed),
              sumDays = sum(elapsed))
nmon