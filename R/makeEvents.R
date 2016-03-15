# Coding by Rob Schick, April 20, 2015
# The goal of this is to make the box plots of time to death by entanglement severity

rm(list=ls())
library(ggplot2)
library(lubridate)
library(denstrip)
library(data.table)
library(reshape)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(stringr)
library(plyr)
library(dplyr)
source('/Users/rob/Documents/code/rss10/rightwhales/makeYearmon.r')
wd <- '/Users/rob/Rdev/tangled/'
setwd(wd)
# load(file = paste(wd, 'gibbsoutput/eg_2015_newData_JUVTRUE__50000_wkspc.rdata', sep = '') )
# load(file = 'data/eg_2015_newData_JUVTRUE__50000_wkspc.rdata')
load(file = 'data/eg_203_ng_50000_BIG_25000.rdata')
load(file="data/egAmyEntData.rdata")
tSub <- tangleOut

# for some reason, two of these contain duplicated info: 1249 & 1980, so I'm going to remove one of them
idx <- which(tSub$EGNo == 1249 & tSub$EventNo == 2)
tSub <- tSub[-idx[2],]
idx <- which(tSub$EGNo == 1980 & tSub$EventNo == 2)
tSub <- tSub[-idx[2],]

# find the last Entanglement event
events <- tSub %>% group_by(EGNo) %>% top_n(n=1, EventNo) %>% arrange(EGNo)

# need to find the max estimate for death from deathyr
# this will produce 'dtime' which is that month of imputed death
idx <- events$EGNo %in% ID
events <- events[idx, ]
events$dtime <- NA
events$knownD <- FALSE
events$presD <- FALSE
events$presA <- FALSE

# We need to choose a cutoff date for censoring;
# per email from Philip Hamilton, VHA health is matched
# through 2012; sightings matched through 2013
dcut <- which(myName == '12-2013') 

for (i in 1:length(unique(events$EGNo))) {
  
  id <- which(ID == unique(events$EGNo)[i])
  dsub <- deathyr[id, ]
  events$dtime[i] <- which.max(dsub)
  
  if (any(dsub == ng) & events$dtime[i] < dcut) {
    events$knownD[i] <- TRUE
  }
  
  if (any(dsub != ng) & events$dtime[i] < dcut) {
    events$presD[i] <- TRUE
  }
  
  if (any(dsub == ng) & events$dtime[i] > dcut) {
    events$presA[i] <- TRUE
  }
  
}
events <- mutate(events, ewindmonyrID = match(ewindmonyr, myName))
save(events, file = 'data/events.rda')