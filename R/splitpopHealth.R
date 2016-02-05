# Coding by Rob Schick, May 6, 2015
# The goal of this is to split the pop health into two
# matrices: 1) unimpacted and non-reproductive animals
# and 2) unimpacted and reproductive animals

rm(list=ls())
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(stringr)
library(plyr)
library(denstrip)
load(file = 'data/eg_2015_newData_JUVTRUE__50000_wkspc.rdata')
load(file="data/egAmyEntData.rdata") 
# egAmyEntData.rdata contains tangleOut, tangRepro, tangNonRepro, so use the repro flag
load(file = 'data/healthAnomaly.rda') # contains 'anom' which is deviation from pop health (Adult males and juveniles) for all animals
useAnom <- TRUE

if (useAnom){
  healthmean <- anom
  for(i in 1:nrow(healthmean)){
    healthmean[i, 1:(firstSight[i] - 1)] <- NA
    if (death[i] < nt) {
      healthmean[i, (death[i] + 1):nt] <- NA  
    }
  }
} else {
  healthmean <- sumh / g
  for(i in 1:nrow(healthmean)){
    healthmean[i, 1:(firstSight[i] - 1)] <- NA
    if (death[i] < nt) {
      healthmean[i, (death[i] + 1):nt] <- NA  
    }
  }
}


# get the reproductively active females set up - use tangRepro
# first use calfTable to parse out healthmean into a subset of reproductively active females
repidx <- match(unique(calfTable$EGNo), ID)
repHealth <- healthmean[repidx, ]

# only get health values during times _after_ first year of calving; use firstCalfidx
fcalf <- tangRepro[, c('EGNo', 'firstCalfidx')]
idx <- duplicated(fcalf)
fcalf <- fcalf[!idx, ]

for (i in 1:length(fcalf$EGNo)) {
  ind <- fcalf$EGNo[i]
  repHealth[which(row.names(repHealth) == ind), 1:fcalf$firstCalfidx[i]] <- NA
}

# set unimpacted times between entanglement events by looping over entanglement events
# and assigning NAs to the entangled periods.
# also in the loop, set to NA any health values for times after severe entanglement events
# n.b. that I altered this per Amy's request to only have the unimpacted times be prior
# to a first entanglement.

for (i in 1:nrow(tangRepro)) {
  ind <- tangRepro$EGNo[i]
  sidx <- match(tangRepro$smonyr[i], myName)
  eidx <- match(tangRepro$emonyr[i], myName)
  repHealth[which(row.names(repHealth) == ind), sidx:nt] <- NA # this is per Amy's request 12/4/15
  sevidx <- match(tangRepro$fsevmonyr[i], myName)
  if (!is.na(sevidx)) {
    repHealth[which(row.names(repHealth) == ind), sevidx:nt] <- NA # set post-severe times  
  }
}



###############################################################
# get the other animals set up - use tangleOut
nonrepHealth <- healthmean

# to get health times before first year of calving, use firstCalfidx
# to set all months after first calf to NA
for (i in 1:length(fcalf$EGNo)) {
  ind <- fcalf$EGNo[i]
  nonrepHealth[which(row.names(nonrepHealth) == ind), fcalf$firstCalfidx[i]:nt] <- NA
}

# set unimpacted times &  set post-severe times
for (i in 1:nrow(tangleOut)) {
  ind <- tangleOut$EGNo[i]
  sidx <- match(tangleOut$smonyr[i], myName)
  eidx <- match(tangleOut$emonyr[i], myName)
  nonrepHealth[which(row.names(nonrepHealth) == ind), sidx:nt] <- NA
  sevidx <- match(tangleOut$fsevmonyr[i], myName)
  if (!is.na(sevidx)) {
    nonrepHealth[which(row.names(nonrepHealth) == ind), sevidx:nt] <- NA # set post-severe times  
  }
}

repvec <- as.vector(repHealth)[!is.na(as.vector(repHealth))]
nonrepvec <- as.vector(nonrepHealth)[!is.na(as.vector(nonrepHealth))]
save(nonrepvec, repvec, file = 'data/unimpacted.rdata')

# test quick plotting to see what they look like
boxplot(repvec)
plot(x = seq(100), xlim = c(0, 100), ylim = c(0, 1), type = 'n')
denstrip(repvec, at = 0.5, width = 1)


boxplot(nonrepvec)
plot(x = seq(100), xlim = c(0, 100), ylim = c(0, 1), type = 'n')
denstrip(nonrepvec, at = 0.5, width = 1)

# both side by side
plot(x = seq(100), xlim = c(0, 100), ylim = c(0, 2), type = 'n')
denstrip(repvec, at = 1.5, width = 1)
denstrip(nonrepvec, at = 0.5, width = 1)


