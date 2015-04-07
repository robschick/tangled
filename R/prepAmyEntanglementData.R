library(ggplot2)
library(lubridate)
library(stringr)
library(gdata)
library(plyr)
# Entanglement Data Prep for Post-Model Window Overlays
rm(list=ls())
source(file='/Users/rob/Documents/code/rss10/rightwhales/makeTangle.r')
source(file = '/Users/rob/Documents/code/rss10/rightwhales/cleanMerge.r')
load(file="data/egsightings.rdata")
sixMo <- TRUE # if true restricts the long entanglement windows to be just 6 - months
days <- 182 # number of days == 6 months
# prior to the End Date

# next chunk is to bring in the entanglement table and pare it down
tangle    <- makeTangle()
idx       <- which(!is.na(tangle$StartDate)) # Find animals with a valid start date
tangle    <- tangle[idx,] # Keep animals with a valid start date
tangle$ID <- seq_along(1:nrow(tangle)) #
tangID    <- tangle$ID
tangle$wingt6mo <- tangle$EndDate - tangle$StartDate > days

# Now we want to add the gear carrying information
etime <- read.csv(file = 'data/TimingEntanglement.csv', header = TRUE)
# ID     <- sort(unique(sights[,'SightingEGNo']))
# n      <- length(ID)
startYr <- 1970
stopYr  <- max(sights[,'SightingYear'],na.rm=T)
yrvec  <- startYr:stopYr
nyr    <- length(yrvec)
monYr  <- cbind( rep(c(1:12),nyr),rep(yrvec,each=12) )
myName <- paste(monYr[,1],monYr[,2],sep='-')
# nt     <- nrow(monYr) 
tvec <- tangle$EntanglementComment
tdx  <- str_match(tvec, 'GEAR')
tdat <- tangle[which(tdx == "GEAR"),]
tndat <- tangle[which(is.na(tdx)),]

tndat$LastDatewGear <- as.Date('1600-01-01', '%Y-%m-%d') # Adding a fake date so it maintains formatting
tndat$EndDatePlus12 <- tndat$EndDate + (365 * 24 * 60 * 60) 
tndat$EndDatePlus6 <- tndat$EndDate + (182 * 24 * 60 * 60) 
tndat$EndDateMinus6 <- as.Date('1600-01-01', '%Y-%m-%d') # Adding a fake date so it maintains formatting
idx6 <- which(tndat$EndDate - tndat$StartDate > days)
tndat$EndDateMinus6[idx6] <- tndat$EndDate[idx6] - ((days) * 24 * 60 * 60) 

# The merge call will bring in the gear carrying times from etime.
# Note that I'm doing two merges because some animals in etime have NA for EventNo, which causes them to be lost in a merge based on both EGNo and EventNo
m1 <- merge(tdat, etime,  by = c('EGNo', 'EventNo'))
idx <- which(is.na(etime$EventNo))
etimeNA <- etime[idx,]
m2 <- merge(tdat, etimeNA,  by = c('EGNo'))
m2 <- subset(m2, select = -EventNo.y)
colnames(m2)[colnames(m2) == 'EventNo.x'] <- 'EventNo'
m2 <- m2[,c(1,3,2,4:20)]
tangleOut <- rbind(m1, m2) # Finally we bind these two data frames together

tangleOut$LastDatewGear <- as.Date(tangleOut$Last.date.w.gear, '%d-%b-%y')
tangleOut$EndDatePlus12 <- tangleOut$LastDatewGear + days(365)
tangleOut$EndDatePlus6 <- tangleOut$LastDatewGear + days(182)

tangleOut$EndDateMinus6 <- as.Date('1600-01-01', '%Y-%m-%d') # Adding a fake date so it maintains formatting
idx6 <- which(tangleOut$EndDate - tangleOut$StartDate > days)
tangleOut$EndDateMinus6[idx6] <- tangleOut$EndDate[idx6] - days(days)
idx <- is.na(tangleOut$LastDatewGear) & tangleOut$gear == 1
# tangleOut$EndDatePlus12[idx] <- tangleOut$EndDate[idx] + (365 * 24 * 60 * 60) # this is if you use 'EndDate' as the start of recovery
tangleOut$EndDatePlus12[idx] <- as.Date(tangleOut$line.gone, '%d-%b-%y')[idx] + days(365) # this is if you use 'line.gone' as the start of recovery
# note that the same column name (EndDatePlus12) has different meaning. For scarred whales, it's time since
# the entanglement window closed; for gear-carrying whales, it's the time since it was last
# seen carrying gear
colx <- which(colnames(tangleOut) %in% colnames(etime)[!colnames(etime) %in% colnames(tangle)])
tangleOut <- tangleOut[,-colx]
tangleOut <- rbind(tndat, tangleOut)

tangleOut$smonyr   <- paste(str_sub(tangleOut$StartDate, 6, 7), str_sub(tangleOut$StartDate, 1, 4), sep = '-')
tangleOut$emonyr   <- paste(str_sub(tangleOut$EndDate, 6, 7), str_sub(tangleOut$EndDate, 1, 4), sep = '-')
tangleOut$e12monyr <- paste(str_sub(tangleOut$EndDatePlus12, 6, 7), str_sub(tangleOut$EndDatePlus12, 1, 4), sep = '-')
tangleOut$emin6monyr <- paste(str_sub(tangleOut$EndDateMinus6, 6, 7), str_sub(tangleOut$EndDateMinus6, 1, 4), sep = '-')
tangleOut$e6monyr <- paste(str_sub(tangleOut$EndDatePlus6, 6, 7), str_sub(tangleOut$EndDatePlus6, 1, 4), sep = '-')
tangleOut$ldwgmonyr <- paste(str_sub(tangleOut$LastDatewGear, 6, 7), str_sub(tangleOut$LastDatewGear, 1, 4), sep = '-')
ws0   <- which(str_locate(str_sub(tangleOut$StartDate, 6, 7), '0')[, 1] == 1)
we0   <- which(str_locate(str_sub(tangleOut$EndDate, 6, 7), '0')[, 1] == 1)
we120 <- which(str_locate(str_sub(tangleOut$EndDatePlus12, 6, 7), '0')[, 1] == 1)
we60 <- which(str_locate(str_sub(tangleOut$EndDateMinus6, 6, 7), '0')[, 1] == 1)
wep60 <- which(str_locate(str_sub(tangleOut$EndDatePlus6, 6, 7), '0')[, 1] == 1)
weld60 <- which(str_locate(str_sub(tangleOut$LastDatewGear, 6, 7), '0')[, 1] == 1)
tangleOut[ws0, 'smonyr'] <- str_replace(tangleOut[ws0, 'smonyr'], '0', "")
tangleOut[we0, 'emonyr'] <- str_replace(tangleOut[we0, 'emonyr'], '0', "")
tangleOut[we120, 'e12monyr'] <- str_replace(tangleOut[we120, 'e12monyr'], '0', "")
tangleOut[we60, 'emin6monyr'] <- str_replace(tangleOut[we60, 'emin6monyr'], '0', "")
tangleOut[wep60, 'e6monyr'] <- str_replace(tangleOut[wep60, 'e6monyr'], '0', "")
tangleOut[weld60, 'ldwgmonyr'] <- str_replace(tangleOut[weld60, 'ldwgmonyr'], '0', "")


# this next chunk is to make sure I have a common integer that refers to the different combinations
tangleOut$gearInj <- NA
id1 <- which(tangleOut$Severity == 'minor' & tangleOut$gear == 0)
id2 <- which(tangleOut$Severity == 'minor' & tangleOut$gear == 1)
id3 <- which(tangleOut$Severity == 'moderate' & tangleOut$gear == 0)
id4 <- which(tangleOut$Severity == 'moderate' & tangleOut$gear == 1)
id5 <- which(tangleOut$Severity == 'severe' & tangleOut$gear == 0)
id6 <- which(tangleOut$Severity == 'severe' & tangleOut$gear == 1)
tangleOut$gearInj[id1] <- 6
tangleOut$gearInj[id2] <- 4
tangleOut$gearInj[id3] <- 5 
tangleOut$gearInj[id4] <- 2
tangleOut$gearInj[id5] <- 3
tangleOut$gearInj[id6] <- 1


# Save the data into one rdata file
save(tangleOut, sixMo, file="data/egAmyEntData.rdata")
