library(ggplot2)
library(lubridate)
library(stringr)
library(gdata)
library(plyr)
# Entanglement Data Prep for Post-Model Window Overlays
rm(list=ls())
source(file='R/makeTangle.R')
source(file = 'R/cleanMerge.R')
load(file="data/egsightings.rdata")
load(file="data/calfTable.rdata")
days <- months(6)

# next chunk is to bring in the entanglement table and pare it down
tangle    <- makeTangle()
idx       <- which(!is.na(tangle$StartDate)) # Find animals with a valid start date
tangle    <- tangle[idx,] # Keep only those animals with a valid start date
tangle$ID <- seq_along(1:nrow(tangle)) #
tangID    <- tangle$ID
tangle$wingt6mo <- tangle$EndDate - tangle$StartDate > days
# tangle <- tangle[,-which(colnames(tangle) == 'EntanglementComment')]
# tangle <- tangle[,-which(colnames(tangle) == 'TimeFrame')]

# Now we want to add the gear carrying information
etime <- read.csv(file = 'data/TimingEntanglementReformatDate.csv', header = TRUE)
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
tndat$wlength <- tndat$EndDate - tndat$StartDate
tndat$StartDateWindow <- tndat$StartDate
tndat$EndDateWindow <- tndat$EndDate

# anything >= 3 months gets three months
idx6 <- which(tndat$wlength > 3 * (365.25/12))
tndat$StartDateWindow[idx6] <- tndat$EndDate[idx6] %m-% months(3)

# anything >= 1 & <2 months gets 1 months
idx6 <- which(tndat$EndDate - tndat$StartDate >= 1 * (365.25/12) & tndat$EndDate - tndat$StartDate < 2 * (365.25/12))
tndat$StartDateWindow[idx6] <- tndat$EndDate[idx6] %m-% months(1)

# anything <1 months gets 1 months
idx6 <- which(tndat$EndDate - tndat$StartDate < 1 * (365.25/12))
tndat$StartDateWindow[idx6] <- tndat$EndDate[idx6] %m-% months(1)

# anything >= 2 & <3 months gets 2 months
idx6 <- which(tndat$EndDate - tndat$StartDate >= 2 * (365.25/12) & tndat$EndDate - tndat$StartDate < 3 * (365.25/12))
tndat$StartDateWindow[idx6] <- tndat$EndDate[idx6] %m-% months(2)

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
tangleOut$LineGone <- as.Date(tangleOut$line.gone, '%d-%b-%y')
# if > 3 months btw LDGW & Line gone, add 3 months to LDWG to construct the end window
tangleOut$postDec6moTF <- ifelse((tangleOut$LineGone - tangleOut$LastDatewGear) > days & is.finite(tangleOut$LineGone), TRUE, FALSE)
tangleOut$EndDateWindow <- as.Date('1600-01-01', '%Y-%m-%d')
idx <- which(tangleOut$postDec6moTF == TRUE)
tangleOut$EndDateWindow[idx] <-  tangleOut$LastDatewGear[idx] %m+% months(3)

# Cases where there is no LDWG: add 3 months from the EndDate in these cases
idx <- which(!is.finite(tangleOut$LastDatewGear))
tangleOut$EndDateWindow[idx] <-  tangleOut$EndDate[idx] %m+% months(3)

idx <- which(is.finite(tangleOut$LineGone) & tangleOut$postDec6moTF == FALSE)
tangleOut$EndDateWindow[idx] <- tangleOut$LineGone[idx]
idx <- which(!is.finite(tangleOut$LineGone) & tangleOut$postDec6moTF == FALSE)
tangleOut$EndDateWindow[idx] <- tangleOut$LastDatewGear[idx] %m+% months(3)

###################################
# Set up the pre-detection window(s) for gear animals
tangleOut$StartDateWindow <- as.Date('1600-01-01', '%Y-%m-%d')
tangleOut$wlength <- tangleOut$EndDate - tangleOut$StartDate
tangleOut$predDectWindow3TF <- ifelse(tangleOut$wlength >= 3 * (365.25/12), TRUE, FALSE)

# if greater than 3, make the window start 3 months prior to detection:
tangleOut$StartDateWindow[tangleOut$predDectWindow3TF] <- tangleOut$EndDate[tangleOut$predDectWindow3TF] %m-% months(3)
# if greater than 2 & < 3, make the window start 2 months prior to detection:
tangleOut$StartDateWindow[tangleOut$wlength > 2 * (365.25/12) & tangleOut$wlength <= 3 * (365.25/12)] <- tangleOut$EndDate[tangleOut$wlength > 2 * (365.25/12) & tangleOut$wlength <= 3 * (365.25/12)] %m-% months(2) # 2 months
# if greater than 1 & < 2, make the window start 1 months prior to detection:
tangleOut$StartDateWindow[tangleOut$wlength > 1 * (365.25/12) & tangleOut$wlength <= 2 * (365.25/12)] <- tangleOut$EndDate[tangleOut$wlength > 1 * (365.25/12) & tangleOut$wlength <= 2 * (365.25/12)] %m-% months(1) # 2 months
# if less than 1, make the window start 1 month3 prior to detection:
tangleOut$StartDateWindow[tangleOut$wlength < 1 * (365.25/12)] <- tangleOut$EndDate[tangleOut$wlength < 1 * (365.25/12)]  %m-% months(1) # 1 months
tangleOutAll <- tangleOut # doing this for later on comparison checking, i.e. if I need the comments, etc.



# pare down the columns for merging the gear and non-gear whales
cidx <- c("EGNo", "EventNo", "StartDate", "EndDate", "Severity", "gear", 'LastDatewGear',   'LineGone', "EndDateWindow", "StartDateWindow")
tndat$LastDatewGear <- as.Date('1600-01-01', '%Y-%m-%d')
tndat$LineGone <- as.Date('1600-01-01', '%Y-%m-%d')
ngearsub <- tndat[, which(colnames(tndat) %in% cidx)]
gearsub <- tangleOut[, which(colnames(tangleOut) %in% cidx)]
tangleOut <- rbind(ngearsub, gearsub)

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

# Adding in the first severe entanglement date so that everything after 
# this won't be included in any subsequent health tallies:
tangleOut$firstSevere <- as.Date('2500-01-01', '%Y-%m-%d')
idx <- which(tangleOut$Severity == 'severe')
tangleOut$firstSevere[idx] <- tangleOut$StartDateWindow[idx]

dupid <- tangleOut$EGNo[idx][duplicated(tangleOut$EGNo[idx])]
for (id in dupid) {
  drange <- data.frame(tangleOut[which(tangleOut$Severity == 'severe' & tangleOut$EGNo == id), 'StartDateWindow'])
  tangleOut[which(tangleOut$Severity == 'severe' & tangleOut$EGNo == id), 'firstSevere'] <- dplyr::summarise(drange, mindate = min(StartDateWindow))
}

# ok with that done, now I need to add in the recovery dates 12 months beyond
# EndDateWindow, from Amy Knowlton (April 14, 2015):
# for gear animals this is 12 months beyond either LastDatewGear or LineGone
# for non-gear animals this is 12 months beyond EndDate
tangleOut$recov12months <- as.Date('2500-01-01', '%Y-%m-%d')
tangleOut$recov12months <- tangleOut$EndDateWindow %m+% months(12)

# set a date we'll use for when females become reproductively active, i.e. year of first calf
# remove the reproductively active females from background health
tangleOut$firstCalf <- as.Date('2500-01-01', '%Y-%m-%d')
tangleOut$firstCalfidx <- NA

for (egno in unique(tangleOut$EGNo)) {
  
  if (length(calfTable[calfTable$EGNo == egno, 'CalvingYear']) > 0) {
    
    year1 <- calfTable[calfTable$EGNo == egno, 'CalvingYear'][which.min(calfTable[calfTable$EGNo == egno, 'CalvingYear'])]
    subdate <- as.Date(paste(year1, '-01-01', sep = ''), '%Y-%m-%d')
    tangleOut$firstCalf[tangleOut$EGNo == egno] <- subdate
    tangleOut$firstCalfidx[tangleOut$EGNo == egno] <- match(year1, monYr[, 2])    
    
  }
  
}




# I'm culling out a handful of animals for viewing in the shiny app:
# tcase <- data.frame(egno = c(1027, 1403, 2212, 1247, 1158, 1102, 1113, 1004, 1602, 1301), 
#                     event =c(3, 2, 3, 2, 4, 1, 2, 1, 4, 1))
# idx <- rep(0, length.out = nrow(tcase))
# for(i in 1:nrow(tcase)){
#   idx[i] <- which(tangleOut$EGNo == tcase[i, 1]& tangleOut$EventNo == tcase[i, 2])
# }
# 
# tshiny <- tangleOut[idx, ]
# save(tshiny, file="../inst/shiny-examples/myapp/shinyEntData.rdata")

# have to get the dates pared down to make sense with our time indexing in the main file.
tangleOut$smonyr   <- paste(str_sub(as.character(tangleOut$StartDate), 6, 7), str_sub(as.character(tangleOut$StartDate), 1, 4), sep = '-')
tangleOut$emonyr   <- paste(str_sub(as.character(tangleOut$EndDate), 6, 7), str_sub(as.character(tangleOut$EndDate), 1, 4), sep = '-')
tangleOut$swindmonyr   <- paste(str_sub(as.character(tangleOut$StartDateWindow), 6, 7), str_sub(as.character(tangleOut$StartDateWindow), 1, 4), sep = '-')
tangleOut$ewindmonyr   <- paste(str_sub(as.character(tangleOut$EndDateWindow), 6, 7), str_sub(as.character(tangleOut$EndDateWindow), 1, 4), sep = '-')
tangleOut$fsevmonyr   <- paste(str_sub(as.character(tangleOut$firstSevere), 6, 7), str_sub(as.character(tangleOut$firstSevere), 1, 4), sep = '-')
tangleOut$rec12monyr   <- paste(str_sub(as.character(tangleOut$recov12months), 6, 7), str_sub(as.character(tangleOut$recov12months), 1, 4), sep = '-')


# ws0   <- which(str_locate(str_sub(as.character(tangleOut$StartDate), 6, 7), '0')[, 1] == 1)
# we0   <- which(str_locate(str_sub(as.character(tangleOut$EndDate), 6, 7), '0')[, 1] == 1)
# we120 <- which(str_locate(str_sub(as.character(tangleOut$StartDateWindow), 6, 7), '0')[, 1] == 1)
# we60 <- which(str_locate(str_sub(as.character(tangleOut$EndDateWindow), 6, 7), '0')[, 1] == 1)
# wep60 <- which(str_locate(str_sub(as.character(tangleOut$firstSevere), 6, 7), '0')[, 1] == 1)
# weld60 <- which(str_locate(str_sub(as.character(tangleOut$recov12months), 6, 7), '0')[, 1] == 1)

# tangleOut[ws0, 'smonyr'] <- str_replace(tangleOut[ws0, 'smonyr'], '0', "")
tangleOut$smonyr <- str_replace(tangleOut$smonyr, '0', "")
# tangleOut[we0, 'emonyr'] <- str_replace(as.character(tangleOut[we0, 'emonyr']), '0', "")
tangleOut$emonyr <- str_replace(tangleOut$emonyr, '0', "")
# tangleOut[we120, 'swindmonyr'] <- str_replace(as.character(tangleOut[we120, 'swindmonyr']), '0', "")
tangleOut$swindmonyr <- str_replace(tangleOut$swindmonyr, '0', "")
# tangleOut[we60, 'ewindmonyr'] <- str_replace(as.character(tangleOut[we60, 'ewindmonyr']), '0', "")
tangleOut$ewindmonyr <- str_replace(tangleOut$ewindmonyr, '0', "")
# tangleOut[wep60, 'fsevmonyr'] <- str_replace(as.character(tangleOut[wep60, 'fsevmonyr']), '0', "")
tangleOut$fsevmonyr <- str_replace(tangleOut$fsevmonyr, '0', "")
# tangleOut[weld60, 'rec12monyr'] <- str_replace(as.character(tangleOut[weld60, 'rec12monyr']), '0', "")
tangleOut$rec12monyr <- str_replace(tangleOut$rec12monyr, '0', "")

# I'm splitting this into two data frames in order to make overlays easier
# the logic is that each entanglement event is tested to see if it's before
# or after the date of the first calf. 
# If the event comes before, then it's a NON-Reproductive event
# If the event comes after, then it's a Reproductive event
tangleOut$afterCalf1 <- NA

for (i in 1:nrow(tangleOut)) {
  if (is.na(tangleOut$firstCalfidx[i])) next()
  idx <- match(tangleOut$smonyr[i], myName)
  tangleOut$afterCalf1[i] <- idx > tangleOut$firstCalfidx[i]
}

tangRepro <- tangleOut[which(tangleOut$afterCalf1 == TRUE), ]
tangNonRepro <- tangleOut[-which(tangleOut$afterCalf1 == TRUE), ]

# Save the data into one rdata file
save(tangleOut, tangRepro, tangNonRepro, file="data/egAmyEntData.rdata")
