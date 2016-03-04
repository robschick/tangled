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
wd <- '/Users/rob/Documents/research/projects/PCAD/rightwhales/'
setwd(wd)
load(file = paste(wd, 'gibbsoutput/eg_2015_newData_JUVTRUE__50000_wkspc.rdata', sep = '') )
load(file="data/egAmyEntData.rdata")
tSub <- tangleOut

# events <- numeric(0)
# for(egno in unique(tSub$EGNo)){
#   dsub <- subset(tSub, EGNo == egno)
#   dsub <- dsub[which.max(dsub$EventNo),]
#   events <- rbind(events, dsub)
# }
# events <- events[order(events$EGNo),]

# for seom reason, two of these contain duplicated info: 1249 & 1980, so I'm going to remove one of them
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
for(i in 1:length(unique(events$EGNo))){
  id <- which(ID == unique(events$EGNo)[i])
  dsub <- deathyr[id, ]
  events$dtime[i] <- which.max(dsub)
  if(any(dsub == 50000) & events$dtime[i] < nt){
    events$knownD[i] <- TRUE
  }
}
events <- mutate(events, 
                 emonyrID = match(emonyr, myName),
                 ldwgmonyrID = match(ldwgmonyr, myName))
# table(events$knownD)

survl <- vector(mode = 'list', nrow(events))

for(i in 1:nrow(events)){
  id <- events[i, 'EGNo']
  dmonth <- as.numeric(events[i, 'dtime'])
  if(is.na(events[i, 'ldwgmonyrID'])) {emonth <- as.numeric(events[i, 'emonyrID'])
  } else {
    emonth <- as.numeric(events[i, 'ldwgmonyrID'])  
  }
  
  kd <- data.frame(events[i, 'knownD'])
  censor <- ifelse(dmonth > nt, TRUE, FALSE)
  cmonth <- dmonth
  dmonth2 <- dmonth 
  svec <- seq(emonth, dmonth2)
  svec0 <- svec - min(svec)
  survl[[i]] <- data.frame(EGNo = id, deathMonth = dmonth, censored = censor, censMonth = cmonth, survTime0 = svec0, 
                                     deathMonth0 = max(svec0), severity = events[i, 'Severity'], sevNumClass = events[i ,'gearInj'],
                                     knownDeath = kd)
}
# surdvf <- do.call('rbind', survl)
survdf <- as.data.frame(data.table::rbindlist(survl))

survdf$censMonth[survdf$censMonth < nt] <- NA
save(survdf, ID, gender, file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/data/kmcalcInput.rda')

# df <- events # as compared to dfsub, this includes all animals 
# df$dtime <- ifelse(df$dtime > nt, nt, df$dtime)
# df <- df[df$knownD == FALSE,]
# df$emonyrID <- match(df$emonyr, myName)
# df$ldwgmonyrID <- match(df$ldwgmonyr, myName)
# df$m2die <- ifelse(!is.na(df$ldwgmonyrID), df$dtime - df$ldwgmonyrID, df$dtime - df$emonyrID)
# dfsum <- data.frame(label = c('Severe - gear', 'Moderate - gear', 'Severe - no gear', 'Minor - gear', 'Moderate - no gear', 'Minor - no gear'),
#                     medianMonths = floor(aggregate(df$m2die, list(df$gearInj), median, na.rm = TRUE))[, 2], 
#                     sdMonths = floor(aggregate(df$m2die, list(df$gearInj), sd, na.rm = TRUE))[, 2])
# write.csv(dfsum, file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/data/timeAlive.csv', row.names = FALSE)
# 
# 
# dfsub <- events[events$dtime < nt,] # as compared to df, this includes all animals presumed dead before nt
# dfsub <- dfsub[dfsub$knownD == FALSE,]
# dfsub$emonyrID <- match(dfsub$emonyr, myName)
# dfsub$ldwgmonyrID <- match(dfsub$ldwgmonyr, myName)
# dfsub$m2die <- ifelse(!is.na(dfsub$ldwgmonyrID), dfsub$dtime - dfsub$ldwgmonyrID, dfsub$dtime - dfsub$emonyrID)
# dfsubsum <- data.frame(label = c('Severe - gear', 'Moderate - gear', 'Severe - no gear', 'Minor - gear', 'Moderate - no gear', 'Minor - no gear'),
#                        medianMonths = floor(aggregate(dfsub$m2die, list(dfsub$gearInj), median, na.rm = TRUE))[, 2], 
#                        sdMonths = floor(aggregate(dfsub$m2die, list(dfsub$gearInj), sd, na.rm = TRUE))[, 2])
# write.csv(dfsubsum, file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/data/time2Death.csv', row.names = FALSE)
# 
# # # Plot out the distributions
# pdf(file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/time2die.pdf', width = 9.4, height = 5.9)
# x <- seq(-4, 4, length=10000)
# par(xpd = FALSE)
# plot(x, xlim=c(0, 250), ylim=c(1, 6), xlab='Time until Death (Years)', ylab="", type="n", las = 1, bty = 'n', axes = FALSE)
# axis(side = 2, at = seq(6), 
#      labels = c('Severe\n Gear', 'Moderate\nGear', 'Severe\nNo Gear', 
#                 'Minor\nGear', 'Moderate\nNo Gear', 'Minor\nNo Gear'), las = 1, cex.axis = .75)
# axis(side = 1, at = seq(0, 20 * 12, by = 12), labels = seq(0, 20 * 12, by = 12) / 12, cex.axis = .65) # x axis
# abline(v = seq(12, 20 * 12, by = 12), col = 'grey80')
# abline(v = seq(60, 20 * 12, by = 60), col = 'grey40')
# denstrip(x = dfsub$m2die[dfsub$gearInj == 1], at = 1, ticks = median(dfsub$m2die[dfsub$gearInj == 1]), twd = 3)
# denstrip(x = dfsub$m2die[dfsub$gearInj == 2], at = 2, ticks = median(dfsub$m2die[dfsub$gearInj == 2]), twd = 3)
# denstrip(x = dfsub$m2die[dfsub$gearInj == 3], at = 3, ticks = median(dfsub$m2die[dfsub$gearInj == 3]), twd = 3)
# denstrip(x = dfsub$m2die[dfsub$gearInj == 5], at = 5, ticks = median(dfsub$m2die[dfsub$gearInj == 5]), twd = 3)
# denstrip(x = dfsub$m2die[dfsub$gearInj == 6], at = 6, ticks = median(dfsub$m2die[dfsub$gearInj == 6]), twd = 3)
# dev.off()
# 
# pdf(file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/time2dieAll.pdf', width = 9.4, height = 5.9)
# plot(x, xlim=c(0, 250), ylim=c(1, 6), xlab='Time (Years)', ylab="", type="n", las = 1, bty = 'n', axes = FALSE)
# axis(side = 2, at = seq(6), 
#      labels = c('Severe\n Gear', 'Moderate\nGear', 'Severe\nNo Gear', 
#                 'Minor\nGear', 'Moderate\nNo Gear', 'Minor\nNo Gear'), las = 1, cex.axis = .75)
# axis(side = 1, at = seq(0, 20 * 12, by = 12), labels = seq(0, 20 * 12, by = 12) / 12, cex.axis = .65) # x axis
# abline(v = seq(12, 20 * 12, by = 12), col = 'grey80')
# abline(v = seq(60, 20 * 12, by = 60), col = 'grey40')
# denstrip(x = df$m2die[df$gearInj == 1], at = 1, ticks = median(df$m2die[df$gearInj == 1]), twd = 3)
# denstrip(x = df$m2die[df$gearInj == 2], at = 2, ticks = median(df$m2die[df$gearInj == 2]), twd = 3)
# denstrip(x = df$m2die[df$gearInj == 3], at = 3, ticks = median(df$m2die[df$gearInj == 3]), twd = 3)
# denstrip(x = df$m2die[df$gearInj == 3], at = 4, ticks = median(df$m2die[df$gearInj == 4]), twd = 3)
# denstrip(x = df$m2die[df$gearInj == 5], at = 5, ticks = median(df$m2die[df$gearInj == 5]), twd = 3)
# denstrip(x = df$m2die[df$gearInj == 6], at = 6, ticks = median(df$m2die[df$gearInj == 6]), twd = 3)
# dev.off()
# 
# # summary statistics
# summary(glm(dfsub$m2die ~ factor(dfsub$gearInj)))
# summary(glm(df$m2die ~ factor(df$gearInj)))
