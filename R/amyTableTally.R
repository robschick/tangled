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

# calcaluating months below 70
healthmean <- sumh / g
for(i in 1:nrow(healthmean)){
  healthmean[i, 1:firstSight[i]] <- NA
}

# this will be for the unimpacted calving females:
healthurf <- sumh / g 
for(i in 1:nrow(healthurf)){
  healthurf[i, 1:firstSight[i]] <- NA
  healthurf[i, lastSight[i]:nt] <- NA
}

# for females are in calf table but not in tangleOut I want stretches of health and after or including the first pregnancy year 
# I'll find the females in calfTable, and not in tangleOut
idx <- match(calfTable$EGNo, tangleOut$EGNo)
ctabSub <- calfTable[which(is.na(idx)), ]
ctabid <- ctabSub %>% 
  group_by(EGNo) %>% 
  summarise(pregyear = min(CalvingYear) - 2)
ctabid <- ctabid[-which(ctabid$EGNo == 1045),]

ctabid$monyr <- paste('12-', ctabid$pregyear, sep = '')# this gets the December before the pregnancy year

# pare down the heatlh data for these criteria
# essentially all animals that are not in this category are set to NA
# And then for the animals that are in the category, any times before their first pregnancy year are set to NA
idx <- match(ID, ctabid$EGNo)
healthnew <- healthurf[which(is.finite(idx)),]
cID <- ID[which(is.finite(idx))]

for(i in 1:nrow(ctabid)){
  iint <- match(ctabid$monyr[i], myName)
  healthnew[cID == ctabid$EGNo[i], 1:iint] <- NA  
}

# Then tally the number of months below 70
nmon <- length(which(is.finite(healthnew)))
nmon70 <- length(which(healthnew < 70))
p70nmon <- (nmon70 / nmon) * 100
  
dfout <- numeric(0)

for(i in 1:nrow(tangleOut)){
  
  ind <- tangleOut$EGNo[i]
  tsub <- tangleOut[i, ]
  htest <- healthmean[which(ID == ind),]
  
  s <- match(tsub[, 'swindmonyr'], myName)  
  e <- match(tsub[, 'ewindmonyr'], myName)
  
  gstat <- tsub[, 'gearInj']
  
  hVal <- htest[s:e] # this gets all the health values, but will be ragged
  lh <- length(hVal)
  l70 <- length(which(hVal < 70))
  p70 <- (l70 / lh ) * 100
  
  dfi <- data.frame(egno = ind, nmonths = lh, mon70 = l70, pctmon70 = p70, gearInjury = gstat) 
  dfout <- rbind(dfout, dfi)
}

m70 <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(n = n(),
            totNumMonths = sum(nmonths),
            maxDurMonths = max(nmonths),
            sumBel70Months = sum(mon70),
            meanp70 = mean(pctmon70))
m70


##################################################
# repro females
dfout <- numeric(0)

for(i in 1:nrow(tangRepro)){
  
  ind <- tangRepro$EGNo[i]
  tsub <- tangRepro[i, ]
  htest <- healthmean[which(ID == ind),]
  
  s <- match(tsub[, 'swindmonyr'], myName)  
  e <- match(tsub[, 'ewindmonyr'], myName)
  
  gstat <- tsub[, 'gearInj']
  
  hVal <- htest[s:e] # this gets all the health values, but will be ragged
  lh <- length(hVal)
  l70 <- length(which(hVal < 70))
  p70 <- (l70 / lh ) * 100
  
  dfi <- data.frame(egno = ind, nmonths = lh, mon70 = l70, pctmon70 = p70, gearInjury = gstat) 
  dfout <- rbind(dfout, dfi)
}

rfm70 <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(n = n(),
            totNumMonths = sum(nmonths),
            maxDurMonths = max(nmonths),
            sumBel70Months = sum(mon70),
            meanp70 = mean(pctmon70))
rfm70



##################################################
# # Non repro females
# dfout <- numeric(0)
# 
# for(i in 1:nrow(tangNonRepro)){
#   
#   ind <- tangNonRepro$EGNo[i]
#   tsub <- tangNonRepro[i, ]
#   htest <- healthmean[which(ID == ind),]
#   
#   s <- match(tsub[, 'swindmonyr'], myName)  
#   e <- match(tsub[, 'ewindmonyr'], myName)
#   
#   gstat <- tsub[, 'gearInj']
#   
#   hVal <- htest[s:e] # this gets all the health values, but will be ragged
#   lh <- length(hVal)
#   l70 <- length(which(hVal < 70))
#   p70 <- (l70 / lh ) * 100
#   
#   dfi <- data.frame(egno = ind, nmonths = lh, mon70 = l70, pctmon70 = p70, gearInjury = gstat) 
#   dfout <- rbind(dfout, dfi)
# }
# 
# nrfm70 <- dfout %>% 
#   group_by(gearInj) %>% 
#   summarise(n = n(),
#             totNumMonths = sum(nmonths),
#             maxDurMonths = max(nmonths),
#             sumBel70Months = sum(mon70),
#             meanp70 = mean(pctmon70))
# nrfm70
# # Non repro females
##################################################


gname <- c('Severe - gear', 'Moderate - gear', 'Severe - no gear', 
           'Minor - gear', 'Moderate - no gear', 'Minor - no gear')
dfb <- data.frame(gearInj = c(rfm70$gearInj, 0),
                  gearName = c(gname, 'Unimpacted'),
                  pct70 = c(rfm70$meanp70, p70nmon),
                  nmon = c(rfm70$sumBel70Months, nmon70),
                  status = c(rep('Repro Fem', times = nrow(rfm70)), 'Unimpacted Repro Fem'))


name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/pct70.pdf'
p <- ggplot(dfb, aes(x = gearName, y = pct70, fill = status))+
  geom_bar(stat = 'identity')+
  geom_text(aes(y = pct70, label = dfb$nmon), vjust = -0.3)+
  theme_bw(base_size = 18)+
  labs(y = '% of Months w Health < 70', x = 'Severity Category')+
  scale_x_discrete(breaks = c('Minor - no gear', 'Minor - gear', 'Moderate - no gear',
                              'Moderate - gear', 'Severe - no gear', 'Severe - gear', 'Unimpacted'),
                   labels = c('Minor\nNo Gear', 'Minor\nGear',
                              'Moderate\nNo Gear', 'Moderate\nGear',
                              'Severe\nNo Gear', 'Severe\nGear', 'Unimpacted'))+
  labs(fill = 'Reproductive\nStatus')+
  theme(legend.position = c(0.175, 0.875))
p



pdf(file = name, width = 9, height = 9*.61)
print(p)
dev.off()