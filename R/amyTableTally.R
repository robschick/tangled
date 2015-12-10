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
# Non repro females
dfout <- numeric(0)

for(i in 1:nrow(tangNonRepro)){
  
  ind <- tangNonRepro$EGNo[i]
  tsub <- tangNonRepro[i, ]
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

nrfm70 <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(n = n(),
            totNumMonths = sum(nmonths),
            maxDurMonths = max(nmonths),
            sumBel70Months = sum(mon70),
            meanp70 = mean(pctmon70))
nrfm70

gname <- c('Severe - gear', 'Moderate - gear', 'Severe - no gear', 
           'Minor - gear', 'Moderate - no gear', 'Minor - no gear')
dfb <- data.frame(gearInj = c(rfm70$gearInj, nrfm70$gearInj),
                  gearName = rep(gname, times = 2),
                  pct70 = c(rfm70$meanp70, nrfm70$meanp70),
                  status = rep(c('Repro Fem', 'Non-Repro Fem'), each = nrow(nrfm70)))


name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/pct70.pdf'
p <- ggplot(dfb, aes(x = gearName, y = pct70, fill = status))+
  geom_bar(position = 'dodge', stat = 'identity')+
  theme_bw(base_size = 18)+
  labs(y = '% of Months w Health < 70', x = 'Severity Category')+
  scale_x_discrete(breaks = c('Minor - no gear', 'Minor - gear', 'Moderate - no gear',
                              'Moderate - gear', 'Severe - no gear', 'Severe - gear'),
                   labels = c('Minor\nNo Gear', 'Minor\nGear',
                              'Moderate\nNo Gear', 'Moderate\nGear',
                              'Severe\nNo Gear', 'Severe\nGear'))+
  labs(fill = 'Reproductive\nStatus')+
  theme(legend.position = c(0.125, 0.875))
p



pdf(file = name, width = 9, height = 9*.61)
print(p)
dev.off()