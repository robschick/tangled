# Coding by Rob Schick, December 4, 2015
# The goal of this is to make the line plots of the health at three points
# swindmonyr
# ewindmonyr
# rec12monyr
# This uses the data (data frame: tangleOut) coming from this script: prepAmyEntanglementData.R

rm(list=ls())
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(multcomp)
library(plyr)
source('/Users/rob/Documents/code/rss10/rightwhales/makeYearmon.r')
load(file = 'data/eg_2015_newData_JUVTRUE__50000_wkspc.rdata')
load(file="data/egAmyEntData.rdata") # egAmyEntData.rdata contains tangleOut, tangRepro, tangNonRepro, so use the repro flag
recover <- TRUE

healthmean <- sumh / g
for(i in 1:nrow(healthmean)){
  healthmean[i, 1:firstSight[i]] <- NA
}

# Need to pare down to animals only with a one year window (Amy Knowlton's suggestion 14 January 2014)
pvec <- tangleOut[, 'EndDate'] - tangleOut[, 'StartDate'] <= 365
tangleOut <- tangleOut[pvec, ]

dfout <- numeric(0)

for(i in 1:nrow(tangleOut)){

  ind <- tangleOut$EGNo[i]
  tsub <- tangleOut[i, ]
  htest <- healthmean[which(ID == ind),]
  
  s <- match(tsub[, 'smonyr'], myName)  
  e <- match(tsub[, 'ewindmonyr'], myName)
  r <- match(tsub[, 'rec12monyr'], myName)
  gstat <- tsub[, 'gearInj']
  
  sVal <- htest[s]
  eVal <- htest[e]
  rVal <- htest[r]
 dfi <- data.frame(egno = ind, startHealth = sVal, endHealth = eVal, recHealth = rVal, gearInjury = gstat) 
 dfout <- rbind(dfout, dfi)
}
library(dplyr)
dfsum <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(shealth = mean(startHealth, na.rm = TRUE), endhealth = mean(endHealth), rechealth = mean(recHealth))

dfCI <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(shealth025 = quantile(startHealth, probs = 0.025, na.rm = TRUE),
            shealth975 = quantile(startHealth, probs = 0.975, na.rm = TRUE),
            ehealth025 = quantile(endHealth, probs = 0.025, na.rm = TRUE),
            ehealth975 = quantile(endHealth, probs = 0.975, na.rm = TRUE),
            rhealth025 = quantile(recHealth, probs = 0.025, na.rm = TRUE),
            rhealth975 = quantile(recHealth, probs = 0.975, na.rm = TRUE))

dfn <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(n = n_distinct(egno))

gvec <- c('Severe Gear', 'Moderate Gear', 'Severe No Gear', 'Minor Gear', 'Moderate No Gear', 'Minor No Gear')
gvec <- factor(gvec, levels = c('Minor No Gear', 'Minor Gear', 'Moderate No Gear', 'Moderate Gear','Severe No Gear','Severe Gear'))

if(recover){
  
  dfplot <- data.frame(rbind(cbind(dfsum$gearInj, dfsum$shealth, rep(1, length(dfsum$gearInj))),
                             cbind(dfsum$gearInj, dfsum$endhealth, rep(2, length(dfsum$gearInj))),
                             cbind(dfsum$gearInj, dfsum$rechealth, rep(3, length(dfsum$gearInj)))))  
  colnames(dfplot) <- c('gearStatus',  'health', 'xval')
  dfplot$gearStatusL <- rep(gvec, times = 3)
  
  } else {
  
  dfplot <- data.frame(rbind(cbind(dfsum$gearInj, dfsum$shealth, rep(1, length(dfsum$gearInj))),
                             cbind(dfsum$gearInj, dfsum$endhealth, rep(2, length(dfsum$gearInj)))))
  colnames(dfplot) <- c('gearStatus',  'health', 'xval')
  dfplot$gearStatusL <- rep(gvec, times = 2)
  
}



if(recover){
  
  name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/slopePlot.pdf' 
  p <- ggplot(dfplot, aes(x = xval, y = health, group = gearStatusL, colour = gearStatusL))+
    geom_line(size = 1.5)+
    scale_color_brewer(type = 'qual', palette = 'Paired')+
    theme_bw(base_size = 16)+
    labs(colour = 'Injury Status', y = 'Estimated Health', x = '')+
    ylim(20, 80)+
    scale_x_continuous(breaks = c(1, 2, 3),
                       labels = c("Sighting Prior to\nEntanglement\nDetection", 
                                  "First Sighting with\nScars or Gear",'Recovery\nAfter\n12 Months'))+
    annotate('text', x = 3.1, y = c(28.4, 67.1, 53.9, 69.3, 74, 75.75), label = dfn$n)
  p
  
} else{
  
  name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/slopePlotNoRecovery.pdf'
  p <- ggplot(dfplot, aes(x = xval, y = health, group = gearStatusL, colour = gearStatusL))+
    geom_line(size = 1.5)+
    scale_color_brewer(type = 'qual', palette = 'Paired')+
    theme_bw(base_size = 16)+
    labs(colour = 'Injury Status', y = 'Estimated Health', x = '')+
    ylim(20, 80)+
    scale_x_continuous(breaks = c(1, 2),
                       labels = c("Sighting Prior to\nEntanglement\nDetection", "First Sighting with\nScars or Gear"))+
    annotate('text', x = 2.05, y = c(31, 69.5, 58, 72, 75, 77), label = dfn$n)
  p
  
}





pdf(file = name, width = 9, height = 9*.61)
print(p)
dev.off()