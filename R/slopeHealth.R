# Coding by Rob Schick, December 4, 2015
# The goal of this is to make the line plots of the health at three points
# swindmonyr
# ewindmonyr
# rec12monyr
# This uses the data (data frame: tangleOut) coming from this script: prepAmyEntanglementData.R
# 
# Update - February 4, 2016
# I'm remaking this as a six panel faceted plot using the health anomaly, not actual health

rm(list=ls())
library(ggplot2)
library(dplyr)

load(file = 'data/eg_2015_newData_JUVTRUE__50000_wkspc.rdata')
load(file = 'data/healthAnomaly.rda') # contains 'anom' which is deviation from pop health (Adult males and juveniles) for all animals
load(file="data/egAmyEntData.rdata") # egAmyEntData.rdata contains tangleOut, tangRepro, tangNonRepro, so use the repro flag

healthmean <- anom


# Need to pare down to animals only with a one year window (Amy Knowlton's suggestion 14 January 2016)
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

labels <- data.frame(gearInjury = 1:6, 
                     fullLab = c('Severe Gear', 'Moderate Gear', 'Severe No Gear', 
                                'Minor Gear', 'Moderate No Gear', 'Minor No Gear'),
                     sevLab = c('Severe', 'Moderate', 'Severe', 
                                'Minor', 'Moderate', 'Minor'),
                     gearLab = c('Gear', 'Gear', 'No Gear', 
                              'Gear', 'No Gear', 'No Gear'))

dfout <- tbl_df(merge(dfout, labels, by.x = 'gearInj', by.y = 'gearInjury'))
dfout

dfsum <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(shealth = mean(startHealth, na.rm = TRUE), shealthSD = sd(startHealth, na.rm = TRUE), 
            endhealth = mean(endHealth), endhealthSD = sd(endHealth), 
            rechealth = mean(recHealth), rechealthSD = sd(recHealth))
dfsum

# dfCI <- dfout %>% 
#   group_by(gearInj) %>% 
#   summarise(shealth025 = quantile(startHealth, probs = 0.025, na.rm = TRUE),
#             shealth975 = quantile(startHealth, probs = 0.975, na.rm = TRUE),
#             ehealth025 = quantile(endHealth, probs = 0.025, na.rm = TRUE),
#             ehealth975 = quantile(endHealth, probs = 0.975, na.rm = TRUE),
#             rhealth025 = quantile(recHealth, probs = 0.025, na.rm = TRUE),
#             rhealth975 = quantile(recHealth, probs = 0.975, na.rm = TRUE))

dfn <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(n = n_distinct(egno))

gvec1 <- c('Gear', 'Gear', 'No Gear', 'Gear', 'No Gear', 'No Gear')
gvec1 <- factor(gvec1, levels = c('No Gear', 'Gear'))
gvec2 <- c('Severe', 'Moderate', 'Severe', 'Minor', 'Moderate', 'Minor')
gvec3 <- c('Severe Gear', 'Moderate Gear', 'Severe No Gear', 'Minor Gear', 'Moderate No Gear', 'Minor No Gear')
gvec3 <- factor(gvec, levels = c('Minor No Gear', 'Minor Gear', 'Moderate No Gear', 'Moderate Gear','Severe No Gear','Severe Gear'))

dfsum$gearLab1 <- gvec1
dfsum$gearLab2 <- gvec2
dfsum$gearLab3 <- gvec3

dfplot <- data.frame(rbind(cbind(dfsum$gearInj, dfsum$shealth, rep(1, length(dfsum$gearInj))),
                           cbind(dfsum$gearInj, dfsum$endhealth, rep(2, length(dfsum$gearInj))),
                           cbind(dfsum$gearInj, dfsum$rechealth, rep(3, length(dfsum$gearInj)))))  
colnames(dfplot) <- c('gearStatus',  'health', 'xval')
dfplot$gearStatusL <- rep(gvec, times = 3)
  

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

name <- paste('/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/slopePlot_', Sys.Date() , '.pdf', sep = '')
pdf(file = name, width = 9, height = 9*.61)
print(p)
dev.off()