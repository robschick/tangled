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

# Need to pare down to animals only with a one year window (Amy Knowlton's suggestion 14 January 2016)
pvec <- tangleOut[, 'EndDate'] - tangleOut[, 'StartDate'] <= 365
tangleOut <- tangleOut[pvec, ]
healthmean <- sumh / g
dfout <- numeric(0)

for(i in 1:nrow(tangleOut)){

  ind <- tangleOut$EGNo[i]
  tsub <- tangleOut[i, ]
  htest <- healthmean[which(ID == ind),]
  atest <- anom[which(ID == ind),]
  
  s <- match(tsub[, 'smonyr'], myName)  
  e <- match(tsub[, 'ewindmonyr'], myName)
  r <- match(tsub[, 'rec12monyr'], myName)
  gstat <- tsub[, 'gearInj']
  sVal <- htest[s]
  eVal <- htest[e]
  rVal <- htest[r]
  asVal <- atest[s]
  aeVal <- atest[e]
  arVal <- atest[r]
 
  dfi <- data.frame(egno = ind, startHealth = sVal, endHealth = eVal, recHealth = rVal, 
                    startAnom = asVal, endAnom = aeVal, recAnom = arVal, gearInjury = gstat) 
  dfout <- rbind(dfout, dfi)
}

labels <- data.frame(gearInjury = 1:6, 
                     fullLab = c('Severe Gear', 'Moderate Gear', 'Severe No Gear', 
                                'Minor Gear', 'Moderate No Gear', 'Minor No Gear'),
                     sevLab = c('Severe', 'Moderate', 'Severe', 
                                'Minor', 'Moderate', 'Minor'),
                     gearLab = c('Gear', 'Gear', 'No Gear', 
                              'Gear', 'No Gear', 'No Gear'))

labels$gearLab <- factor(labels$gearLab, levels = c('No Gear', 'Gear'))

dfout <- tbl_df(merge(dfout, labels, by.x = 'gearInj', by.y = 'gearInjury'))
dfout

dfHsum <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(shealth = median(startHealth), shealthSD = sd(startHealth), 
            endhealth = median(endHealth), endhealthSD = sd(endHealth), 
            rechealth = median(recHealth), rechealthSD = sd(recHealth),
            fullLab = unique(fullLab),
            sevLab = unique(sevLab),
            gearLab = unique(gearLab))
dfHsum

dfAsum <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(sAnom = median(startAnom), 
            endAnom = median(endAnom), 
            recAnom = median(recAnom),
            fullLab = unique(fullLab),
            sevLab = unique(sevLab),
            gearLab = unique(gearLab))
dfAsum

dfAsuml <- reshape2::melt(dfAsum, id.vars = c('gearInj', 'fullLab', 'sevLab', 'gearLab'))
dfAsuml$x <- rep(0:2, each = 6)
head(dfAsuml)



dfn <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(n = n_distinct(egno))

p <- ggplot(data = dfout)+
  geom_segment(aes(y = startAnom, yend = endAnom, x = 0, xend = 1), colour = alpha('grey', 0.75)) +
  geom_segment(aes(y = endAnom, yend = recAnom, x = 1, xend = 2), colour = alpha('grey', 0.75))+
  geom_path(data = dfAsuml, aes(y = value, x = x), lwd = 1) +
  # plotting order for annotation label: minor NO (6), moderate NO (5), severe NO (3), 
  # minor gear (4), moderate Gear (2), severe Gear (1)
  annotate('text', x = 0.2, y = -75, size = 5,
           label = c(paste('n = ', dfn[which(dfn$gearInj == 6), 'n'], sep = ''), 
                     paste('n = ', dfn[which(dfn$gearInj == 5), 'n'], sep = ''), 
                     paste('n = ', dfn[which(dfn$gearInj == 3), 'n'], sep = ''), 
                     paste('n = ', dfn[which(dfn$gearInj == 4), 'n'], sep = ''), 
                     paste('n = ', dfn[which(dfn$gearInj == 2), 'n'], sep = ''), 
                     paste('n = ', dfn[which(dfn$gearInj == 1), 'n'], sep = '')))+
    scale_x_continuous(breaks = c(0, 1, 2),
                     labels = c( 'Start', 'End', 'After\n12 Months'))+
  facet_grid(gearLab ~ sevLab)+
  theme_bw()+
  theme(panel.grid.minor.x = element_blank())+
  labs(x = '', y = 'Health Anomaly')
p



# Prepping data for horizontal Bar plot
dfAsuml <- dfAsum %>% 
  group_by(fullLab, sevLab, gearLab) %>% 
  summarise(diff1 = endAnom - sAnom,
            diff2 = recAnom - endAnom) %>% 
reshape2::melt(id.vars = c('fullLab', 'sevLab', 'gearLab'))

p2 <- ggplot(dfAsuml, aes(x = fullLab, y = value, fill = variable)) +
geom_bar(stat = 'identity', position = 'dodge')+
coord_flip()+
  scale_x_discrete(limits = rev(c("Minor No Gear", "Minor Gear", "Moderate No Gear", "Moderate Gear", "Severe No Gear",  "Severe Gear")))+
labs(x = '', y = 'Deviation From Population Health', fill = c('Time Period'))+
scale_fill_brewer(type = 'qual', palette = 'Dark2', direction = -1, labels = c('Entanglement\nWindow', 'Recovery\nWindow'))+
theme_bw()
p2


######################### Final Plot ###########################
name <- paste('/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/slopePlot_', Sys.Date() , '.pdf', sep = '')
pdf(file = name, width = 9, height = 9*.61)
library(gridExtra)
grid.arrange(p, p2, ncol=1, heights = c(0.7,  0.3))
dev.off()