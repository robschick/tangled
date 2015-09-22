library(dplyr)
rm(list = ls())
load(file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/data/kmcalcInput.rda') # contains survdf
# so known death animals get censored and shown with a tick mark

# Let's start with just 5 animals to work out the maths
nt <- 504
survdf$yearInt <- findInterval(survdf$survTime0, seq(0, nt, by = 12)) # to group the data into yearly summaries
survdf$censYearInt <- survdf$deathyearInt <- findInterval(survdf$deathMonth0, seq(0, nt, by = 12)) # to group the death data into yearly summaries in order to ask if an animal died on a particular interval. We won't need to do this with month survival
# survdf$censYearInt <- findInterval(survdf$censMonth, seq(0, nt, by = 12)) 
# for testing
# idx <- sample(unique(survdf$EGNo), 20)
# dsub <- survdf[survdf$EGNo %in% idx, ]

# All entanglement events
dsub <- survdf
# dtimes <- unique(dsub$deathMonth0) # when the animals died or are censored

kmdf <- data.frame(interval = 0, atRiskStart = nrow(dsub), censored = 0,
                   diedOnInterval = 0, atRiskEnd = 0, propSurv = 1)
for(i in 1:length(unique(dsub$yearInt))){
  dsubi <- dsub[which(dsub$yearInt == i), ]
  interval <- i
  
  atRiskStart <- length(unique(dsubi$EGNo))
  censored <- length(which(!duplicated(dsubi$EGNo) & dsubi$censored == TRUE & dsubi$deathyearInt == interval))
  died <- length(which(!duplicated(dsubi$EGNo) & dsubi$censored == FALSE & dsubi$deathyearInt == interval))
  atRiskEnd <- atRiskStart - (censored + died)
  proportionSurviving <- atRiskEnd / atRiskStart
  
  kmdf <- rbind(kmdf, data.frame(interval = interval, atRiskStart = atRiskStart, censored = censored,
                                 diedOnInterval = died, atRiskEnd = atRiskEnd, propSurv = proportionSurviving))
}
kmdf$psurv <- cumprod(kmdf$propSurv)

# I want a censored month in here as well for plotting purposes
csub <- subset(dsub, censored == TRUE)
csub <- csub[!duplicated(csub$EGNo),]
psvec <- rep(NA, nrow(csub))# to store the death months

for(i in 1:nrow(csub)){
  interval <- csub[i, 'censYearInt']
  psvec[i] <- kmdf[kmdf$interval == (interval - 1), 'psurv']
}

csub$psurv <- psvec

# plot it
pdf(file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveAllEntEvents.pdf', width = 8, height = 6)
ggplot(kmdf, aes(interval, psurv)) + 
  geom_step() + 
  ylim(0, 1) + 
  geom_segment(data = csub, aes(x = deathMonth0 / 12, y = psurv, xend = deathMonth0 / 12, yend = psurv + 0.015)) + 
  labs(y = 'Probability of Survival', x = 'Years Following End of Entanglement')+
  theme_bw()+
  theme(panel.grid.major = element_line(size = 1.5), panel.grid.minor = element_line(size = 1))+
  scale_x_continuous(breaks = seq(0, 30, 5), minor_breaks = seq(0, 30, 1))
dev.off()

# Break it out by entanglement severity:
kmdfAll <- numeric(0)
csubAll <- numeric(0)
for(j in 1:3){
  svvec <- c("moderate", "minor", "severe" )
  dsub <- survdf[survdf$Severity == svvec[j], ]
  kmdf <- data.frame(interval = 0, atRiskStart = nrow(dsub), censored = 0,
                     diedOnInterval = 0, atRiskEnd = 0, propSurv = 1)
  for(i in 1:length(unique(dsub$yearInt))){
    dsubi <- dsub[which(dsub$yearInt == i), ]
    interval <- i
    
    atRiskStart <- length(unique(dsubi$EGNo))
    censored <- length(which(!duplicated(dsubi$EGNo) & dsubi$censored == TRUE & dsubi$deathyearInt == interval))
    died <- length(which(!duplicated(dsubi$EGNo) & dsubi$censored == FALSE & dsubi$deathyearInt == interval))
    atRiskEnd <- atRiskStart - (censored + died)
    proportionSurviving <- atRiskEnd / atRiskStart
    
    kmdf <- rbind(kmdf, data.frame(interval = interval, atRiskStart = atRiskStart, censored = censored,
                                   diedOnInterval = died, atRiskEnd = atRiskEnd, propSurv = proportionSurviving))
  }
  kmdf$psurv <- cumprod(kmdf$propSurv)
  kmdf$sev <- svvec[j]
  kmdfAll <- rbind(kmdfAll, kmdf)
  
  # I want a censored month in here as well for plotting purposes
  csub <- subset(dsub, censored == TRUE)
  csub <- csub[!duplicated(csub$EGNo),]
  psvec <- rep(NA, nrow(csub))# to store the death months
  
  for(i in 1:nrow(csub)){
    interval <- csub[i, 'censYearInt']
    psvec[i] <- kmdf[kmdf$interval == (interval - 1), 'psurv']
  }
  
  csub$psurv <- psvec
  csub$sev <- svvec[j]
  
  csubAll <- rbind(csubAll, csub)
  
} # end loop over severity




pdf(file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveBySeverity.pdf', width = 8, height = 6)
ggplot(kmdfAll, aes(interval, psurv, group = sev, colour = sev)) + 
  geom_step(lwd = 1) + 
  ylim(0, 1) + 
  geom_segment(data = csubAll, aes(x = deathMonth0 / 12, y = psurv, xend = deathMonth0 / 12, yend = psurv + 0.015)) + 
  labs(y = 'Probability of Survival', x = 'Years Following End of Entanglement')+
  theme_bw()+
  theme(panel.grid.major = element_line(size = 1.25), panel.grid.minor = element_line(size = 1))+
  scale_x_continuous(breaks = seq(0, 30, 5), minor_breaks = seq(0, 30, 1), expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_colour_brewer(palette = 'Set2', name = 'Entanglement\nSeverity',
                      labels = c('Minor', 'Moderate', 'Severe'))
dev.off()



# ###################################################################################################
# # this is just a hack to work through the example on this page: http://www.cancerguide.org/scurve_km.html
# #Interval (Start-End)	# At Risk at Start of Interval	# Censored During Interval	# At Risk at End of Interval	# Who Died at End of Interval	Proportion Surviving This Interval	Cumulative Survival at End of Interval
# i1 <- data.frame(interval = 1, atRiskStart = 5, censored = 0, atRiskEnd = 5, died = 0, proportionSurviving = 5 / 5)
# i2 <- data.frame(interval = 2, atRiskStart = 5, censored = 0, atRiskEnd = 5, died = 0, proportionSurviving = 5 / 5)
# i3 <- data.frame(interval = 3, atRiskStart = 5, censored = 0, atRiskEnd = 5, died = 0, proportionSurviving = 5 / 5)
# i4 <- data.frame(interval = 4, atRiskStart = 5, censored = 0, atRiskEnd = 4, died = 1, proportionSurviving = 4 / 5)
# i5 <- data.frame(interval = 5, atRiskStart = 4, censored = 0, atRiskEnd = 4, died = 0, proportionSurviving = 4 / 4)
# i6 <- data.frame(interval = 6, atRiskStart = 4, censored = 0, atRiskEnd = 4, died = 0, proportionSurviving = 4 / 4)
# i7 <- data.frame(interval = 7, atRiskStart = 4, censored = 0, atRiskEnd = 3, died = 1, proportionSurviving = 3 / 4)
# i8 <- data.frame(interval = 8, atRiskStart = 3, censored = 0, atRiskEnd = 3, died = 0, proportionSurviving = 3 / 3)
# df <- rbind(i1, i2, i3, i4, i5, i6, i7, i8)
# df$surv <- cumprod(df$proportionSurviving)
# 
# 
# # Plotting
# ggplot(df, aes(interval, surv)) + 
#   geom_step() + 
#   ylim(0, 1) + 
#   geom_segment(aes(x = 5, y = 0.8, xend = 5, yend = 0.825))