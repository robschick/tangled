#' Plots of the Kaplan-Meier survival curves.
#'

# plot it
pdf(file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveAllEntEvents.pdf', width = 9, height = 9*.61)
ggplot(kmdf, aes(interval, psurv)) + 
  geom_step(lwd = 1, colour = grey(0.5)) + 
  ylim(0, 1) + 
  geom_segment(data = csub, aes(x = deathMonth0 / 12, y = psurv, xend = deathMonth0 / 12, yend = psurv + 0.015)) + 
  labs(y = 'Probability of Survival', x = 'Years Following End of Entanglement')+
  theme_bw()+
  theme(panel.grid.major = element_line(size = 1.25), panel.grid.minor = element_line(size = 1))+
  # scale_x_continuous(breaks = seq(0, 30, 1), minor_breaks = seq(0, 30, 1), expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0.05))+
  xlim(0, 7)
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

# Get the gender in using ID and Gender
csubAll$gender <- gender[match(csubAll$EGNo, ID)]


pdf(file = '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveBySeverity.pdf', width = 9, height = 9*.61)
ggplot(kmdfAll, aes(interval, psurv, group = sev, colour = sev)) + 
  geom_step(lwd = 1.5) + 
  ylim(0, 1) + 
  geom_segment(data = csubAll, aes(x = deathMonth0 / 12, y = psurv, xend = deathMonth0 / 12, yend = psurv + 0.015)) + 
  labs(y = 'Survivorship', x = 'Years Following End of Entanglement')+
  theme_bw(base_size = 18)+
  theme(panel.grid.major = element_line(size = 1.25), panel.grid.minor = element_line(size = 1))+
  # scale_x_continuous(breaks = seq(0, 30, 5), minor_breaks = seq(0, 30, 1), expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0.05))+
  scale_colour_brewer(palette = 'Set2', name = 'Entanglement\nSeverity',
                      labels = c('Minor', 'Moderate', 'Severe'))+
  theme(legend.position = c(.15, .2))+
  coord_cartesian(xlim = c(0, 7.5))
# facet_grid(. ~ gender)
dev.off()



name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveBySeverityGenderM.pdf'
if(g == 'F') {name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalCurveBySeverityGenderF.pdf'}
pdf(file = name, width = 9, height = 9)
ggplot(kmdfAll, aes(interval, psurv, group = sev, colour = sev)) + 
  geom_step(lwd = 1.5) + 
  ylim(0, 1) + 
  geom_segment(data = csubAll, aes(x = deathMonth0 / 12, y = psurv, xend = deathMonth0 / 12, yend = psurv + 0.015)) + 
  labs(y = 'Survivorship', x = 'Years Following End of Entanglement')+
  theme_bw(base_size = 18)+
  theme(panel.grid.major = element_line(size = 1.25), panel.grid.minor = element_line(size = 1))+
  # scale_x_continuous(breaks = seq(0, 30, 5), minor_breaks = seq(0, 30, 1), expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0.05))+
  scale_colour_brewer(palette = 'Set2', name = 'Entanglement\nSeverity',
                      labels = c('Minor', 'Moderate', 'Severe'))+
  theme(legend.position = c(.15, .2))+
  coord_cartesian(xlim = c(0, 17.5))+
  facet_grid(. ~ gender)
dev.off()

oddsAll <- c(oddsF, oddsM)
oddAll <- data.frame(oddRatio = oddsAll, interval = 1:15, gender = rep(c('F', 'M'), each = 15))

name <- '/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/survivalOddsRatio.pdf'
pdf(file = name, width = 9, height = 9*.61)
ggplot(oddAll, aes(x = interval, y = oddRatio, colour = gender))+
  labs(y = 'Severe / Moderate Survivorship', x = 'Years Following End of Entanglement')+
  geom_line(lwd = 1.5)+
  theme_bw(base_size = 18)+
  scale_x_continuous(breaks = seq(0, 15, 5), minor_breaks = seq(0, 15, 1), expand = c(0, 0))+
  theme(legend.position = c(0.9, 0.85))+
  scale_colour_brewer(palette = 'Set2', name = 'Sex',
                      labels = c('Female', 'Male'))
dev.off()