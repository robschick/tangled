rm(list = ls())
library(dplyr)


load(file = 'data/kmcalcInput.rda') # canonical data for making the median curves; 
# contains: survdf, kdpasurvldf, ID, gender, dcut, myName
canonsurvdf <- survdf
load(file="data/events.rda") # use this to call the data presDeadtime.R
load(file = 'data/deathyr.rda') # contains deathyr
nt <- dcut

nboot <- 10
kmlines <- vector(mode = 'list', (nboot + 1)) # idea of the dimension is to put the median value in the first list slot
censTicks <- vector(mode = 'list', (nboot + 1))

for(nb in 1:(nboot + 1)) {
  
  if (nb == 1) {
    survdf <- canonsurvdf
  } else {
    deathSamp <- getDeaths(deathyr)
    enew <- presDeadsurvdat(events, dcut, deathSamp)
    survdf <- rbind(enew, kdpasurvldf)  
  }
  
  survdf$yearInt <- findInterval(survdf$survTime0, seq(0, nt, by = 12)) # to group the data into yearly summaries
  survdf$censYearInt <- findInterval(survdf$censMonth0, seq(0, nt, by = 12)) # to group the death data into yearly 
  survdf$deathyearInt <- findInterval(survdf$deathMonth0, seq(0, nt, by = 12)) # to group the death data into yearly 
  dsub <- survdf
  
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
  kmlines[[nb]] <- kmdf
  
  # I want a censored month in here as well for plotting purposes, i.e. to make the ticks
  csub <- subset(dsub, censored == TRUE)
  csub <- csub[!duplicated(csub$EGNo),]
  psvec <- rep(NA, nrow(csub))# to store the death months
  
  for(i in 1:nrow(csub)){
    interval <- csub[i, 'censYearInt']
    psvec[i] <- kmdf[kmdf$interval == (interval - 1), 'psurv']
  }
  
  csub$psurv <- psvec
  censTicks[[nb]] <- csub
} # end bootstrap interval







# Break it out by entanglement severity & Gender:
# Get the gender in using ID and Gender
survdf$gender <- gender[match(survdf$EGNo, ID)]
kmdfAll <- numeric(0)
csubAll <- numeric(0)
for(j in 1:3){
  g <- 'M'
  svvec <- c("moderate", "minor", "severe" )
  dsub <- survdf[survdf$Severity == svvec[j] & survdf$gender == g, ]
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

oddsR <- kmdfAll$psurv[kmdfAll$sev == 'severe'][1:15] / kmdfAll$psurv[kmdfAll$sev == 'moderate'][1:15]
ifelse(g == 'F', oddsF <- oddsR, oddsM <- oddsR)

