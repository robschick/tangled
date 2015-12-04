# Coding by Rob Schick, May 6, 2015
# The goal of this is to make the box plots of the health during the entanglement window
# This uses the data (data frame: tangleOut) coming from this script: prepAmyEntanglementData.R

rm(list=ls())
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(stringr)
library(plyr)
source('/Users/robs/Dropbox/code/rightwhales/makeYearmon.r')
load(file = '../data/eg_2015_newData_JUVTRUE__50000_wkspc.rdata')
load(file="../data/egAmyEntData.rdata") 
load(file = '../data/unimpacted.rdata')
# egAmyEntData.rdata contains tangleOut, tangRepro, tangNonRepro, so use the repro flag
repro <- TRUE
if (repro) {
  tSub <- tangRepro 
} else {
  tSub <- tangNonRepro
}  


  
healthmean <- sumh / g
for(i in 1:nrow(healthmean)){
  healthmean[i, 1:firstSight[i]] <- NA
}

dfSum <- matrix(NA, nrow = length(unique(tSub$EGNo)), ncol = 9)
colnames(dfSum) <- c('ID', 'Severe - 1', 'Severe - 0', 'Moderate - 1', 
                     'Moderate - 0', 'Minor - 1', 'Minor - 0', 'None', 'Post-Reprod')
idFac <- factor(unique(tSub$EGNo))

for(i in 1:length(unique(tSub$EGNo))){
  # i <- 32 #, EGNo == 1130 is a good test animal; i <- 390 is another (EGNo = 1102)
  ind <- unique(tSub$EGNo)[i]
  if(!ind %in% ID){next()}
  htest <- healthmean[which(ID == ind),]
  ti <- tSub[tSub$EGNo == ind,]
  
  # Assemble the vector of integers for the duration of different gear injury combinations
  # asking for the start and end of the health window during which I'll calculate health
  # also for the date of first severe entanglement & the the 12 month recovery date
  evnum <- nrow(ti)
  s <- match(tSub[tSub$EGNo == ind, 'swindmonyr'], myName)  
  e <- match(tSub[tSub$EGNo == ind, 'ewindmonyr'], myName)
  sev <- match(tSub[tSub$EGNo == ind, 'fsevmonyr'], myName)
  sev <- sev[is.finite(sev)]
  tfac <- rep(NA, length.out = length(myName))
  
  for(j in 1:evnum){
  
      tfac[s[j]:e[j]] <- ti[j, 'gearInj'] 
    
  }
  
#   # Find the times before a severe entanglement or death that are NA and populate them with a state
#   if (length(which(tfac == 1 | tfac == 3)) == 0) {    
#     if (death[which(ID == ind)] < nt) {
#       idx <- which(is.na(tfac[1:death[which(ID == ind)]]))  
#     }
#   } else {
#     idx <- which(is.na(tfac[1:sev]))  
#   }
#   
#   tfac[idx] <- 7 # unentangled state(s) before a severe entanglement, i.e. UNIMPACTED
  
#   if (ti$EGNo %in% dfmyears$id) {
#     idx <- dfmyears[dfmyears$id == ti$EGNo, 'tidx']
#     tfac[idx:nt] <- 9 # post-first reproduction
#   }
  
  
  # then extract the health values for these categories
  tSum <- tapply(htest, tfac, median, na.rm = T)  
  dfSum[i, 1] <- ind
  
  if ('1' %in% names(tSum) ) {# severe with gear
    dfSum[i, 2] <- tSum[which(names(tSum) == '1')]  
  }
  
  if ('3' %in% names(tSum)) {# severe without gear
    dfSum[i, 3] <- tSum[which(names(tSum) == '3')]  
  }
  
  if ('2' %in% names(tSum)) {# moderate with gear
    dfSum[i, 4] <- tSum[which(names(tSum) == '2')]  
  }

  if ('5' %in% names(tSum)) {# moderate without gear
    dfSum[i, 5] <- tSum[which(names(tSum) == '5')]  
  }
  
    if ('4' %in% names(tSum)) {# minor with gear
    dfSum[i, 6] <- tSum[which(names(tSum) == '4')]  
  }
  
  if ('6' %in% names(tSum)) {# minor without gear
    dfSum[i, 7] <- tSum[which(names(tSum) == '6')]  
  }
  
  
}

dfDat <- data.frame(ID = idFac, dfSum[,2:8])
dfLong <- melt(dfDat, na.rm = TRUE)

# dfLong$gearnogear <- 0
# sev1idx <- which(dfLong$variable == 'Severe...1')
# mod1idx <- which(dfLong$variable == 'Moderate...1')
# min1idx <- which(dfLong$variable == 'Minor...1')
# dfLong[sev1idx, 'gearnogear'] <- 1
# dfLong[mod1idx, 'gearnogear'] <- 1
# dfLong[min1idx, 'gearnogear'] <- 1

# add in the unimpacted animals - using data coming from the splitpopHealth.R function
if (repro) {
  uvec <- repvec
} else {
  uvec <- nonrepvec
}  
dfuvec <- data.frame(ID = 9999,
                     variable = 'unimpacted',
                     value = uvec)
dfLong <- rbind(dfLong, dfuvec)

dfLong$group <- 'severe'
mod0idx <- which(dfLong$variable == 'Moderate...0' | dfLong$variable == 'Moderate...1' )
min0idx <- which(dfLong$variable == 'Minor...0' | dfLong$variable == 'Minor...1')
unimpactedidx <- which(dfLong$variable == 'unimpacted')
dfLong[c(mod0idx), 'group'] <- 'moderate'
dfLong[c(min0idx), 'group'] <- 'minor'
dfLong[unimpactedidx, 'group'] <- 'unimpacted'

save(dfLong, file = '../data/healthEntanglementWindowDataReproFems.rdata')

summary(glm(dfLong$value ~ factor(dfLong$group)))

# run the glm 
# first relevel so unimpacted is the reference level.
dfglm <- transform(dfLong, group = factor(group))
dfglm <- within(dfglm, group <- relevel(group, ref = 'unimpacted'))

ft1 <- glm(value ~ group, data = dfglm)
summary(ft1)
summary(glht(ft1, mcp(group = "Tukey")))

nvals <- as.vector(table(dfLong$group))
plabel <- c(nvals[2],nvals[3], nvals[4])
name <- '/Users/robs/Dropbox/Papers/KnowltonEtAl_Entanglement/images/BoxPlotHealthByEntglClass_ReproFemales.pdf'  

p <- ggplot(dfLong, aes(x = factor(group), y = value)) +
    geom_boxplot(aes(group = paste(group)), outlier.shape=NA, notch = FALSE)+
    annotate('text', x = c(2, 3, 4), y = 92.5, 
             label = plabel, cex = 3)+
    labs(y = 'Estimated Health', x = 'Injury Status')+
    scale_x_discrete(labels = c('Unimpacted', 'Minor', 'Moderate', 'Severe'))+
    theme_bw()
p



pdf(file = name, width = 8, height = 6)
  print(p)
dev.off()



