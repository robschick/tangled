# Coding by Rob Schick, May 6, 2015
# The goal of this is to make the box plots of the health during the entanglement window
# This uses the data (data frame: tangleOut) coming from this script: prepAmyEntanglementData.R

rm(list=ls())
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(multcomp)
library(plyr)
library(dplyr)
source('/Users/rob/Documents/code/rss10/rightwhales/makeYearmon.r')
load(file = 'data/eg_2015_newData_JUVTRUE__50000_wkspc.rdata')
load(file = 'data/healthAnomaly.rda') # contains 'anom' which is deviation from pop health (Adult males and juveniles) for all animals
load(file="data/egAmyEntData.rdata") # egAmyEntData.rdata contains tangleOut, tangRepro, tangNonRepro, so use the repro flag
load(file = 'data/unimpacted.rdata') # contains "nonrepvec" and "repvec"  
useAnom <- TRUE

for(z in 1:2){
  ifelse(z == 1, repro <- TRUE, repro <- FALSE)



if (repro) {
  tSub <- tangRepro 
} else {
  tSub <- tangNonRepro
}  

if (useAnom){
  healthmean <- anom
} else {
  healthmean <- sumh / g
  for(i in 1:nrow(healthmean)){
    healthmean[i, 1:(firstSight[i] - 1)] <- NA
  }  
}


dfSum <- data.frame(egno = rep(NA, times = nrow(tSub)), 
                    eventNo = rep(NA, times = nrow(tSub)),
                    nMonths = rep(NA, times = nrow(tSub)),
                    hAnom = rep(NA, times = nrow(tSub)),
                    gearInj = rep(NA, times = nrow(tSub)))
# colnames(dfSum) <- c('ID', 'Severe - 1', 'Severe - 0', 'Moderate - 1', 
#                      'Moderate - 0', 'Minor - 1', 'Minor - 0', 'None', 'Post-Reprod', 'repStatus')
idFac <- factor(unique(tSub$EGNo))

for(i in 1:length(tSub$EGNo)){
  # i <- 32 #, EGNo == 1130 is a good test animal; i <- 390 is another (EGNo = 1102) (Both for Non-Repro)
  # i = 3 # EGNo == 1014 for Repro
  ind <- tSub$EGNo[i]
  eventNo <- tSub$EventNo[i]
  if(!ind %in% ID){next()}
  htest <- healthmean[which(ID == ind),]
  ti <- tSub[tSub$EGNo == ind,]
  
  # Assemble the vector of integers for the duration of different gear injury combinations
  # asking for the start and end of the health window during which I'll calculate health
  # also for the date of first severe entanglement & the the 12 month recovery date
  evnum <- nrow(ti)
  s <- match(t(tSub[i, 'swindmonyr']), myName)  
  e <- match(t(tSub[i, 'ewindmonyr']), myName)
  sev <- match(t(tSub[i, 'fsevmonyr']), myName)
  sev <- sev[is.finite(sev)]
  gstat <- tSub[i, 'gearInj']
  hind <- htest[s:e]
  
  dfSum[i, 'egno'] <- ind
  dfSum[i, 'eventNo'] <- eventNo
  dfSum[i, 'hAnom'] <- median(hind, na.rm = TRUE)
  dfSum[i, 'gearInj'] <- gstat
  dfSum[i, 'nMonths'] <- length(s:e)
  
  
  
  
}


dfSum$gearnogear <- 0
sev1idx <- which(dfSum$gearInj == 1)
mod1idx <- which(dfSum$gearInj == 2)
min1idx <- which(dfSum$gearInj == 4)
dfSum[sev1idx, 'gearnogear'] <- 1
dfSum[mod1idx, 'gearnogear'] <- 1
dfSum[min1idx, 'gearnogear'] <- 1

dfSum$variable <- 'impacted'

# add in the unimpacted animals - using data coming from the splitpopHealth.R function
# ONly adding them in once to avoid the double data, i.e. adding the same unimpacted data for both
# the rep and the non-rep, when we really only want them once.
if (repro) {
  uvec <- repvec
  dfuvec <- data.frame(egno = 9999,
                       eventNo = 0,
                       nMonths = 0,
                       hAnom = uvec,
                       gearInj = 0, 
                       gearnogear = 0,
                       variable = 'unimpacted')
  dfLong <- rbind(dfSum, dfuvec)
}  else {
  uvec <- nonrepvec
  dfurvec <- data.frame(egno = 9999,
                        eventNo = 0,
                        nMonths = 0,
                        hAnom = uvec,
                        gearInj = 0, 
                        gearnogear = 0,
                        variable = 'unimpacted')
  dfLong <- rbind(dfSum, dfurvec)
}


dfLong$group <- 1
mod0idx <- which(dfLong$gearInj == 5)
min0idx <- which(dfLong$gearInj == 6)
unimpactedidx <- which(dfLong$variable == 'unimpacted')
dfLong[c(mod1idx, mod0idx), 'group'] <- 2
dfLong[c(min1idx, min0idx), 'group'] <- 3
dfLong[unimpactedidx, 'group'] <- 4
dfLong$group <- factor(dfLong$group, levels = c(4, 3, 2, 1) )

# # Update to put an identifier column and then bind both together
if (repro) {
  dfLongRepro <- dfLong
  dfLongRepro$status <- 'RepFem'
  } else {
  dfLongNonRepro <- dfLong
  dfLongNonRepro$status <- 'NonRepFem'
}  






# # run the glm 
# # first relevel so unimpacted is the reference level.
# dfglm <- transform(dfLong, variable = as.factor(variable))
# dfglm <- within(dfglm, variable <- relevel(variable, ref = 'unimpacted'))
# 
# ft1 <- glm(hAnom ~ variable, data = dfglm)
# summary(ft1)
# summary(glht(ft1, mcp(variable = "Tukey")))

} # end loop over the 'repro' variable
save(dfLongRepro, dfLongNonRepro, file = 'data/healthEntanglementWindowDataBoth.rdata') # Both means it's containing repro and non-repro fems



# set up for labeling the box plots
dfLong <- rbind(dfLongRepro, dfLongNonRepro)
namevec = c('Unimpacted'=0, 'Minor No Gear'=6, 'Minor Gear'=4, 'Moderate No Gear'=5,
            'Moderate Gear'=2, 'Severe No Gear'=3, 'Severe Gear'=1)

nvals <- as.data.frame(table(dfLong$status, dfLong$gearInj))

nvals$label <- namevec[nvals$Var2]

plabel <- c(nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 6], 
            nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 6], 
            nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 4], 
            nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 4], 
            nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 5], 
            nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 5], 
            nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 2], 
            nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 2], 
            nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 3], 
            nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 2], 
            nvals$Freq[nvals$Var1 == 'NonRepFem' & nvals$Var2 == 1], 
            nvals$Freq[nvals$Var1 == 'RepFem' & nvals$Var2 == 1])

name <- paste('/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/BoxPlotHealthByEntglClass_NonReproAndReproAnimals_', Sys.Date(), '.pdf', sep = '')
# This updated plot is per Amy's request to include both categories side by side. Instead of breaking down the gear/no-gear for shading
# the shading will be repro vs non-repro
p <- ggplot(dfLong, aes(x = factor(gearInj, levels = c(0, 6, 4, 5, 2, 3, 1)), y = hAnom)) +
  geom_hline(aes(yintercept = 0), colour = 'grey50')+
  geom_boxplot(aes(fill = factor(status), group = paste(factor(gearInj), status)), outlier.shape=NA, notch = FALSE)+
  annotate('text', x = c(1 + 0.82, 1 + 1.18, 1 + 1.82, 1 + 2.18, 1 + 2.82, 1 + 3.18,
                         1 + 3.82, 1 + 4.18, 1 + 4.82, 1 + 5.18, 1 + 5.82, 1 + 6.18), y = 25, 
           label = plabel, cex = 5)+
  labs(y = 'Deviation From Population Health', x = 'Injury Status', fill = 'Reproductive Status')+
  scale_x_discrete(labels = c('Unimpacted', 'Minor\nNo Gear', 'Minor\nGear', 'Moderate\nNo Gear','Moderate\nGear',   'Severe\nNo Gear', 'Severe\nGear'))+
  scale_fill_grey(start = 1, end = 0.65,labels = c('All Other\nDemographic\nCategories\n', 'Reproductive\nFemales\n'))+
  theme_bw(base_size = 16)
p


pdf(file = name, width = 9, height = 9*.61)
print(p)
dev.off()