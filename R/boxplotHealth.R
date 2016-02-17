# Coding by Rob Schick, May 6, 2015
# The goal of this is to make the box plots of the health during the entanglement window
# This uses the data (data frame: tangleOut) coming from this script: prepAmyEntanglementData.R

rm(list=ls())
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(multcomp)
# library(plyr)
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


dfSum <- matrix(NA, nrow = length(unique(tSub$EGNo)), ncol = 10)
colnames(dfSum) <- c('ID', 'Severe - 1', 'Severe - 0', 'Moderate - 1', 
                     'Moderate - 0', 'Minor - 1', 'Minor - 0', 'None', 'Post-Reprod', 'repStatus')
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
  s <- match(t(tSub[tSub$EGNo == ind, 'swindmonyr']), myName)  
  e <- match(t(tSub[tSub$EGNo == ind, 'ewindmonyr']), myName)
  sev <- match(t(tSub[tSub$EGNo == ind, 'fsevmonyr']), myName)
  sev <- sev[is.finite(sev)]
  tfac <- rep(NA, length.out = length(myName))
  
  for(j in 1:evnum){
  
      tfac[s[j]:e[j]] <- as.numeric(ti[j, 'gearInj'] )
    
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
  # dfSum[i, 'repStatus'] <- ind
  
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
  
#   if ('7' %in% names(tSum)) {# no entanglement - before severe
#     dfSum[i, 8] <- tSum[which(names(tSum) == '7')]  
#   }
  
}

dfDat <- data.frame(ID = idFac, dfSum[,2:8])
dfLong <- melt(dfDat, na.rm = TRUE)

dfLong$gearnogear <- 0
sev1idx <- which(dfLong$variable == 'Severe...1')
mod1idx <- which(dfLong$variable == 'Moderate...1')
min1idx <- which(dfLong$variable == 'Minor...1')
dfLong[sev1idx, 'gearnogear'] <- 1
dfLong[mod1idx, 'gearnogear'] <- 1
dfLong[min1idx, 'gearnogear'] <- 1

# add in the unimpacted animals - using data coming from the splitpopHealth.R function
# ONly adding them in once to avoid the double data, i.e. adding the same unimpacted data for both
# the rep and the non-rep, when we really only want them once.
if (repro) {
  uvec <- repvec
  dfuvec <- data.frame(ID = 9999,
                       variable = 'unimpacted',
                       value = uvec,
                       gearnogear = 0)
  dfLong <- rbind(dfLong, dfuvec)
}  else {
  uvec <- nonrepvec
  dfurvec <- data.frame(ID = 9999,
                       variable = 'unimpacted',
                       value = uvec,
                       gearnogear = 0)
  dfLong <- rbind(dfLong, dfurvec)
}


dfLong$group <- 1
mod0idx <- which(dfLong$variable == 'Moderate...0')
min0idx <- which(dfLong$variable == 'Minor...0')
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
# dfglm <- transform(dfLong, variable = factor(variable))
# dfglm <- within(dfLong, variable <- relevel(variable, ref = 'unimpacted'))
# 
# ft1 <- glm(value ~ variable, data = dfglm)
# summary(ft1)
# summary(glht(ft1, mcp(variable = "Tukey")))

} # end loop over the 'repro' variable
save(dfLongRepro, dfLongNonRepro, file = 'data/healthEntanglementWindowDataBoth.rdata') # Both means it's containing repro and non-repro fems



# Commenting this all out because I'm now going to do both side-by-side
# if (repro) {
# #   name <- '/Users/robs/Dropbox/Papers/KnowltonEtAl_Entanglement/images/BoxPlotHealthByEntglClass_ReproFemales.pdf'  
# #   p <- ggplot(dfLong, aes(x = factor(group), y = value)) +
# #     geom_boxplot(aes(fill = factor(gearnogear), group = paste(factor(variable), group)), outlier.shape=NA, notch = FALSE)+
# #     annotate('text', x = c(1 + 0.82, 1 + 1.18, 1 + 1.82, 1 + 2.18, 1 + 2.82, 1 + 3.18), y = 92.5, 
# #              label = plabel, cex = 3)+
# #     labs(y = 'Estimated Health', x = 'Injury Status', fill = 'Gear Status')+
# #     scale_x_discrete(labels = c('Unimpacted', 'Minor', 'Moderate', 'Severe'))+
# #     scale_fill_grey(start = 1, end = 0.65,labels = c('No Gear', 'Carrying Gear'))+
# #     theme_bw()
# } else {
#   name <- '/Users/robs/Dropbox/Papers/KnowltonEtAl_Entanglement/images/BoxPlotHealthByEntglClass_NonReproAnimals.pdf'
#   p <- ggplot(dfLong, aes(x = factor(group), y = value)) +
#     geom_boxplot(aes(fill = factor(gearnogear), group = paste(factor(variable), group)), outlier.shape=NA, notch = FALSE)+
#     annotate('text', x = c(1 + 0.82, 1 + 1.18, 1 + 1.82, 1 + 2.18, 1 + 2.82, 1 + 3.18), y = 92.5, 
#              label = plabel, cex = 3)+
#     labs(y = 'Estimated Health', x = 'Injury Status', fill = 'Gear Status')+
#     scale_x_discrete(labels = c('Unimpacted', 'Minor', 'Moderate', 'Severe'))+
#     scale_fill_grey(start = 1, end = 0.65,labels = c('No Gear', 'Carrying Gear'))+
#     theme_bw()
# }
# 
# 
# pdf(file = name, width = 8, height = 6)
#   print(p)
# dev.off()

# set up for labeling the box plots
dfLong <- rbind(dfLongRepro, dfLongNonRepro)
nvals <- as.vector(table(dfLong$status, dfLong$variable))
plabel <- c(nvals[11], nvals[12], nvals[9], nvals[10], nvals[7], nvals[8],
            nvals[5], nvals[6], nvals[3], nvals[4], nvals[1], nvals[2])

name <- paste('/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/BoxPlotHealthByEntglClass_NonReproAndReproAnimals_', Sys.Date(), '.pdf', sep = '')
# This updated plot is per Amy's request to include both categories side by side. Instead of breaking down the gear/no-gear for shading
# the shading will be repro vs non-repro
p <- ggplot(dfLong, aes(x = factor(variable, levels = c('unimpacted', 'Minor...0', 'Minor...1', 'Moderate...0', 'Moderate...1', 'Severe...0', 'Severe...1')), y = value)) +
  geom_boxplot(aes(fill = factor(status), group = paste(factor(variable), status)), outlier.shape=NA, notch = FALSE)+
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