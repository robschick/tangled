# Plot out the estimates of health for the animals with body fat == 3, i.e. severely thin
# It would be good to plot their sightings history, and tag the place where it was
# scored thin - both on sightings history, and on the health plot time series
rm(list=ls())
library(ggplot2)
library(lubridate)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(stringr)
library(plyr)
wd <- '/Users/rob/Documents/research/projects/PCAD/rightwhales/'
setwd(wd)
load(file = paste(wd, 'gibbsoutput/eg_105_ng_50000_BIG_50000.rdata', sep = '') )
ngg <- g-1
outrdata <- paste('/Users/rob/Documents/research/projects/PCAD/rightwhales/', modelname, 'HealthPlot.rdata', sep = '')
save(sumh, sumh2, ngg, ID, deathyr, monYr, nt, rsum, tIndex, hobs, health, deadID, deadTime, deadTable, skin, 
     calves, rake, cyam, breakBf, modelname, startYr, stopYr, newhgibbsMales,newhgibbsJuvs1, newhgibbsJuvs3,
     newhgibbsJuvs12, newhgibbsJuvs32, newhgibbsMales2,
     file = outrdata)
load('egDemogDataOldVersion_Pre2014.rdata')
demog <- new.env()
demog$calves <- calves
load(file = outrdata)
load(file = 'goodcalves.rdata')
load(file="egAmyEntData.rdata") # contains tangleOut to be used to plot gear carrying
source('/Users/rob/Documents/code/rss10/rightwhales/plotOne.R')
source('/Users/rob/Documents/code/rss10/rightwhales/makeYearmon.r')
load(file = "/Users/rob/Documents/research/projects/PCAD/rightwhales/tStatesLumped.rdata")
FAT <- FALSE
present <- FALSE

# Set up some different sets of animals to loop over:
# 1. Severely entangled animals
# 2. Body Fat == 3 animals
# 3. Animals with lots of sightings
# 4. Animals with few sightings

# 1. Severe
# This contains all of the animals that were serverly entangled in the 1995-2005 range.
# I'm just using it now to show how this could work with Amy's entangled animals
sev.idx <- c(1601, 1815, 2030, 2212, 2427, 2557, 3107, 1608, 2220, 2240, 2301, 1430, 1027, 1167, 3445, 1055)

# 2. Body Fat == 3 animals
bf.sights <- sights[which(sights$BodyFatCode == 3),]
bf3.idx <- unique(bf.sights$SightingEGNo)

# 3. Animals with lots of sightings
sights.tl <- ddply(sights, .(SightingEGNo), nrow)
sights.tl <- sights.tl[sights.tl$V1 > 200, ]
many.idx <- sights.tl$SightingEGNo

# # 4. Animals with few sightings (20 < n < 30)
# sights.tl <- ddply(sights, .(SightingEGNo), nrow)
# sights.tl <- sights.tl[sights.tl$V1 > 20 & sights.tl$V1 < 30, ]
# few.idx <- sights.tl$SightingEGNo

# 5. Manuscript Animals - i.e. the ones we plotted in the PLoS-ONE paper
plos.idx <- c(1014, 1033, 1333, 1245, 3911, 1077, 1014, 1170, 1507)

# 6. long sighting gap animals
# long.idx <- c(1715, 1412, 1036, 1175, 1990, 1035, 1043, 1155, 2420, 1154, 1276, 1320, 1805)

# 7. Entanglement animals - a list of animals that Amy wants to check
# efems <- c(1140, 1158, 1163, 1266, 1430, 1601, 1608, 1705, 1815, 2029, 2040, 2240, 2301, 2645)
efems <- c(1503, 1113, 1130, 1140, 1151, 1163, 1167, 1249, 1424, 1427, 2040, 2320, 2366, 3346, 1152) # list from Amy 19 Mar 2015

healthPaper <- 3903
  
# Put the IDs all together in a list
allIdx <- list(efems)# few.idx, plos.idx, long.idx, sev.idx, bf3.idx, many.idx, plos.idx, 
allNames <- c('entangledWhalesforAmy_HealthwAnomaly.pdf') #'healthPaper_EG3903_HealthwAnomaly.pdf', 'plosPaperAnimalsHealthSegmentwHealthAnomaly.pdf','FewSightsAnimalsHealthSegment.pdf', 'longSightingGapAnimalsHealthSegment.pdf', 'SevEntAnimalsHealthSegment.pdf', 'BodyFat3_AnimalsHealthSegment.pdf', 'ManySightsAnimalsHealthSegment.pdf', 'plosPaperAnimalsHealthSegment.pdf', 


#################################################################################
# Calc population level mean health to be used later in the anomaly plot
healthmean <- sumh/ngg
healthmeanM <- newhgibbsMales / ngg
healthmeanJ1 <- newhgibbsJuvs1 / ngg
healthmeanJ2 <- newhgibbsJuvs3 / ngg
healthmeanMSD <- newhgibbsMales2 / ngg
healthmeanJ1SD <- newhgibbsJuvs12 / ngg
healthmeanJ2SD <- newhgibbsJuvs32 / ngg

for(i in 1:nrow(healthmean)){
  healthmean[i, 1:firstSight[i]] <- NA
  healthmeanM[i, 1:firstSight[i]] <- NA
  healthmeanJ1[i, 1:firstSight[i]] <- NA
  healthmeanJ2[i, 1:firstSight[i]] <- NA
  healthmeanMSD[i, 1:firstSight[i]] <- NA
  healthmeanJ1SD[i, 1:firstSight[i]] <- NA
  healthmeanJ2SD[i, 1:firstSight[i]] <- NA
}

newPopCat <- rbind(healthmeanM, healthmeanJ1, healthmeanJ2)
newPopMedian <- apply(newPopCat, 2, median, na.rm = TRUE)

newPopAllVar <- rbind(healthmeanMSD, healthmeanJ1SD, healthmeanJ2SD)
newPopvar <- newPopAllVar - newPopCat ^ 2
newpopsd <- sqrt(newPopvar)
newpopsdmean <- apply(newpopsd, 2, median, na.rm = T)

popplot <- data.frame(time = tIndex, 
                    popHealth = newPopMedian, 
                    hsd = newpopsdmean, 
                    date = makeYearmon(monYr))
# End pop-level health calc
#################################################################################


for(l in 1:length(allIdx))
{
  bf3.idx <- unlist(allIdx[l])
  name <- allNames[l]

name <- paste(modelname, name, sep = '')
if(present){name <- paste('Present_', name, sep = '')}
mypal <- brewer.pal(4, 'Set2')


pdf(file = paste('images/', name, sep = ''), width = 7, height = 7)
# 	grid.newpage()	
# 	pushViewport(viewport(layout=grid.layout(3,2)))
# 	vplayout <- function(x,y)
# 		viewport(layout.pos.row=x,layout.pos.col=y)

for(j in 1:length(bf3.idx)){
# Plot of Sightings
p1 <-  plotOne(egno = bf3.idx[j], sights, demog$calves, batch, present = present)


bsub <- p1$bsub

if('trectLong' %in% names(p1)) {
  trectLong <- p1$trectLong
  trectLong$cnum <- NA
  trectLong[trectLong$severity == 'minor', 'cnum'] <- 1
  trectLong[trectLong$severity == 'moderate', 'cnum'] <- 2
  trectLong[trectLong$severity == 'severe', 'cnum'] <- 3
}


# plot of health  
  hvar <- sumh2 / ngg - healthmean ^ 2
  healthsd   <- sqrt(hvar)
  sub <- bf.sights[bf.sights$SightingEGNo == bf3.idx[j], ]
  wan <- 	which(ID == bf3.idx[j])
  svec <- 1 - cumsum(deathyr[wan, ] / ngg)[-(nt + 1)]
  svec <- svec[1:ncol(healthmean)]
  t1   <- min(which(apply(rsum[wan, , ], 1, sum) / ngg > 0))
  svec[1:(t1 - 1)] <- 0
  wm <- which(monYr[, 1] == 1)
  wy <- which(monYr[, 2] %in% seq(1900, 2020, by = 10))
  ww <- intersect(wm, wy)
  wws <- which(svec > .001)
  ti <- which(svec > 0)
  tci1 <- which(colnames(tStates) == paste('1-', startYr, sep = ''))
  tci2 <- which(colnames(tStates) == paste('12-', stopYr, sep = ''))
  tStates <- tStates[, tci1:tci2]
  www <- which(is.finite(hobs[wan, ]))
  wwt <- which(is.finite(tStates[rownames(tStates) == bf3.idx[j], ]))
  wwsk <- which(is.finite(skin[wan, ]))
  wwc <- which(is.finite(calves[rownames(calves) == bf3.idx[j], ]))
  wwcy <- which(is.finite(cyam[wan, ]))
  wwrk <- which(is.finite(rake[wan, ]))
  lobf <- which(hobs[wan, ] == 1)   
  


  csub <- numeric(0)
  if(length(wwc) != 0)
  {  
    csub <- data.frame(date = makeYearmon(monYr[wwc,]), calves = calves[which(rownames(calves) == bf3.idx[j]), wwc], group = monYr[wwc,2])
    csub$calfac[csub$calves == 1] <- 2  
    csub$calfac[csub$calves == 2] <- 1
  }

  

  dplot1 <- data.frame(value = healthmean[wan, wws], 
    hsd = healthsd[wan, wws], 
    date = makeYearmon(monYr[wws,]),
    type = 'Health')  

# If I want to plot survival as well, I uncomment this next bit
#   dplot2 <- data.frame(value = round(1 * svec[wws], 4), 
#                     hsd = 0, 
#                     date = makeYearmon(monYr[wws,]),
#                     type = 'Survival') 
# dplot <- rbind(dplot1, dplot2)
dplot <- dplot1
 
# Body Fat
if(length(www) == 0)
{
  hsub <- numeric(0)
} else {
  hsub <- data.frame(date = makeYearmon(monYr[www,]), obs = hobs[wan, www])
  hsub$fac[hsub$obs == 3] <- 1
  hsub$fac[hsub$obs == 2] <- 2
  hsub$fac[hsub$obs == 1] <- 3
  hsub$type <- 'Body'
}
  

# Skin
if(length(wwsk) == 0)
{
  ssub <- numeric(0)
} else {
  ssub <- data.frame(date = makeYearmon(monYr[wwsk,]), obs = skobs[wan, wwsk])
  ssub$fac[ssub$obs == 1] <- 2
  ssub$fac[ssub$obs == 2] <- 1
  ssub$type <- 'Skin'
}
  

# Cyamids
if(length(wwcy) == 0)
  {
  cysub <- numeric(0)
  } else{
  cysub <- data.frame(date = makeYearmon(monYr[wwcy,]), obs = cyam[wan, wwcy])
  cysub$fac[cysub$obs == 1] <- 2
  cysub$fac[cysub$obs == 2] <- 1
  cysub$type <- 'Cyam'
  }

# Rake Marks
if(length(wwrk) == 0)
  {
  rsub <- numeric(0)
  } else{
  rsub <- data.frame(date = makeYearmon(monYr[wwrk,]), obs = rake[wan, wwrk])
  rsub$fac <- rsub$obs
  rsub$fac[rsub$obs == 1] <- 3
  rsub$fac[rsub$obs == 3] <- 1
  rsub$type = 'Rake'
  }


# Calves

  if(length(wwc) != 0)
  {
    csubp <- data.frame(date = csub$date, obs = 0, fac = csub$calfac, type = 'Calves')
  } else {
    csubp <- numeric(0)
  }  


# # Entanglement 
# if(length(wwt) != 0 & sum(is.finite(tsub$tangle)) > 0)
# {
#   tsubp <- data.frame(date = tsub$date, obs = 0, fac = tsub$tfac, type = 'Entngl')
# } else {
#   tsubp <- numeric(0)
# }

# if(bf3.idx[j] == 3911)
#   {
#   tsubp <- data.frame(date = tsub$date, obs = 0, fac = tsub$tfac, type = 'Entngl')
# }

# Put them all together to facilitate faceting
if(length(hsub) == 0 & length(ssub) == 0 & length(cysub) == 0 & length(rsub) == 0 )
{
  next
}
hplot <- rbind(hsub, ssub, cysub, rsub)
hplot$type <- factor(hplot$type, levels = c("Body",   "Skin" ,  "Cyam" ,  "Rake"))

# Start/end days
  sday <- data.frame(date = as.Date('1980-01-01'))
  eday <- data.frame(date = as.Date(max(dplot$date) + months(2)))  
#   lday <- data.frame(date = hsub[nrow(hsub), 'date'])
  

if(present == FALSE){
  base_size = 10
  point_size = 2} else {
    base_size = 16
    point_size = 4
  }

toffset <- 10
textsize <- 4

didx <- which(bf3.idx[j] == ID[deadID])
dtime <- deadTime[didx]
dtimedate <- makeYearmon(monYr[dtime,])
ddat <- data.frame(date = dtimedate, health = dplot[nrow(dplot), 'value'])

p <- ggplot(data = dplot, aes(date, value))+
  geom_line(data = popplot, aes(date, popHealth), lty = 2, colour = 'grey50')+
  geom_ribbon(aes(x = date, ymin = value - hsd, ymax = value + hsd), alpha = 0.2)+    
  geom_line()+
  geom_point(data = ddat, aes(date, health), pch = 22, fill = 'red', size = 6)+
  ylim(c(0, 100))+
  facet_grid(type ~ ., scales = 'free_y')+
  labs(x = 'Year', y = '')+
  scale_x_date(minor_breaks = date_breaks(width = "1 year"), limits = c(as.Date(min(dplot$date) - months(2)), as.Date(max(dplot$date))))+
  theme(plot.margin = unit(c(0, 1, 0, 0.5), 'line'))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = base_size, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = base_size, angle=90),
        legend.text = element_text(size = base_size),
        legend.title = element_text(size = base_size),
        panel.grid.minor.x = element_line(colour = 'grey70', size = 0.25),
        panel.grid.major.x = element_line(colour = 'grey50', size = 0.5))
# p


############################################
# Anomaly Plot
dfAnom <- merge(popplot, dplot, by = 'date')
dfAnom$anomaly <- dfAnom$value - dfAnom$popHealth
idx <- which(abs(dfAnom$anomaly) < dfAnom$hsd.y)
dfAnom$anomaly[idx] <- NA
dfAnom$pos <- dfAnom$anomaly > 0
dfAnom$type <- 'Anomaly'

anom <- ggplot(dfAnom, aes(x = date, y = anomaly, fill = pos))+
  geom_bar(stat = 'identity', position = 'identity', width = 50)+
  scale_colour_brewer('+/- Health', palette = 'Dark2', labels = c('Pos', 'Neg'), drop = FALSE)+
  labs(x = 'Date', y = '')+
  scale_x_date(minor_breaks = date_breaks(width = "1 year"), limits = c(as.Date(min(dplot$date) - months(2)), as.Date(max(dplot$date))) )+
  facet_grid(type ~ .)+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = base_size, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = base_size, angle=90),
        legend.text = element_text(size = base_size),
        legend.title = element_text(size = base_size),
        legend.position = 'none')+
  theme(plot.margin = unit(c(-0.5, 1, 0, 0.5), 'line'),
        panel.grid.minor.x = element_line(colour = 'grey70', size = 0.25),
        panel.grid.major.x = element_line(colour = 'grey50', size = 0.5))
# anom



# Plot the standard Error
pstDat <- dplot
pstDat$type  <- 'Standard Error'
pstd <- ggplot(data = pstDat, aes(date, value))+
  geom_ribbon(aes(x = date, ymin = 0, ymax = 2 * hsd), alpha = 0.4)+
  ylim(0,20)+
  facet_grid(type ~ ., scales = 'free_y')+
  scale_x_date(minor_breaks = date_breaks(width = "1 year"), limits = c(as.Date(min(dplot$date) - months(2)), as.Date(max(dplot$date))))+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), plot.margin = unit(c(0, 1, 0, 0.5), 'line'),
        panel.grid.minor.x = element_line(colour = 'grey70', size = 0.25),
        panel.grid.major.x = element_line(colour = 'grey50', size = 0.5))+ 
  labs(x=NULL)

phealth <- ggplot(data = hplot, aes(x = as.Date(date), y = 1, colour = factor(fac, levels = c(1, 2, 3)), size = factor(fac, levels = c(1, 2, 3))))+
  geom_point()+
  scale_colour_brewer('Health Obs', palette = 'Dark2', labels = c('Good', 'Fair', 'Poor'), drop = FALSE)+
  scale_size_discrete('Health Obs', labels = c('Good', 'Fair', 'Poor'), drop = FALSE, range = c(2, 4, 6)) +
  labs(x = '', y = '')+
  labs(title = paste('EGNo = ', bf3.idx[j], sep = ''))+
  scale_x_date(minor_breaks = date_breaks(width = "1 year"), limits = c(as.Date(min(dplot$date) - months(2)), as.Date(max(dplot$date))) )+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = base_size),
  axis.title.y = element_text(size = base_size, angle=90),
  legend.text = element_text(size = base_size),
  legend.title = element_text(size = base_size),
        legend.position = 'none')+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), plot.margin = unit(c(0.5, 1, 0, 0.5), 'line'),
        panel.grid.minor.x = element_line(colour = 'grey70', size = 0.25),
        panel.grid.major.x = element_line(colour = 'grey50', size = 0.5))+ 
  labs(x=NULL)+
  facet_grid(type ~ .)




# To plot the entanglement events separately
library(grid)
idx <- seq(2, max(nrow(trectLong)), by = 2)
tend <- trectLong[idx, ]
tend$type <- 'Entgl'

tpal <- brewer.pal(n = 3, 'Dark2')

dfe <- subset(tangleOut, EGNo == bf3.idx[j]) # I'll use the dates from here (EndDate, LastDatewGear) to plot the gear carrying time
dfe  <-  dfe[dfe$LastDatewGear != '1600-01-01', ]
colnames(dfe)[colnames(dfe) == 'EventNo'] <- 'event'

ptangle <- ggplot(data = trectLong, aes(x = as.Date(date), y = 1, group = event )) +
    geom_line(lwd = 1.5, arrow = arrow(ends = 'first', angle = 90, length = unit(0.2, 'cm')), colour = grey(0.5)) +
    geom_point(data = tend, aes(x = as.Date(date), y = 1, size = factor(severity), fill = factor(severity)), pch = 21, colour = 'white' )+
    scale_size_manual(values = c(4, 6, 8))+
    scale_fill_manual(values = c('minor' = tpal[1], 'moderate' = tpal[2], 'severe' = tpal[3]))+
    scale_x_date(minor_breaks = date_breaks(width = "1 year"), limits = c(as.Date(min(dplot$date) - months(2)), as.Date(max(dplot$date))) )+
    facet_grid(type ~ .)+
    theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 1, 0, 0.5), 'line'),
        axis.title.x = element_text(size = base_size),
        axis.title.y = element_text(size = base_size, angle=90),
        legend.text = element_text(size = base_size),
        legend.title = element_text(size = base_size),
        legend.position = 'none')+
    theme(panel.grid.minor.x = element_line(colour = 'grey70', size = 0.25),
        panel.grid.major.x = element_line(colour = 'grey50', size = 0.5))+ 
    labs(y = NULL, x = 'Date')

if(!nrow(dfe) == 0){ptangle <- ptangle + geom_segment(data = dfe, aes(x = as.Date(EndDate), xend = as.Date(LastDatewGear), y = 1, yend = 1), lwd = 1.5, arrow = arrow(ends = 'last', angle = 90, length = unit(0.2, 'cm')), colour = grey(0.25)) +
                      geom_point(data = tend, aes(x = as.Date(date), y = 1, size = factor(severity), fill = factor(severity)), pch = 21, colour = 'white' )}
# ptangle


# To plot the Calving events separately
if(length(wwc) != 0){
  csubp$type <- 'Calf'
  pcalf <- ggplot(data = csubp, aes(x = as.Date(date), y = 1, group = factor(fac), colour = factor(fac) )) +
    geom_point()+
    #   geom_line(lwd = 1.5, arrow = arrow(ends = 'both', angle = 90, length = unit(0.2, 'cm'))) +
    scale_colour_manual(values = c('2' = grey(0.25), '1' = grey(0.75)))+
    scale_x_date(minor_breaks = date_breaks(width = "1 year"), limits = c(as.Date(min(dplot$date) - months(2)), as.Date(max(dplot$date))) )+
    facet_grid(type ~ .)+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(size = base_size),
          axis.title.y = element_text(size = base_size, angle=90),
          legend.text = element_text(size = base_size),
          legend.title = element_text(size = base_size),
          legend.position = 'none')+
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
          axis.title.x = element_blank(), plot.margin = unit(c(-.5, 1, 0, 0.5), 'line'),
          panel.grid.minor.x = element_line(colour = 'grey70', size = 0.25),
          panel.grid.major.x = element_line(colour = 'grey50', size = 0.5))+ 
    labs(x = NULL, y = NULL)
  # pcalf
}




# grid.newpage()
gA <- ggplot_gtable(ggplot_build(phealth))
gB <- ggplot_gtable(ggplot_build(p))
gC <- ggplot_gtable(ggplot_build(pstd))
gD <- ggplot_gtable(ggplot_build(ptangle))
if(length(wwc) != 0){gE <- ggplot_gtable(ggplot_build(pcalf))}
gF <- ggplot_gtable(ggplot_build(anom))

if(length(wwc) != 0){
  maxWidth = grid::unit.pmax(gA$widths[2:3], gB$widths[2:3], gC$widths[2:3], gD$widths[2:3], gE$widths[2:3], gF$widths[2:3])
} else {
  maxWidth = grid::unit.pmax(gA$widths[2:3], gB$widths[2:3], gC$widths[2:3], gD$widths[2:3], gF$widths[2:3])
}

gA$widths[2:3] <- as.list(maxWidth)
gB$widths[2:3] <- as.list(maxWidth)
gC$widths[2:3] <- as.list(maxWidth)
gD$widths[2:3] <- as.list(maxWidth)
if(length(wwc) != 0){gE$widths[2:3] <- as.list(maxWidth)}
gF$widths[2:3] <- as.list(maxWidth)

if(length(wwc) != 0){
  # grid.arrange(gA, gC, gB,  ncol=1, heights = c(1/4, 1/4, 1/2))
#   grid.arrange(gA, gE, gD, gB, gF, ncol=1, heights = c(0.4, 0.075, 0.075, 0.3, 0.2)) # original
  grid.arrange(gA, gB, gF, gE, gD,  ncol=1, heights = c(0.25,  0.3, 0.15, 0.05, 0.1))# with entanglement and calving on the bottom
} else {
#   grid.arrange(gA, gD, gB, gF, ncol=1, heights = c(0.3925, .075, 0.3, 0.2))# original
  grid.arrange(gA, gB, gF, gD, ncol=1, heights = c(0.3, 0.3, 0.2, .1))# with entanglement and calving on the bottom
}

# grid.arrange(gB, gF, ncol=1, heights = c(0.5, 0.5))
} # end j loop over individuals
dev.off()
} # end l loop over the 3 indices