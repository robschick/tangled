rm(list = ls())
library(dplyr)
library(ggplot2)
load(file = 'data/healthAnomaly.rda') # contains 'anom' which is deviation from pop health (Adult males and juveniles) for all animals
load(file="data/egAmyEntData.rdata") # egAmyEntData.rdata contains tangleOut, tangRepro, tangNonRepro, so use the repro flag
# filter(tangRepro, gearInj == 2)# just to see what animal to choose


yrvec  <- 1970:2011
nyr    <- length(yrvec)
monYr  <- cbind( rep(c(1:12), nyr), rep(yrvec, each = 12) )
myName <- paste(monYr[, 1], monYr[, 2], sep = '-')
dateTime <- as.Date(paste(1, monYr[, 1], monYr[, 2], sep = '/'), format = '%d/%m/%Y')

stopifnot(all.equal(length(myName), dim(anom)[2])) # this is in there because of how I'm trying to recreate the time vector.

times <- tangleOut[tangleOut$EGNo == 1167, c('swindmonyr', 'ewindmonyr', 'rec12monyr')]
sidx <- match(times$swindmonyr, myName)
eidx <- match(times$ewindmonyr, myName)
ridx <- match(times$rec12monyr, myName)
tidx <- cbind(sidx, eidx, ridx)

df <- data.frame(dateTime = dateTime, 
                 time = 1:ncol(anom), 
                 health = anom[which(rownames(anom) == 1167), ])
df$pos <- df$health > 0
df <- df[!is.na(df$health), ]

dfsub <- subset(df, time %in% tidx[4,])

# to label the health during the first event: 
#  median(df[df$time %in% 329:332, 'health'])

p <- ggplot(df, aes(x = dateTime, y = health, fill = pos)) +
  geom_bar(stat = 'identity', position = 'identity', width = 40)+
  annotate('text', x = dateTime[tidx[2, 1]] + 30, y = 16, label = 'Event #2')+
  annotate('rect', xmin = dateTime[tidx[2, 1]], xmax = dateTime[tidx[2, 2]], ymin = 0, ymax = 14, alpha = 0.4, fill = 'blue')+
  annotate('segment', x = dateTime[tidx[2, 1]], xend = dateTime[tidx[2, 2]], y = 15, yend = 15, 
           arrow=arrow(ends = 'both', angle = 90, length= unit(0.2, 'cm')))+
  annotate('text', x = dateTime[tidx[2, 1]] + 45, y = -5, label = 'Median Anomaly\nDuring Window\n(12.98)')+
  annotate('segment', x = dateTime[tidx[2, 1]] + 45, xend = dateTime[tidx[2, 1]] + 45, 
           y = -3.75, yend = -0.25, arrow = arrow())+
  annotate('segment', x = dateTime[tidx[3, 1]], xend = dateTime[tidx[3, 2]], y = 15, yend = 15, 
           arrow=arrow(ends = 'both', angle = 90, length= unit(0.2, 'cm')))+
  annotate('text', x = dateTime[tidx[3, 1]] + 30, y = 16, label = 'Event #3')+
  annotate('segment', x = dateTime[tidx[4, 1]], xend = dateTime[tidx[4, 2]], y = 15, yend = 15, 
           arrow=arrow(ends = 'both', angle = 90, length= unit(0.2, 'cm')))+
  annotate('text', x = dateTime[tidx[4, 1]] + 550, y = 16, label = 'Event #4')+
  annotate('point', x = df$dateTime[tidx[4, 1]], y = df$health[tidx[4, 1]])+
  geom_point(data = dfsub, size = 3)+
  annotate('segment', x = dfsub$dateTime[1], xend = dfsub$dateTime[2], y = dfsub$health[1], yend = dfsub$health[2])+
  annotate('text', x = dfsub$dateTime[1] + 180, y = dfsub$health[1], label = 'Anomaly at\nStart of Window')+
  annotate('segment', x = dfsub$dateTime[2], xend = dfsub$dateTime[3], y = dfsub$health[2], yend = dfsub$health[3])+
  annotate('text', x = dfsub$dateTime[2] - 150, y = dfsub$health[2] + 2, label = 'Anomaly at\nEnd of Window')+
  annotate('text', x = dfsub$dateTime[3] - 150, y = dfsub$health[3] + 1.5, label = 'Anomaly at\nAfter 12 Months')+
  labs(x = 'Time', y = 'Health Anomaly')+
  scale_x_date(limits = c(as.Date("1997-01-01"), as.Date("2004-01-01")))+
  theme(legend.position = 'none')


name <- paste('/Users/rob/Dropbox/Papers/KnowltonEtAl_Entanglement/images/ExtractionDiagram_', Sys.Date(), '.pdf', sep = '')

pdf(file = name, width = 9, height = 9*.61)
  print(p)
dev.off()