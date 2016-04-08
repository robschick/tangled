# repro females
dfout <- numeric(0)

healthmean <- sumh / g
for(i in 1:nrow(healthmean)){
  healthmean[i, 1:firstSight[i]] <- NA
}


for(i in 1:nrow(tangRepro)){
  
  ind <- tangRepro$EGNo[i]
  tsub <- tangRepro[i, ]
  htest <- healthmean[which(ID == ind),]
  
  s <- match(tsub[, 'swindmonyr'], myName)  
  e <- match(tsub[, 'ewindmonyr'], myName)
  
  gstat <- tsub[, 'gearInj']
  
  hVal <- htest[s:e] # this gets all the health values, but will be ragged
  lh <- length(hVal)
  l70 <- length(which(hVal < 70))
  p70 <- (l70 / lh ) * 100
  l67 <- length(which(hVal < 67))
  p67 <- (l67 / lh ) * 100
  
  dfi <- data.frame(egno = ind, nmonths = lh, mon67 = l67, pctmon67 = p67, 
                    mon70 = l70, pctmon70 = p70, gearInjury = gstat) 
  dfout <- rbind(dfout, dfi)
}

rfm70 <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(n = n(),
            totNumMonths = sum(nmonths),
            maxDurMonths = max(nmonths),
            sumBel70Months = sum(mon70),
            meanp70 = mean(pctmon70))
rfm70

rfm67 <- dfout %>% 
  group_by(gearInj) %>% 
  summarise(n = n(),
            totNumMonths = sum(nmonths),
            maxDurMonths = max(nmonths),
            sumBel70Months = sum(mon67),
            meanp67 = mean(pctmon67))
rfm67
