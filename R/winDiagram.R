rm(list = ls())
library(dplyr)
library(ggplot2)
load(file = 'data/healthAnomaly.rda') # contains 'anom' which is deviation from pop health (Adult males and juveniles) for all animals
load(file="data/egAmyEntData.rdata") # egAmyEntData.rdata contains tangleOut, tangRepro, tangNonRepro, so use the repro flag
# filter(tangRepro, gearInj == 2)# just to see what animal to choose

times <- tangRepro[tangRepro$EGNo == 2040, c('swindmonyr', 'ewindmonyr', 'rec12monyr')]

df <- data.frame(time = 1:ncol(anom), health = anom[which(rownames(anom) == 2040), ])
df$pos <- df$health > 0
df <- df[!is.na(df$health), ]


ggplot(df, aes(x = time, y = health, fill = pos)) +
  geom_bar(stat = 'identity', position = 'identity', width = 0.5)
