rm(list = ls())
load(file = 'data/eg_203_ng_50000_BIG_25000.rdata')

source(file = 'R/getDeaths.R')
healthmean <- sumh / g
deathMed <- getDeaths(deathyr, medProb = TRUE)
for(i in 1:nrow(healthmean)){
  healthmean[i, 1:firstSight[i]] <- NA
  if (deathMed[i] < nt) {
    healthmean[i, (deathMed[i] + 1):nt] <- NA
  }
}
devtools::use_data(healthmean, overwrite = TRUE)
devtools::use_data(deathyr, overwrite = TRUE)
devtools::use_data(ng, overwrite = TRUE)
devtools::use_data(deadTable, overwrite = TRUE)

source('R/returnhealthAnom.R')
anom <- returnhealthAnom(healthmean)
devtools::use_data(anom, overwrite = TRUE)

devtools::use_data(ID, overwrite = TRUE)

devtools::use_data(nt, overwrite = TRUE)

devtools::use_data(myName, overwrite = TRUE)

dcut <- which(myName == '12-2013') 
devtools::use_data(dcut, overwrite = TRUE)

devtools::use_data(firstSight, overwrite = TRUE)

devtools::use_data(lastSight, overwrite = TRUE)
rm(list = ls())