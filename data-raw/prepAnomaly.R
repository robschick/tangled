rm(list = ls())
load(file = 'data/eg_205_ng_50000_BIG_25000_BIG_25000.rdata')
source('r/returnhealthAnom.R')
anom <- returnhealthAnom()
# save(anom, file = 'data/healthAnomaly.rda')
devtools::use_data(anom, overwrite = TRUE)

healthmean <- sumh / g
for(i in 1:nrow(healthmean)){
  healthmean[i, 1:firstSight[i]] <- NA
}
devtools::use_data(healthmean, overwrite = TRUE)

devtools::use_data(ID, overwrite = TRUE)

devtools::use_data(nt, overwrite = TRUE)

devtools::use_data(myName, overwrite = TRUE)

devtools::use_data(firstSight, overwrite = TRUE)

devtools::use_data(lastSight, overwrite = TRUE)
rm(list = ls())