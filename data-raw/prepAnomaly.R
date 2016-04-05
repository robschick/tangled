rm(list = ls())
load(file = 'data/eg_205_ng_50000_BIG_25000_BIG_25000.rdata')
source('r/returnhealthAnom.R')
anom <- returnhealthAnom()
# save(anom, file = 'data/healthAnomaly.rda')
devtools::use_data(anom)

healthmean <- sumh / g
devtools::use_data(healthmean)

devtools::use_data(ID)

devtools::use_data(nt)

devtools::use_data(myName)