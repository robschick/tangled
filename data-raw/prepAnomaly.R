rm(list = ls())
load(file = 'data-raw/eg_203_ng_50000_BIG_25000_BIG_25000.rdata')

# Population level median health
source('R/returnPophealth.R')
pophealth <- returnPophealth()
devtools:: use_data(pophealth, overwrite = TRUE)

# Individual health
source(file = 'R/getDeaths.R')
deathMed <- getDeaths(deathyr, medProb = TRUE)
healthmeanSP <- healthmean <- sumh / g
for(i in 1:nrow(healthmean)){
  
  healthmean[i, 1:firstSight[i]] <- NA
  healthmeanSP[i, 1:firstSight[i]] <- NA
  
  if (deathMed[i] < nt) {
    healthmean[i, (deathMed[i] + 1):nt] <- NA
  }
  
}
devtools::use_data(healthmean, overwrite = TRUE)
devtools::use_data(healthmeanSP, overwrite = TRUE)

# Population level standard deviation of health 
hvar <- sumh2 / ngg - healthmean ^ 2
healthsd   <- sqrt(hvar)
devtools::use_data(healthsd, overwrite = TRUE)

# Death information
devtools::use_data(deathyr, overwrite = TRUE)
devtools::use_data(deadTable, overwrite = TRUE)

# Population level median health Anomaly
source('R/returnhealthAnom.R')
anom <- returnhealthAnom(healthmean)
devtools::use_data(anom, overwrite = TRUE)
anomSP <- returnhealthAnom(healthmeanSP)
devtools::use_data(anomSP, overwrite = TRUE)


# Ancillary support info:
devtools::use_data(ng, overwrite = TRUE)
devtools::use_data(gender, overwrite = TRUE)
devtools::use_data(ID, overwrite = TRUE)
devtools::use_data(nt, overwrite = TRUE)
devtools::use_data(myName, overwrite = TRUE)
dcut <- which(myName == '12-2013') 
devtools::use_data(dcut, overwrite = TRUE)
devtools::use_data(firstSight, overwrite = TRUE)
devtools::use_data(lastSight, overwrite = TRUE)
entvec <- c('Minor - No Gear' = 6, 'Moderate - No Gear' = 5, 'Minor - Gear' = 4, 
            'Severe - No Gear' = 3, 'Moderate - Gear' = 2, 'Severe - Gear' = 1)
devtools::use_data(entvec, overwrite = TRUE)
rm(list = ls())