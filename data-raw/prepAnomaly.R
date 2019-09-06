rm(list = ls())
load(file = 'data-raw/OLD_eg_203_ng_50000_BIG_25000_BIG_25000.rdata')

# # Population level median health
# source('R/returnPophealth.R')
# pophealth <- returnPophealth()
# devtools::use_data(pophealth, overwrite = TRUE)

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
usethis::use_data(healthmean, overwrite = TRUE)
usethis::use_data(healthmeanSP, overwrite = TRUE)

# Population level standard deviation of health 
hvar <- sumh2 / ngg - healthmean ^ 2
healthsd   <- sqrt(hvar)
usethis::use_data(healthsd, overwrite = TRUE)

# Death information
usethis::use_data(deathyr, overwrite = TRUE)
usethis::use_data(deadTable, overwrite = TRUE)

# Population level median health Anomaly
source('R/returnhealthAnom.R')
anom <- returnhealthAnom(healthmean)
usethis::use_data(anom, overwrite = TRUE)
anomSP <- returnhealthAnom(healthmeanSP)
usethis::use_data(anomSP, overwrite = TRUE)

# Ancillary support info:
usethis::use_data(ng, overwrite = TRUE)
usethis::use_data(gender, overwrite = TRUE)
usethis::use_data(ID, overwrite = TRUE)
usethis::use_data(nt, overwrite = TRUE)
usethis::use_data(myName, overwrite = TRUE)
dcut <- which(myName == '12-2017') 
usethis::use_data(dcut, overwrite = TRUE)
usethis::use_data(firstSight, overwrite = TRUE)
usethis::use_data(lastSight, overwrite = TRUE)
entvec <- c('Minor - No Gear' = 6, 'Moderate - No Gear' = 5, 'Minor - Gear' = 4, 
            'Severe - No Gear' = 3, 'Moderate - Gear' = 2, 'Severe - Gear' = 1)
usethis::use_data(entvec, overwrite = TRUE)

# Information about: Entanglement, Calves and Observed Deaths 
tgl <- readr::read_csv(here::here('data-raw', '2019-04-03_Tbl-Whale-Entanglement.csv'))
deadTable <- readr::read_csv(here::here('data-raw', '2019-04-08_All-dead-events_non-ID-removed.csv'))
calves <- readr::read_csv(here::here('data-raw', '2019-04-08_All-calving-events.csv'))

tgl$StartDate <- as.Date(tgl$StartDate, format = '%m/%d/%Y')
tgl$EndDate <- as.Date(tgl$EndDate, format = '%m/%d/%Y')
tangled <- dplyr::tbl_df(tgl)
calves <- dplyr::tbl_df(calves)

usethis::use_data(tangled, overwrite = TRUE)
usethis::use_data(deadTable, overwrite = TRUE)
usethis::use_data(calves, overwrite = TRUE)

rm(list = ls())