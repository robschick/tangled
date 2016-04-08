# with a health threshold of 70
gname <- c('Severe - gear', 'Moderate - gear', 'Severe - no gear', 
           'Minor - gear', 'Moderate - no gear', 'Minor - no gear')
dfb <- data.frame(gearInj = c(rfm70$gearInj, 0),
                  gearName = c(gname, 'Unimpacted'),
                  pct70 = c(rfm70$meanp70, p70nmon),
                  pct67 = c(rfm67$meanp67, p67nmon),
                  nmon70 = c(rfm70$totNumMonths, nmon),
                  nmonB70 = c(rfm70$sumBel70Months, nmon70), 
                  nmon67 = c(rfm67$totNumMonths, nmon),
                  nmonB67 = c(rfm67$sumBel67Months, nmon67),
                  status = c(rep('Repro Fem', times = nrow(rfm70)), 'Unimpacted Repro Fem'))
