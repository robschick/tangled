#' Return the health data for unimpacted whales (reproductively active and non-reproductively active)
#' 
#' This function creates and returns two data frames that will be used
#' in the plotting of health during the entanglement windows. IN particular
#' this function makes data frames that will be used as the reference cases
#' of health for non-impacted whales, i.e. what does the health look like
#' for animals that are not entangled. If a whale does at some point become
#' entangled, then we will only include their health (anomaly) estimates
#' up to the point of the first entanglement. This needs to be called
#' in order to make the boxplots
#' 
#' @return A list element containing 2 data frames for health of the 1) 
#'    unimpacted and reproductive active whales and 2) unimpacted and 
#'    non-reproductive active whales. 
#' @export
#' @examples
#' \dontrun{
#' returnUnimpactedHealth()
#' }
returnUnimpactedHealth <- function(){

  useAnom <- TRUE
  death <- getDeaths(deathyr, medProb = TRUE)
  if (useAnom){
    healthmean <- anom
    for(i in 1:nrow(healthmean)){
      healthmean[i, 1:(firstSight[i] - 1)] <- NA
      if (death[i] < nt) {
        healthmean[i, (death[i] + 1):nt] <- NA  
      }
    }
  } else {
    healthmean <- sumh / g
    for(i in 1:nrow(healthmean)){
      healthmean[i, 1:(firstSight[i] - 1)] <- NA
      if (death[i] < nt) {
        healthmean[i, (death[i] + 1):nt] <- NA
      }
    }
  }

  # get the reproductively active females set up - use tangRepro
  # first use calfTable to parse out healthmean into a subset of reproductively active females
  repidx <- match(unique(calfTable$EGNo), ID)
  repHealth <- healthmean[repidx, ]

  # only get health values during times _after_ first year of calving; use firstCalfidx
  fcalf <- tangRepro[, c('EGNo', 'firstCalfidx')]
  idx <- duplicated(fcalf)
  fcalf <- fcalf[!idx, ]

  for (i in 1:length(fcalf$EGNo)) {
    ind <- fcalf$EGNo[i]
    repHealth[which(row.names(repHealth) == ind), 1:fcalf$firstCalfidx[i]] <- NA
  }

  # Set to NA any health values for times after severe entanglement events
  # Have the unimpacted times be prior to a first entanglement

  for (i in 1:nrow(tangRepro)) {
    ind <- tangRepro$EGNo[i]
    sidx <- match(tangRepro$smonyr[i], myName)
    eidx <- match(tangRepro$emonyr[i], myName)
    repHealth[which(row.names(repHealth) == ind), sidx:nt] <- NA # this is per Amy's request 12/4/15
    sevidx <- match(tangRepro$fsevmonyr[i], myName)
    if (!is.na(sevidx)) {
      repHealth[which(row.names(repHealth) == ind), sevidx:nt] <- NA # set post-severe times  
    }
  }

  ###############################################################
  # get the other animals set up - use tangleOut
  nonrepHealth <- healthmean

  # to get health times before first year of calving, use firstCalfidx
  # to set all months after first calf to NA
  for (i in 1:length(fcalf$EGNo)) {
    ind <- fcalf$EGNo[i]
    nonrepHealth[which(row.names(nonrepHealth) == ind), fcalf$firstCalfidx[i]:nt] <- NA
  }

  # set unimpacted times &  set post-severe times
  for (i in 1:nrow(tangleOut)) {
    ind <- tangleOut$EGNo[i]
    sidx <- match(tangleOut$smonyr[i], myName)
    eidx <- match(tangleOut$emonyr[i], myName)
    nonrepHealth[which(row.names(nonrepHealth) == ind), sidx:nt] <- NA
    sevidx <- match(tangleOut$fsevmonyr[i], myName)
    if (!is.na(sevidx)) {
      nonrepHealth[which(row.names(nonrepHealth) == ind), sevidx:nt] <- NA # set post-severe times  
    }
  }

  repvec <- as.vector(repHealth)[!is.na(as.vector(repHealth))]
  nonrepvec <- as.vector(nonrepHealth)[!is.na(as.vector(nonrepHealth))]
  list(nonrep = nonrepvec, rep = repvec)

}