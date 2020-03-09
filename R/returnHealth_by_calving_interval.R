#' Return the health data for calving females during intervals
#' 
#' Calculate health by interval...
#' 
#' @return XXX 
#' @export
#' @examples
#' \dontrun{
#' returnHealth_by_calving_interval()
#' }
returnHealth_by_calving_interval <- function(anomFlag = FALSE){

  useAnom <- anomFlag
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
    healthmean <- sumh / ng
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
    if(ind == 1045) next()
    sidx <- match(tangleOut$smonyr[i], myName)
    eidx <- match(tangleOut$emonyr[i], myName)
    nonrepHealth[which(row.names(nonrepHealth) == ind), sidx:nt] <- NA
    sevidx <- match(tangleOut$fsevmonyr[i], myName)
    if (!is.na(sevidx)) {
      nonrepHealth[which(row.names(nonrepHealth) == ind), sevidx:nt] <- NA # set post-severe times  
    }
  }

  # update for bringing in health by decade:
  s1980 <- which(myName == "1-1980")
  s1990 <- which(myName == "1-1990")
  s2000 <- which(myName == "1-2000")
  s2010 <- which(myName == "1-2010")
  
  hrep1980 <- repHealth[, s1980:(s1990 - 1)]
  hnonrep1980 <- nonrepHealth[, s1980:(s1990 - 1)]
  
  hrep1990 <- repHealth[, s1990:(s2000 - 1)]
  hnonrep1990 <- nonrepHealth[, s1990:(s2000 - 1)]
  
  hrep2000 <- repHealth[, s2000:(s2010 - 1)]
  hnonrep2000 <- nonrepHealth[, s2000:(s2010 - 1)]
  
  
  repvec <- as.vector(repHealth)[!is.na(as.vector(repHealth))]
  nonrepvec <- as.vector(nonrepHealth)[!is.na(as.vector(nonrepHealth))]

  repvec1980 <- as.vector(hrep1980)[!is.na(as.vector(hrep1980))]
  nonrepvec1980 <- as.vector(hnonrep1980)[!is.na(as.vector(hnonrep1980))]
  
  repvec1990 <- as.vector(hrep1990)[!is.na(as.vector(hrep1990))]
  nonrepvec1990 <- as.vector(hnonrep1990)[!is.na(as.vector(hnonrep1990))]
  
  repvec2000 <- as.vector(hrep2000)[!is.na(as.vector(hrep2000))]
  nonrepvec2000 <- as.vector(hnonrep2000)[!is.na(as.vector(hnonrep2000))]
  
  list(nonrep = nonrepvec, rep = repvec,
       rep1980 = repvec1980, nonrep1980 = nonrepvec1980,
       rep1990 = repvec1990, nonrep1990 = nonrepvec1990,
       rep2000 = repvec2000, nonrep2000 = nonrepvec2000)

}