#' Data frame of survival information for presumed dead animals
#' 
#' \code{presDeadsurvdat} returns a matrix of all of the estimated death times for each animal in the model
#' 
#' This is a function that will build an object to be used in the construction
#' of the Kaplan-Meier survival curves for right whales. The idea behind this
#' is to loop over each animal that is presumed dead and calculate the
#' necessary informaiton for the survival curve plotting. 
#' 
#' It will take in a new vector of death times (\code{newDeadtimes}), and replace the median
#' death times in \code{events} with the newly sampled ones.
#' 
#' @param events A data frame of the entanglement data: timing, severity, etc.
#' @param dcut An numerical index between 1 and \code{nt} that is used to determine 
#' when animals are censored.
#' @param newDeadtimes a named vector of estimated death times. This is used to update
#' the events data, so that new survival information can be calculated.
#' @return The output will be a data frame of survival information based on the
#' imputed death times.
#' @example 
#' presDeadsurvdat(events, dcut, newDeadtimes)
#' 
presDeadsurvdat <- function(events, dcut, newDeadtimes) {
  
  esub <- subset(events, presD)
  esub[, 'dtimenew'] <- NA
  dtimenew <- as.vector(newDeadtimes[match(esub$EGNo, names(newDeadtimes))])
  esub[, 'dtimenew'] <- dtimenew
  survl <- vector(mode = 'list', nrow(esub))
  kd <- FALSE 
  
  for(i in 1:nrow(esub)){
    id <- esub[i, 'EGNo']
    dmonth <- as.numeric(esub[i, 'dtimenew'])
    emonth <- as.numeric(esub[i, 'ewindmonyrID'])
    censor <- ifelse(dmonth > dcut, TRUE, FALSE)
    cmonth <- dcut
    dmonth2 <- dmonth 
    svec <- seq(emonth, dmonth2)
    svec0 <- svec - min(svec)
    csvec <- seq(emonth, cmonth)
    csvec0 <- csvec - min(csvec)
    survl[[i]] <- data.frame(EGNo = id, deathMonth = dmonth, censored = censor, censMonth = cmonth, survTime0 = svec0, 
                             deathMonth0 = max(svec0), censMonth0 = max(csvec0), severity = esub[i, 'Severity'], 
                             sevNumClass = esub[i ,'gearInj'], knownDeath = kd)
  }
  
  
  survdf <- as.data.frame(data.table::rbindlist(survl))
  survdf$censMonth[survdf$censMonth < dcut] <- NA
  survdf 
}

