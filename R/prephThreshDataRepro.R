#' Assemble health of impacted reproductively active females
#' 
#' This function takes heatlh estimates and intersects them
#' with the entanglement information to find out the number
#' of months each animal spent below a certain health threshold
#' 
#' @param \code{healthmean} Estimates of health from the model
#' @param \code{thold} A scalar value of the health threshold to be
#'     examined.     
#'     
#' @return \code{rfmthold} A 6 by 6 data frame summarising the 
#'     information about the number of months entangled
#'     whales are spending below certain health thresholds
#' \describe{
#'   \item{gearInj}{The entanglement injury class of the animal. 
#'       1 = Severe w/Gear; 2 = Moderate w/Gear; 3 = Severe no gear;
#'       4 = Minor w/Gear; 5 = Moderate no gear; 6 = Minor no gear}
#'   \item{n}{The number of cases in each of these categories}
#'   \item{totNumMonths}{The total number of months tallied up
#'       from each individual whale, i.e. total number of
#'       months all whales were entangled}
#'   \item{maxDurMonths}{Maximum length of any entanglement}
#'   \item{sumBeltholdMonths}{Sum of the number of months
#'       entangled whales were below the threshold}
#'   \item{meanpthold}{The mean (by entanglement) percentage 
#'       of months the animal was below a threshold value
#'       during the event}     
#' }
#' @examples 
#' prephThreshDataRepro(healthmean, thold)     
prephThreshDataRepro <- function(healthmean, thold){

  dfout <- numeric(0)
  
  for(i in 1:nrow(tangRepro)){
    
    ind <- tangRepro$EGNo[i]
    tsub <- tangRepro[i, ]
    htest <- healthmean[which(ID == ind),]
    
    s <- match(tsub[, 'swindmonyr'], myName)  
    e <- match(tsub[, 'ewindmonyr'], myName)
    
    gstat <- tsub[, 'gearInj']
    
    hVal <- htest[s:e] # this gets all the health values, but will be ragged
    lh <- length(hVal)
    lthold <- length(which(hVal < thold))
    pthold <- (lthold / lh ) * 100
    
    dfi <- data.frame(egno = ind, nmonths = lh, monthold = lthold, 
                      pctmonthold = pthold, gearInjury = gstat) 
    dfout <- rbind(dfout, dfi)
  }
  
  rfmthold <- dfout %>% 
    group_by(gearInj) %>% 
    summarise(n = n(),
              totNumMonths = sum(nmonths),
              maxDurMonths = max(nmonths),
              sumBeltholdMonths = sum(monthold),
              meanpthold = mean(pctmonthold))
  rfmthold

}

