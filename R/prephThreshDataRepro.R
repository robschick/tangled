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
#' @return A list containing: 1) \code{rfmthold} A 6 by 6 data frame 
#'     summarising the information about the number of months entangled
#'     whales are spending below certain health thresholds, and 2)
#'     \code{dfout} a 113 x 5 data frame containing the threshold
#'     data at the level of the infidividual, i.e. not summarised like
#'     in \code{rfmthold}
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
#' @export
#' @examples 
#' \dontrun{
#' prephThreshDataRepro(healthmean, thold)     
#' }
prephThreshDataRepro <- function(healthmean, thold){

  dfout <- numeric(0)
  
  for(i in 1:nrow(tangRepro)){
    
    ind <- tangRepro$EGNo[i]
    tsub <- tangRepro[i, ]
    htest <- healthmean[which(ID == ind),]
    hsd <- healthsd[which(ID == ind),]
    
    s <- match(tsub[, 'swindmonyr'], myName)  
    e <- match(tsub[, 'ewindmonyr'], myName)
    
    gstat <- tsub[, 'gearInj']
    
    hVal <- htest[s:e] # this gets all the health values, but will be ragged
    hsd_Val <- hsd[s:e]
    lh <- length(hVal)
    
    pthold <- lthold <- numeric(0)
    for(j in 1:1000){
      new_health <- rnorm(lh, hVal, hsd_Val)
      lthold[j] <- length(which(new_health < thold))
      pthold[j] <- (lthold[j] / lh ) * 100  
    }
    
    # need to put these together across animals; above is just one animal
    
    dfi <- data.frame(egno = ind, nmonths = lh, monthold = mean(lthold, na.rm = TRUE), 
                      pctmonthold = mean(pthold, na.rm = TRUE), gearInjury = gstat) 
    dfout <- rbind(dfout, dfi)
  }
  
  rfmthold <- dfout %>% 
    group_by(gearInj) %>% 
    summarise(n = n(),
              totNumMonths = sum(nmonths),
              maxDurMonths = max(nmonths),
              sumBeltholdMonths = sum(monthold),
              meanpthold = mean(pctmonthold))
  
  
  list(rfmthold = rfmthold, dfout = dfout)
}

