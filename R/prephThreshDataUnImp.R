#' Prepare data frame of health in unimpacted females for \% below barplot
#' 
#' Goal is to look at the amount of time (in months) the whales are spending
#' below certain health thresholds. The idea is to see if female whales
#' are spending times in periods of low health and as a result have 
#' delayed or impaired fecundity. This function returns the health of 
#' these animals
#' 
#' @param \code{healthmean} A 696 by 564 matrix of estimated health
#'     for all animals in the catalog dataset
#' @param \code{firstSight} A length 696 vector of the first month
#'     each animal was seen
#' @param \code{lastSight} A length 696 vector of the last month
#'     each animal was seen
#' @param \code{thold} A scalar value of the health threshold to be
#'     examined.     
#' @return \code{healthnew} a 23 by 564 matrix of health estimates for 
#'     unimpacted females calving females before they've had their first
#'     calf, that is we don't want to examine health after the animals
#'     have had a calf and are subject to natural fluctuations related
#'     to the calving and nursing cycle.
#' @return \code{nAnimal} the number of unimpacted females
#' @export
#' @examples 
#' \dontrun{
#' prephThreshDataUnImp(healthmean, firstSight, lastSight, thold)
#' }
prephThreshDataUnImp <- function(healthmean, firstSight, lastSight, thold){

  # this will be for the unimpacted calving females:
  healthurf <- healthmean
  for(i in 1:nrow(healthurf)){
    healthurf[i, 1:firstSight[i]] <- NA
    healthurf[i, lastSight[i]:nt] <- NA
  }

  # for females are in calf table but not in tangle I want stretches of health and after or including the first pregnancy year 
  # I'll find the females in calfTable, and not in tangle. In this case I'm using tangle, because it contains all entangled whales
  # even if they don't have a valid start date
  idx <- match(calfTable$EGNo, tangleAll$EGNo)
  ctabSub <- calfTable[which(is.na(idx)), ]
  ctabid <- ctabSub %>% 
    group_by(EGNo) %>% 
    summarise(pregyear = min(CalvingYear) - 2)
  # ctabid <- ctabid[-which(ctabid$EGNo == 1045),]

  ctabid$monyr <- paste('12-', ctabid$pregyear, sep = '')# this gets the December before the pregnancy year

  # essentially all animals that are not in this category are set to NA
  idx <- match(ID, ctabid$EGNo)
  healthnew <- healthurf[which(is.finite(idx)),]
  cID <- ID[which(is.finite(idx))]

  # And then for the animals that are in the category, any times before their first pregnancy year are set to NA
  for(i in 1:nrow(ctabid)){
   iint <- match(ctabid$monyr[i], myName)
   healthnew[cID == ctabid$EGNo[i], 1:iint] <- NA  
  }
  
  nmon <- length(which(is.finite(healthnew)))
  nmonInd <- apply(healthnew, 1, function(x) length(which(is.finite(x))))
  nmonThold <- length(which(healthnew < thold))
  nmonTholdInd <- apply(healthnew, 1, function(x) length(which(x < thold)))
  pThold <- (nmonThold / nmon) * 100
  numAn <- nrow(healthnew)
  
  list(healthnew = healthnew, nmon = nmon, nmonInd = nmonInd,
       nmonThold = nmonThold, nmonTholdInd = nmonTholdInd, pThold = pThold,
       pTholdInd = (nmonTholdInd / nmonInd) * 100, nAnimal = numAn)
       
       
}

