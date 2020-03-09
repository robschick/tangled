#' Return the right whale population level health as a data frame
#' 
#' @param ngg A scalar corresponding to the number of Gibbs steps
#' @param tIndex A vector containing values 1:nt
#' @param monYr An nt by 2 matrix containing months in the first column and years in the second
#' @param newgibbsMales A matrix containing the estimates of right whale health for Adult Males
#' @param newgibbsJuvs1 A matrix containing the estimates of right whale health for Young Juveniles
#' @param newgibbsJuvs3 A matrix containing the estimates of right whale health for Old Juveniles
#' @param newgibbsMales2 A matrix containing the estimates of standard deviation of right whale health for Adult Males
#' @param newgibbsJuvs12 A matrix containing the estimates of standard deviation of right whale health for Young Juveniles
#' @param newgibbsJuvs32 A matrix containing the estimates of standard deviation of right whale health for Old Juveniles
#' 
#' @return A data frame of right whale population level health, comprised of median health between Adult males, and young and old juveniles
#' @export
#' @examples
#' \dontrun{
#' returnPophealth()
#' }
#' 
returnPophealth <- function() {
  
  healthmeanM <- newhgibbsMales / ngg
  healthmeanJ1 <- newhgibbsJuvs1 / ngg
  healthmeanJ2 <- newhgibbsJuvs3 / ngg
  healthmeanMSD <- newhgibbsMales2 / ngg
  healthmeanJ1SD <- newhgibbsJuvs12 / ngg
  healthmeanJ2SD <- newhgibbsJuvs32 / ngg
  
  for(i in 1:nrow(healthmean)){
    
    healthmeanM[i, 1:firstSight[i]] <- NA
    healthmeanJ1[i, 1:firstSight[i]] <- NA
    healthmeanJ2[i, 1:firstSight[i]] <- NA
    healthmeanMSD[i, 1:firstSight[i]] <- NA
    healthmeanJ1SD[i, 1:firstSight[i]] <- NA
    healthmeanJ2SD[i, 1:firstSight[i]] <- NA
    
  }
  
  newPopCat <- rbind(healthmeanM, healthmeanJ1, healthmeanJ2)
  newPopMedian <- apply(newPopCat, 2, median, na.rm = TRUE)
  
  newPopAllVar <- rbind(healthmeanMSD, healthmeanJ1SD, healthmeanJ2SD)
  newPopvar <- newPopAllVar - newPopCat ^ 2
  newpopsd <- sqrt(newPopvar)
  newpopsdmean <- apply(newpopsd, 2, median, na.rm = TRUE)
  
  popplot <- data.frame(time = tIndex, 
                        popHealth = newPopMedian, 
                        hsd = newpopsdmean, 
                        date = makeYearmon(monYr))
  popplot
}

