#' Return the right whale population level health Anomaly as a data frame
#' 
#' @param ngg A scalar corresponding to the number of Gibbs steps
#' @param tIndex A vector containing values 1:nt
#' @param monYr An nt by 2 matrix containing months in the first column and years in the second
#' @param sumh A matrix containing the estimates of right whale health for all whales
#' @param newgibbsMales A matrix containing the estimates of right whale health for Adult Males
#' @param newgibbsJuvs1 A matrix containing the estimates of right whale health for Young Juveniles
#' @param newgibbsJuvs3 A matrix containing the estimates of right whale health for Old Juveniles
#' 
#' @return A data frame of right whale population level health anomaly, e.g. observed health (matrix) - pop health (vector)
#' @examples
#' returnhealthAnom(healthmean)
returnhealthAnom <- function(healthmean) {
  
  healthmeanM <- newhgibbsMales / g
  healthmeanJ1 <- newhgibbsJuvs1 / g
  healthmeanJ2 <- newhgibbsJuvs3 / g

  for(i in 1:nrow(healthmean)){
    
    healthmeanM[i, 1:(firstSight[i] - 1)] <- NA
    healthmeanJ1[i, 1:(firstSight[i] - 1)] <- NA
    healthmeanJ2[i, 1:(firstSight[i] - 1)] <- NA

  }
  
  newPopCat <- rbind(healthmeanM, healthmeanJ1, healthmeanJ2)
  newPopMedian <- apply(newPopCat, 2, median, na.rm = TRUE)
  
  popAnom <- sweep(healthmean, 2, newPopMedian)
  
  popAnom
}

