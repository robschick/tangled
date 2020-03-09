#' Matrix of imputed death times
#' 
#' \code{getDeaths} returns a matrix of all of the estimated death times for each animal in the model
#' 
#' This is a function that will build an object to be used in the construction
#' of the Kaplan-Meier survival curves for right whales. The idea behind this
#' is to loop over the \code{deathyr} matrix and sample a death time for each
#' animal. The sampling is done with a call to \code{rmultinom} based on the 
#' estimates of death. These estimates are normalised, and then we choose a
#' candidate month. 
#' 
#' @param deathyr A matrix where each row contains the estimated death times
#'   for each animal. Times are in the columns from 1:bnt, which allows for
#'   the animal to be alive at the time modelling end.
#' @param \code{medProb} logical indicating whether you want to sample
#'   from the deaths using a multinomial, or just extract the median
#'   death month (default)
#' @return The output will be a vector of sampled death times. Each column
#'   will represent an individual animal's sampled times. 
#' @export
#' @examples 
#' getDeaths(deathyr, medProb = TRUE)
getDeaths <- function(deathyr, medProb = TRUE) {
  
  if(medProb) {
    deaths <- apply(deathyr, 1, function(x) which.max(x))  
  } else {
    psamp <- deathyr / rowSums(deathyr, na.rm = TRUE)
    deaths <- apply(psamp, 1, function(x) which(rmultinom(1, 1, prob = x) == 1))    
  }
  
  names(deaths) <- rownames(deathyr)
  deaths
}