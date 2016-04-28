#' Return the differences in health between individuals and pop average
#' 
#' Goal of this is to take in the individual health data, along with the
#' population health data, and calculate the anomaly between the two
#' @param ind - a 2 by t data frame of time-stamped individual health
#' @param pop - a 3 by T data frame of time-stamped population health with SD
#' @return hdiff - the health anomaly
calcAnom <- function(ind, pop){
  # Inputs: 
  # ind - a 2 by t data frame of time-stamped individual health
  # pop - a 3 by T data frame of time-stamped population health with SD
  
  hdat <- merge(ind, pop, by = 'dateTime')
  hdiff <- hdat$popHealth - hdat$indHealth
  return(hdiff)
}