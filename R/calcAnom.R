calcAnom <- function(ind, pop){
  # This function will return the differences in health between
  # and individual right whale, and the population level average
  # health. 
  # Initial Coding by Rob Schick, August 2014, 
  # University of St Andrews
  #
  # Inputs: 
  # ind - a 2 by t data frame of time-stamped individual health
  # pop - a 3 by T data frame of time-stamped population health with SD
  
  hdat <- merge(ind, pop, by = 'dateTime')
  hdiff <- hdat$popHealth - hdat$indHealth
  return(hdiff)
}