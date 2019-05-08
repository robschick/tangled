#' Prepare the median line data for slope health plot
#'
#' This function starts with \code{dfout} and calculates median summary
#' lines to be used in the plotting of the upper panel of the slope plot
#' 
#' @return \code{dfoutMed} A data frame containing the median health and 
#' health anomaly for each entanglement event at each of the three 
#' locations
#' @export
#' @examples
#' \dontrun{
#' prepSlopeHealthDataMedian(dfout)
#' }
prepSlopeHealthDataMedian <- function(dfout){
  
  dfAsum <- dfout %>% 
    group_by(gearInj) %>% 
    summarise(sHealth = median(startHealth, na.rm = TRUE), 
              endHealth = median(endHealth, na.rm = TRUE), 
              recHealth = median(recHealth, na.rm = TRUE),
              sAnom = median(startAnom, na.rm = TRUE), 
              endAnom = median(endAnom, na.rm = TRUE), 
              recAnom = median(recAnom, na.rm = TRUE),
              fullLab = unique(fullLab),
              sevLab = unique(sevLab),
              gearLab = unique(gearLab))
  dfoutMed <- reshape2::melt(dfAsum, id.vars = c('gearInj', 'fullLab', 'sevLab', 'gearLab'))
  dfoutMed$x <- rep(0:2, each = 6)
  dfoutMed
 
}
