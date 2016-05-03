#' Prepare the data for barplot portion of the slope health plot
#'
#' This function starts with the \code{dfout} data, which is prepared
#' in \code{prepSlopeHealthData.R} and calculates the change during 
#' two different periods: 1) the entanglement window; and 2) the
#' recovery window. The goal is to show the magnitude of change
#' that is depicted with each different line plot in the upper
#' part of the graph.
#' 
#' @return \code{dfAsuml} A data frame containing the difference health 
#' in health anomaly in the two windows for each of the 6 conditions, 
#' i.e. health during and after for each of 6 entanglement types
#' @export
#' @examples
#' prepSlopeHealthDataBarplot(dfout)
prepSlopeHealthDataBarplot <- function(dfout){
  
  dfAsum <- dfout %>% 
    group_by(gearInj) %>% 
    summarise(sAnom = median(startAnom, na.rm = TRUE), 
              endAnom = median(endAnom, na.rm = TRUE), 
              recAnom = median(recAnom, na.rm = TRUE),
              fullLab = unique(fullLab),
              sevLab = unique(sevLab),
              gearLab = unique(gearLab))

  dfAsuml <- dfAsum %>% 
    group_by(fullLab, sevLab, gearLab) %>% 
    summarise(diff1 = endAnom - sAnom,
              diff2 = recAnom - endAnom) %>% 
    reshape2::melt(id.vars = c('fullLab', 'sevLab', 'gearLab'))
  
  dfAsuml
}
