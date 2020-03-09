#' Prepare the data for annotating the slope health plot
#'
#' This function starts with the \code{dfout} data frame, and puts that
#' through a \code{dplyr} wringer to calculate the number of cases
#' in each of the 6 different entanglement types.
#' 
#' @return \code{dfn} A data frame containing the tally of the number of
#' events in each entanglement type.
#' @export
#' @examples
#' \dontrun{
#' prepSlopeHealthDataAnnotation(dfout)
#' }
prepSlopeHealthDataAnnotation <- function(dfout){
  
  dfn <- dfout %>% 
    group_by(gearInj) %>% 
    summarise(n = n())
  dfn

}
