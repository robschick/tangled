#' Plot of Kaplan-Meier survival curve for all entangled whales.
#' 
#' \code{plotSurv} returns a matrix of all of the estimated death times for each animal in the model
#' 
#' This is a function that will build a \code{ggplot2} object that displays
#' the KM curve along with the times animals get censored. The median estimates
#' of survivorship and censored times are used to make the main line. This comes
#' from the first element of each list that is passed to the function. 
#' 
#' The idea behind this function is to show the uncertainty in survivorship
#' that arises from the different estimates of death in each animal.
#' 
#' @param kmlines A list with nboot components that contains a data frame
#'   in each component. The data frame is the KM curve based on samples
#'   of death times for the presumed dead animals. The first component of the
#'   list is the median estimate of survivorship. This will be used as the
#'   main KM curve. The other components will be used to display uncertainty.
#' @param censticks A list with nboot components that contains a data frame
#'   in each component. The data frame contains information on when the animal
#'   is censored. The changes in eact iteration, and right now the function
#'   is set up to just plot the censored marks from the most probable censored
#'   year.
#' @param yearEnd A matrix where each row contains the estimated death times
#'   for each animal. Times are in the columns from 1:bnt, which allows for
#'   the animal to be alive at the time modelling end.
#' @param increment Scalar representing the temporal unit at which we're 
#'   showing survival.
#' @return A ggplot2 object that can be used to create the output plot
#' @export
#' @examples 
#' \dontrun{
#' plotSurv(kmlines, censTicks, 7)
#' }
plotSurv <- function(kmlines, censTicks, yearEnd, increment) {
  plotdf <- as.data.frame(data.table::rbindlist(kmlines))

  p <- ggplot(data = subset(plotdf, group == 'iter1'), aes(interval, psurv, group = group)) + 
    geom_step(data = subset(plotdf, group != 'iter1'), aes(y = jitter(psurv, 5), group = group), alpha = 0.05) + 
    geom_step() + 
    geom_segment(data = censTicks[[1]], aes(x = censMonth0 / increment, y = psurv, xend = censMonth0 / increment, yend = psurv + 0.015)) + 
    labs(y = 'Survivorship', x = 'Years Following End of Entanglement Injury')+
    theme_bw()+
    theme(panel.grid.major = element_line(size = 1.25), panel.grid.minor = element_line(size = 1))+
    scale_y_continuous(expand = c(0, 0.05))+
    xlim(0, yearEnd)
  
  return(p)
}