#' Plot of Kaplan-Meier survival curve for all entangled whales.
#' 
#' \code{plotSurvGenderSeverity} returns a plot of survivorship be severity and gender
#' 
#' This is a function that will build a \code{ggplot2} object that displays
#' the KM curve along with the times animals get censored. The median estimates
#' of survivorship and censored times are used to make the main line. This comes
#' from the first element of each list that is passed to the function. 
#' 
#' The idea behind this function is to show the uncertainty in survivorship
#' that arises from the different estimates of death in each animal.
#' 
#' The main difference from \code{plotSurv} is that this breaks out overall
#' survival and produces three lines for each of the three entanglement
#' categories as well as plotting facets for each gender
#' 
#' @param kmlines A list with \code{nboot} components that contains a data frame
#'   in each component. The data frame is the KM curve based on samples
#'   of death times for the presumed dead animals. The first component of the
#'   list is the median estimate of survivorship. This will be used as the
#'   main KM curve. The other components will be used to display uncertainty.
#' @param censticks A list with \code{nboot} components that contains a data frame
#'   in each component. The data frame contains information on when the animal
#'   is censored. The changes in eact iteration, and right now the function
#'   is set up to just plot the censored marks from the most probable censored
#'   year.
#' @param yearEnd A matrix where each row contains the estimated death times
#'   for each animal. Times are in the columns from 1:bnt, which allows for
#'   the animal to be alive at the time modelling end.
#' @param increment Scalar representing the temporal unit at which we're 
#'   showing survival.
#' @param increment Scalar representing the temporal unit at which we're 
#'   showing survival.
#' @return A ggplot2 object that can be used to create the output plot
#' @export
#' @examples 
#' \dontrun{
#' plotSurvGenderSeverity(kmlines, censTicks, 7)
#' }
plotSurvGenderSeverity <- function(kmlines, censTicks, yearEnd, increment) {
  
  plotdf <- as.data.frame(data.table::rbindlist(kmlines))
  plotdf$genderLab <- NA
  plotdf$genderLab[plotdf$gender == 'M'] <- 'Male'
  plotdf$genderLab[plotdf$gender == 'F'] <- 'Female'
  plotdf$newgroup <- paste(plotdf$group, plotdf$sev, sep = '.')
  
  cplotdf <- as.data.frame(data.table::rbindlist(censTicks))
  cplotdf$genderLab <- NA
  cplotdf$genderLab[cplotdf$gender == 'M'] <- 'Male'
  cplotdf$genderLab[cplotdf$gender == 'F'] <- 'Female'
  cplotdf$newgroup <- paste(cplotdf$group, cplotdf$sev, sep = '.')
  
  p <- ggplot(data = plotdf, aes(interval, psurv, group = newgroup, colour = sev)) + 
    # geom_step(aes(y = jitter(psurv, 5), group = newgroup), alpha = 0.15, colour = 'grey50') + 
    geom_step(data = subset(plotdf, group == 'iter1')) +
    ylim(0, 1) + 
    geom_segment(data = subset(cplotdf, group == 'iter1'), 
                 aes(x = censMonth0 / increment, y = psurv, 
                     xend = censMonth0 / increment, yend = psurv + 0.015)) + 
    labs(y = 'Survivorship', x = 'Time Following End of Entanglement')+
    theme_bw()+
    theme(panel.grid.major = element_line(size = 1.25), panel.grid.minor = element_line(size = 1))+
    scale_y_continuous(expand = c(0, 0.05))+
    scale_colour_brewer(palette = 'Dark2', name = 'Entanglement\nInjury',
                        labels = c('Minor (F = 135, M = 169)', 'Moderate (F = 52, M = 59)', 'Severe (F = 23, M = 26)'))+
    theme(legend.position = c(.15, .15))+
    coord_cartesian(xlim = c(0, yearEnd))+
    facet_grid(. ~ genderLab)
  
  return(p)
}