#' Assemble the data for plotting the barplot of \% below x-health
#' 
#' Goal of this is to take outputs from two different functions: 
#' 1) \code{prephThresDataUnImp.R} and 2) \code{prephThreshDataRepro.R}
#' and assemble a data frame to be used for plotting with the
#' \code{plotHealthThreshold.R} function
#' @param \code{tmp} A list containing the health information from
#'     the unimpacted females. This includes: \code{healthnew}
#'     \code{nmon}, \code{nmonThold}, and \code{pThold}
#' @return \code{rfmthold} A 6 by 7 data frame summarising the 
#'     information about the number of months entangled
#'     whales are spending below certain health thresholds
#' @export     
#' @examples 
#' \dontrun{
#' prepHealthThresholdPlotData(tmp, rfmthold)     
#' }
prepHealthThresholdPlotData <- function(tmp, rfmthold){
  
  gname <- c('Severe - gear', 'Moderate - gear', 'Severe - no gear', 
             'Minor - gear', 'Moderate - no gear', 'Minor - no gear')
  
  dfb <- data.frame(gearInj = c(rfmthold$gearInj, 0),
                    gearName = c(gname, 'Unimpacted'),
                    numAnimals = c(rfmthold$n, tmp$nAnimal),
                    pctthold = c(rfmthold$meanpthold, tmp$pThold),
                    nmonthold = c(rfmthold$totNumMonths, tmp$nmon),
                    nmonBthold = c(rfmthold$sumBeltholdMonths, tmp$nmonThold), 
                    status = c(rep('Repro Fem', times = nrow(rfmthold)), 'Unimpacted Repro Fem'))
  dfb
}