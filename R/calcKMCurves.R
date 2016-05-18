#' Return lists to be used in plotting Kaplan-Meier survival curves
#' 
#' This function takes the output from the functions used to make
#' the survival data for the known dead, presumed alive, and presumed
#' dead animals, and prepare the survivorship information needed to 
#' plot the KM curves. If needed, we can vary the interval over which
#' the survivorship is calculated. And we can store uncertainty
#' around the death times through the use of a boot strap
#' 
#' @param \code{survdf} is the data frame containing survival data for all
#'     animals in the three possible categories. 
#' @param \code{kdpasurvldf} is the data frame containing survival data for
#'     just the known dead and presumed alive animals
#' @param \code{nboot} an integer indicating the number of times we want to
#'     run a bootstrap to collect uncertainty around the survivorship. Default
#'     is to not run a boot strap
#' @param \code{dcut} integer indicating the month-year combo when the sightings 
#'     data are considered complete through
#' @param \code{increment} the unit of time overwhich we calculate survivorship
#'     Default value is to calculate it over a yearly interval
#' @param \code{medProb} A logical indicating whether or not the median estimate
#'     of death is returned from the \code{getDeaths()} function. If TRUE, yes. 
#'     If FALSE, then a value sampled from the posterior with normalised
#'     probability for all candidate death months
#' @return A list with two elements: 1) \code{kmlines} and 2) \code{censTicks}.
#'     The first element is a data frame of the survivorship for the different
#'     time frames. The second element is a data frame of censor times - simply
#'     used to make tick marks of when the animal was removed from the study
#'     via censoring.
#' @export     
#' @examples 
#' \dontrun{
#' calcKMCurves(survdf, kdpasurvldf, nboot = 1, dcut, increment = 12, medProb = TRUE)
#' }
calcKMCurves <- function(survdf, kdpasurvldf, nboot = 1, dcut, increment = 12, medProb = TRUE){
>>>>>>> f04602636b4ee9860f6a5b6568da4d9fc6ade942
  
  medsurvdf <- kdpasurvldf
  nt <- dcut
  
  # set up lists to hold output; if we run a bootstrap each
  # output from the bootstrap gets placed into a list element:
  kmlines <- vector(mode = 'list', (nboot)) 
  censTicks <- vector(mode = 'list', (nboot))
  
  for(nb in 1:(nboot)) {
    
    # The default is to _not_ run a bootstrap, which means
    # the survdf input is what we'll use. If we do run 
    # a boot strap, then we'll bind together the newly
    # sampled data along with the median survival data frame
    # which is input to the function:
    if (nb > 1) {
      deathSamp <- getDeaths(deathyr, medProb = medProb)
      enew <- presDeadsurvdat(events, dcut, deathSamp)
      survdf <- rbind(enew, medsurvdf)  
    }
    
    # to group the data into temporal units, e.g. yearly
    survdf$yearInt <- findInterval(survdf$survTime0, seq(0, nt, by = increment)) 
    survdf$censYearInt <- findInterval(survdf$censMonth0, seq(0, nt, by = increment)) 
    survdf$deathyearInt <- findInterval(survdf$deathMonth0, seq(0, nt, by = increment))
    dsub <- survdf
    
    kmdf <- data.frame(interval = 0, atRiskStart = nrow(dsub), censored = 0,
                       diedOnInterval = 0, atRiskEnd = 0, propSurv = 1) 
    
    for(i in 1:length(unique(dsub$yearInt))){
      
      dsubi <- dsub[which(dsub$yearInt == i), ]
      interval <- i
      
      atRiskStart <- length(unique(dsubi$EGNo))
      censored <- length(which(!duplicated(dsubi$EGNo) & dsubi$censored == TRUE & dsubi$deathyearInt == interval))
      died <- length(which(!duplicated(dsubi$EGNo) & dsubi$censored == FALSE & dsubi$deathyearInt == interval))
      atRiskEnd <- atRiskStart - (censored + died)
      proportionSurviving <- atRiskEnd / atRiskStart
      
      kmdf <- rbind(kmdf, data.frame(interval = interval, atRiskStart = atRiskStart, 
                                     censored = censored, diedOnInterval = died, 
                                     atRiskEnd = atRiskEnd, propSurv = proportionSurviving))
    }
    
    kmdf$psurv <- cumprod(kmdf$propSurv)
    kmdf$group <- paste('iter', nb, sep = '')
    kmlines[[nb]] <- kmdf
    
    # I want a censored month in here as well for plotting purposes, i.e. to make the ticks
    csub <- subset(dsub, censored == TRUE)
    csub <- csub[!duplicated(csub$EGNo),]
    
    psvec <- rep(NA, nrow(csub)) # to store the death months
    
    for(i in 1:nrow(csub)){
      
      interval <- csub[i, 'censYearInt']
      psvec[i] <- kmdf[kmdf$interval == (interval - 1), 'psurv']
    
    }
    
    csub$psurv <- psvec
    csub$group <- paste('iter', nb, sep = '')
    censTicks[[nb]] <- csub
    
  } # end bootstrap interval
  
  list(kmlines = kmlines, censTicks = censTicks)
}
