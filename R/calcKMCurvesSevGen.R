#' Return lists to be used in plotting Kaplan-Meier survival curves by gender and entanglement severity
#' 
#' This function is very similar to \code{calcKMCurves()} in that it takes the 
#' output from the functions used to make the survival data for the known 
#' dead, presumed alive, and presumed dead animals, and prepare the survivorship
#' information needed to plot the KM curves. Like that function, we can vary 
#' the interval over which the survivorship is calculated. And we can store 
#' uncertainty around the death times through the use of a boot strap. Where
#' it differs is that it is designed to examine survivorship split out
#' by gender and by entanglement severity.
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
#' @export
#' @examples 
#' \dontrun{
#' calcKMCurvesSevGen(survdf, kdpasurvldf, nboot = 1, dcut, 
#'     increment = 12, medProb = TRUE)
#' }
#' @return A list with two elements: 1) \code{kmlines} and 2) \code{censTicks}.
#'     The first element is a data frame of the survivorship for the different
#'     time frames. The second element is a data frame of censor times - simply
#'     used to make tick marks of when the animal was removed from the study
#'     via censoring.
calcKMCurvesSevGen <- function(survdf, kdpasurvldf, nboot = 1, dcut, increment = 12, medProb = TRUE){
  
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
      deathSamp <- getDeaths(deathyr, medProb = TRUE)
      enew <- presDeadsurvdat(events, dcut, deathSamp)
      survdf <- rbind(enew, medsurvdf)  
    }
  
    # to group the data into temporal units, e.g. yearly
    survdf$gender <- gender[match(survdf$EGNo, ID)]
    survdf$yearInt <- findInterval(survdf$survTime0, seq(0, nt, by = increment)) 
    survdf$censYearInt <- findInterval(survdf$censMonth0, seq(0, nt, by = increment))
    survdf$deathyearInt <- findInterval(survdf$deathMonth0, seq(0, nt, by = increment))
    svvec <- c("moderate", "minor", "severe" )
    kmdfAll <- numeric(0) 
    csubAll <- numeric(0)

    for(g in c("M", "F")) {
      
      for(j in 1:3){
    
        dsub <- survdf[survdf$Severity == svvec[j] & survdf$gender == g, ]
        kmdf <- data.frame(interval = 0, atRiskStart = nrow(dsub), censored = 0,
                           diedOnInterval = 0, atRiskEnd = 0, propSurv = 1, gender = g)
        
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
                                         atRiskEnd = atRiskEnd, propSurv = proportionSurviving, gender = g))
        }
        
        kmdf$psurv <- cumprod(kmdf$propSurv)
        kmdf$sev <- svvec[j]
        kmdfAll <- rbind(kmdfAll, kmdf)
        kmdfAll$genderLab <- kmdfAll$gender
        kmdfAll$genderLab[kmdfAll$genderLab == 'M'] <- 'Male'
        kmdfAll$genderLab[kmdfAll$genderLab == 'F'] <- 'Female'
        kmlines[[nb]] <- data.frame(kmdfAll, group = paste('iter', nb, sep = ''))
        
        # I want a censored month in here as well for plotting purposes
        csub <- subset(dsub, censored == TRUE)
        csub <- csub[!duplicated(csub$EGNo),]
        psvec <- rep(NA, nrow(csub))# to store the death months
        
        for(i in 1:nrow(csub)){
          interval <- csub[i, 'censYearInt']
          psvec[i] <- kmdf[kmdf$interval == (interval - 1), 'psurv']
        }
        
        csub$psurv <- psvec
        csub$sev <- svvec[j]
        
        csubAll <- rbind(csubAll, csub)
        csubAll$gender <- gender[match(csubAll$EGNo, ID)]
        csubAll$genderLab <- csubAll$gender
        csubAll$genderLab[csubAll$genderLab == 'M'] <- 'Male'
        csubAll$genderLab[csubAll$genderLab == 'F'] <- 'Female'
        censTicks[[nb]] <- data.frame(csubAll, group = paste('iter', nb, sep = ''))
        
        } # end loop over severity
    
      } # gender loop

  } # end loop over nboot

  list(kmlines = kmlines, censTicks = censTicks)
  
}  
