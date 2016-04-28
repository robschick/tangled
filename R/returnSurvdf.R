#' Return survival data frame for testing differences in the curves
#' 
#' We want to statistically test the differences between survival
#' as a function of gender and entanglement severity. So this function
#' will return an object formatted for use in the \code{survival}
#' package.
#' 
#' @examples
#' returnSurvdf(survdf)
returnSurvdf <- function(survdf){
  
  survSub <- distinct(survdf, EGNo)
   
#   nrow(survSub)
#   nrow(filter(survSub, censored == TRUE))
#   nrow(filter(survSub, knownDeath == TRUE))
#   nrow(filter(survSub, knownDeath == FALSE && censored == FALSE))
   
  df1 <- dplyr::select(filter(survSub, censored == TRUE & knownDeath == FALSE), c(1, 3, 7, 10, 6, 8, 9))
  df2 <- dplyr::select(filter(survSub, knownDeath == TRUE), c(1, 3, 7, 10, 6, 8, 9))
  df3 <- dplyr::select(filter(survSub, censored == FALSE & knownDeath == FALSE), c(1, 3, 7, 10, 6, 8, 9))
  
  dfSurv <- rbind(df1, df2, df3)
  dfSurv$futime <- NA
  dfSurv$event  <- NA
  
  dfSurv$futime[dfSurv$censored == TRUE & dfSurv$knownDeath == FALSE] <- dfSurv$censMonth0[dfSurv$censored == TRUE & dfSurv$knownDeath == FALSE]
  dfSurv$futime[dfSurv$knownDeath == TRUE] <- dfSurv$deathMonth0[dfSurv$knownDeath == TRUE]
  dfSurv$futime[dfSurv$censored == FALSE & dfSurv$knownDeath == FALSE] <- dfSurv$deathMonth0[dfSurv$censored == FALSE & dfSurv$knownDeath == FALSE]
  dfSurv$event[dfSurv$censored == TRUE & dfSurv$knownDeath == FALSE] <- 0
  dfSurv$event[dfSurv$knownDeath == TRUE] <- 1
  dfSurv$event[dfSurv$censored == FALSE & dfSurv$knownDeath == FALSE] <- 1
  
  dfSurv$gender <- gender[match(dfSurv$EGNo, ID)]
  
  dfSurv
}