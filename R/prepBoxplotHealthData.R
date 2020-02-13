#' Prepare data for making the boxplots of health during entanglement
#' 
#' The goal of this is to make the box plots of the health during the entanglement window.
#' This uses two data frames: (tangRepro and tangNonRepro) which are stored 
#' in the package structure. The function also used the unimpacted reference 
#' cases that are returned from the `returnUnimpactedHealth.R` helper
#' function. This function returns two data frames: \code{dfLongRepro}, and
#' \code{dfLongNonRepro} 
#' 
#' @param \code{tangRepro} data frame with information on the temporal
#'     extend of windows for entnanglement events of 
#'     reproductively active animals
#' @param \code{tangNonRepro} data frame with information on the temporal
#'     extend of windows for entnanglement events of 
#'     \emph{non}-reproductively active animals
#' @return \code{dfLongRepro} a melted data frame of the health of 
#'     reproductively active animals during entanglement windows
#' @return \code{dfLongNonRepro} a melted data frame of the health of 
#'     \emph{non}-reproductively active animals during entanglement windows
#' @export
#' @examples 
#' \dontrun{
#' prepBoxplotHealthData(tangRepro, tangNonRepro, anomFlag = TRUE)
#' }
prepBoxplotHealthData <- function(tangRepro, tangNonRepro, anomFlag = FALSE){
  
  tmp <- returnUnimpactedHealth(anomFlag = anomFlag)
  nonrepvec <- tmp$nonrep
  repvec <- tmp$rep
  
  nonrepvec1980 <- tmp$nonrep1980
  repvec1980 <- tmp$rep1980
  
  nonrepvec1990 <- tmp$nonrep1990
  repvec1990 <- tmp$rep1990
  
  nonrepvec2000 <- tmp$rep2000
  repvec2000 <- tmp$nonrep2000
  
  useAnom <- anomFlag

  for(z in 1:2){
  ifelse(z == 1, repro <- TRUE, repro <- FALSE)



  if (repro) {
   tSub <- tangRepro 
  } else {
   tSub <- tangNonRepro
  }  

  if (useAnom){
    healthmean <- anom
  } 


  dfSum <- data.frame(egno = rep(NA, times = nrow(tSub)), 
                    eventNo = rep(NA, times = nrow(tSub)),
                    nMonths = rep(NA, times = nrow(tSub)),
                    hAnom = rep(NA, times = nrow(tSub)),
                    gearInj = rep(NA, times = nrow(tSub)),
                    startDate = rep(NA, times = nrow(tSub)),
                    endDate = rep(NA, times = nrow(tSub)))
  idFac <- factor(unique(tSub$EGNo))

  for(i in 1:length(tSub$EGNo)){
    # i <- 32 #, EGNo == 1130 is a good test animal; i <- 390 is another (EGNo = 1102) (Both for Non-Repro)
    # i = 4 # EGNo == 1014 for Repro
    ind <- tSub$EGNo[i]
    if(ind == 1045) next()
    eventNo <- tSub$EventNo[i]
    if(!ind %in% ID){next()}
    htest <- healthmean[which(ID == ind),]
    ti <- tSub[tSub$EGNo == ind,]
  
    # Assemble the vector of integers for the duration of different gear injury combinations
    # asking for the start and end of the health window during which I'll calculate health
    # also for the date of first severe entanglement & the the 12 month recovery date
    evnum <- nrow(ti)
    s <- match(t(tSub[i, 'swindmonyr']), myName)  
    sDate <- tSub[i, 'StartDateWindow', drop = TRUE]
    e <- match(t(tSub[i, 'ewindmonyr']), myName)
    eDate <- tSub[i, 'EndDateWindow', drop = TRUE]
    sev <- match(t(tSub[i, 'fsevmonyr']), myName)
    sev <- sev[is.finite(sev)]
    gstat <- tSub[i, 'gearInj']
    hind <- htest[s:e]
  
    dfSum[i, 'egno'] <- ind
    dfSum[i, 'eventNo'] <- eventNo
    dfSum[i, 'hAnom'] <- median(hind, na.rm = TRUE)
    dfSum[i, 'gearInj'] <- gstat
    dfSum[i, 'nMonths'] <- length(s:e)
    dfSum[i, 'startDate'] <- lubridate::year(sDate)
    dfSum[i, 'endDate'] <- lubridate::year(eDate)
  }

  dfSum$gearnogear <- 0
  sev1idx <- which(dfSum$gearInj == 1)
  mod1idx <- which(dfSum$gearInj == 2)
  min1idx <- which(dfSum$gearInj == 4)
  dfSum[sev1idx, 'gearnogear'] <- 1
  dfSum[mod1idx, 'gearnogear'] <- 1
  dfSum[min1idx, 'gearnogear'] <- 1
  dfSum$variable <- 'impacted'

  # add in the unimpacted animals - using data coming from the splitpopHealth.R function
  # ONly adding them in once to avoid the double data, i.e. adding the same unimpacted data for both
  # the rep and the non-rep, when we really only want them once.
  if (repro) {
    uvec <- repvec
    dfuvec <- data.frame(egno = 9999,
                       eventNo = 0,
                       nMonths = 0,
                       hAnom = uvec,
                       gearInj = 0, 
                       startDate = '01-0000',
                       endDate = '01-0000',
                       gearnogear = 0,
                       variable = 'unimpacted')
    
    # 1980
    uvec1980 <- repvec1980
    dfuvec1980 <- data.frame(egno = 9999,
                         eventNo = 0,
                         nMonths = 0,
                         hAnom = uvec1980,
                         gearInj = 0, 
                         startDate = '01-1980',
                         endDate = '12-1989',
                         gearnogear = 0,
                         variable = 'unimpacted')
    
    # 1990
    uvec1990 <- repvec1990
    dfuvec1990 <- data.frame(egno = 9999,
                             eventNo = 0,
                             nMonths = 0,
                             hAnom = uvec1990,
                             gearInj = 0, 
                             startDate = '01-1990',
                             endDate = '12-1999',
                             gearnogear = 0,
                             variable = 'unimpacted')
    
    # 2000
    uvec2000 <- repvec2000
    dfuvec2000 <- data.frame(egno = 9999,
                             eventNo = 0,
                             nMonths = 0,
                             hAnom = uvec2000,
                             gearInj = 0, 
                             startDate = '01-2000',
                             endDate = '12-2009',
                             gearnogear = 0,
                             variable = 'unimpacted')
    
    dfLong <- rbind(dfSum, dfuvec, dfuvec1980, dfuvec1990, dfuvec2000)
  } else {
    uvec <- nonrepvec
    dfurvec <- data.frame(egno = 9999,
                        eventNo = 0,
                        nMonths = 0,
                        hAnom = uvec,
                        gearInj = 0, 
                        startDate = '01-0000',
                        endDate = '01-0000',
                        gearnogear = 0,
                        variable = 'unimpacted')
    
    # 1980
    uvec1980 <- nonrepvec1980
    dfurvec1980 <- data.frame(egno = 9999,
                          eventNo = 0,
                          nMonths = 0,
                          hAnom = uvec1980,
                          gearInj = 0, 
                          startDate = '01-1980',
                          endDate = '12-1989',
                          gearnogear = 0,
                          variable = 'unimpacted')
    
    # 1990
    uvec1990 <- nonrepvec1990
    dfurvec1990 <- data.frame(egno = 9999,
                              eventNo = 0,
                              nMonths = 0,
                              hAnom = uvec1990,
                              gearInj = 0, 
                              startDate = '01-1990',
                              endDate = '12-1999',
                              gearnogear = 0,
                              variable = 'unimpacted')
    
    # 2000
    uvec2000 <- nonrepvec2000
    dfurvec2000 <- data.frame(egno = 9999,
                              eventNo = 0,
                              nMonths = 0,
                              hAnom = uvec2000,
                              gearInj = 0, 
                              startDate = '01-2000',
                              endDate = '12-2009',
                              gearnogear = 0,
                              variable = 'unimpacted')
    
    dfLong <- rbind(dfSum, dfurvec, dfurvec1980, dfurvec1990, dfurvec2000)
  }

  dfLong$group <- 1
  mod0idx <- which(dfLong$gearInj == 5)
  min0idx <- which(dfLong$gearInj == 6)
  unimpactedidx <- which(dfLong$variable == 'unimpacted')
  dfLong[c(mod1idx, mod0idx), 'group'] <- 2
  dfLong[c(min1idx, min0idx), 'group'] <- 3
  dfLong[unimpactedidx, 'group'] <- 4
  dfLong$group <- factor(dfLong$group, levels = c(4, 3, 2, 1) )

  # Put an identifier column
  if (repro) {
    dfLongRepro <- dfLong
    dfLongRepro$status <- 'RepFem'
    } else {
    dfLongNonRepro <- dfLong
    dfLongNonRepro$status <- 'NonRepFem'
  }  

  } # end loop over the 'repro' variable

  list(dfLongRepro = dfLongRepro, dfLongNonRepro = dfLongNonRepro)

}