#' Prepare the data needed to calculate survivorship for presumed alive and known dead animals
#' 
#' Goal of this is to ingest the \code{events} data frame and make a survivorship
#' data frame for the known dead, and presumed alive animals. These are easy,
#' because there is no uncertainty around their deaths. The output from this
#' will feed into \code{calcpresdSurvdat()} to make one data frame that has
#' survivorship for all three death categories
#' 
#' @usage \code{calckdpaSurvdat(events)}
#' @param \code{events} a data frame of the \emph{last} entanglement event
#'     for individual whales.
#' @return \code{kdpasurvldf} a many by 10 stacked data frame containing the 
#'     information needed to calculate survivorship (Done in another function).
#'     \describe{
#'        \item{EGNo}{Numerical identifier of the individual whale}
#'        \item{deathMonth}{Integer noting the \code{dtime} of the animal,
#'            i.e. the median month it was estimated to die in}
#'        \item{censored}{Logical indicating whether or not the animal was 
#'            censored at \code{dcut}}
#'        \item{censMonth}{Integer indicating the integer time for \code{dcut}}
#'        \item{survTime0}{Integer noting the length of time between the end 
#'            of the entanglement window and the \code{dtime}}
#'        \item{deathMonth0}{Integer noting the maximum of \code{survTime0}}
#'        \item{censMonth0}{Integer indicating number of months between
#'            the end of the entanglement and the censoring month}
#'        \item{Severity}{Character describing the severity of the entanglement
#'            injury. Ordinal values include: minor, moderate, and severe}
#'        \item{gearInj}{Integer denoting our numerical representation of the
#'            entanglement injury and whether or not the injury includes gear}
#'        \item{knownDeath}{Logical indicating whether or not the animal is
#'            known to have died}
#'     }
#' @export     
#' @examples  
#' calckdpaSurvdat(events)
calckdpaSurvdat <- function(events){
  
  kdpasub <- subset(events, !presD)
  kdpasurvl <- vector(mode = 'list', nrow(kdpasub))
  
  for(i in 1:nrow(kdpasub)){
    id <- kdpasub[i, 'EGNo']
    dmonth <- as.numeric(kdpasub[i, 'dtime'])
    emonth <- as.numeric(kdpasub[i, 'ewindmonyrID'])
    censor <- ifelse(dmonth > dcut, TRUE, FALSE)
    kd <- as.character(kdpasub[i, 'knownD'])
    cmonth <- dcut
    dmonth2 <- dmonth 
    svec <- seq(emonth, dmonth2)
    svec0 <- svec - min(svec)
    csvec <- seq(emonth, cmonth)
    csvec0 <- csvec - min(csvec)
    kdpasurvl[[i]] <- data.frame(EGNo = id, deathMonth = dmonth, censored = censor, censMonth = cmonth, survTime0 = svec0, 
                                 deathMonth0 = max(svec0), censMonth0 = max(csvec0), severity = kdpasub[i, 'Severity'], 
                                 sevNumClass = kdpasub[i ,'gearInj'], knownDeath = kd)
  }
  
  kdpasurvldf <- as.data.frame(data.table::rbindlist(kdpasurvl))
  kdpasurvldf
}     
