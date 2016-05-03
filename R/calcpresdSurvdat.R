#' Prepare the data needed to calculate survivorship for presumed dead animals
#' 
#' Goal of this is to ingest the \code{events} data frame and the output
#' from \code{calckdpaSurvdat()} to make one list that can be used
#' to calculate survivorship. The first element of this list will be
#' the presumed Alive and known Dead animals. All other elements will contain
#' the presumed dead animals. The goal of this structure was to be able
#' to 
#' 
#' @param \code{events} a data frame of the \emph{last} entanglement event
#'     for individual whales.
#' @param \code{kdpasurvldf} a data frame output from the 
#'     \code{calckdpaSurvdat()} function. These are the preparatory data for
#'     calculating survivorship
#' @return \code{survdf} a many by 10 stacked data frame containing the 
#'     information needed to calculate survivorship (Done in another function).
#'     \describe{
#'        \item{EGNo}{Numerical identifier of the individual whale}
#'        \item{deathMonth}{Integer noting the death month of the animal,
#'            i.e. the median month it was estimated to die in}
#'        \item{censored}{Logical indicating whether or not the animal was 
#'            censored at \code{dcut}}
#'        \item{censMonth}{Integer indicating the integer time for \code{dcut}}
#'        \item{survTime0}{Integer noting the length of time between the end 
#'            of the entanglement window and the \code{deathMonth}}
#'        \item{deathMonth0}{Integer noting the maximum of \code{survTime0}}
#'        \item{censMonth0}{Integer indicating number of months between
#'            the end of the entanglement and the censoring month 
#'            (\code{censMonth})}
#'        \item{Severity}{Character describing the severity of the entanglement
#'            injury. Ordinal values include: minor, moderate, and severe}
#'        \item{gearInj}{Integer denoting our numerical representation of the
#'            entanglement injury and whether or not the injury includes gear}
#'        \item{knownDeath}{Logical indicating whether or not the animal is
#'            known to have died}
#'     }
#' @export
#' @examples 
#' \dontrun{
#' calcpresdSurvdat(events, kdpasurvldf)
#' }
calcpresdSurvdat <- function(events, kdpasurvldf){
  
  esub <- subset(events, presD)
  survl <- vector(mode = 'list', nrow(esub))
  kd <- FALSE 
  
  for(i in 1:nrow(esub)){
    
    id <- esub[i, 'EGNo']
    dmonth <- as.numeric(esub[i, 'dtime'])
    emonth <- as.numeric(esub[i, 'ewindmonyrID'])
    censor <- ifelse(dmonth > dcut, TRUE, FALSE)
    cmonth <- dcut
    dmonth2 <- dmonth 
    svec <- seq(emonth, dmonth2)
    svec0 <- svec - min(svec)
    csvec <- seq(emonth, cmonth)
    csvec0 <- csvec - min(csvec)
    
    survl[[i]] <- data.frame(EGNo = id, deathMonth = dmonth, censored = censor, censMonth = cmonth, survTime0 = svec0, 
                             deathMonth0 = max(svec0), censMonth0 = max(csvec0), severity = esub[i, 'Severity'], 
                             sevNumClass = esub[i ,'gearInj'], knownDeath = kd)
  
  }
  
  survl[['kdpa']] <- kdpasurvldf# populate with one data frame of the known dead animals and the presumed Alive animals
  survdf <- as.data.frame(data.table::rbindlist(survl))
  
  survdf$censMonth[survdf$censMonth < dcut] <- NA
  survdf  
}
