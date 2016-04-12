#' Return \code{events} data frame of last entanglement event
#' 
#' Goal of this function is to take data that are stored in the package
#' including the entanglement information, and the estimated median death 
#' times, and prepare a data frame that only has the \emph{last}
#' entanglement event for each animal in the database. This is because 
#' we want to look at survival following these events, and compare the 
#' impact of different types of entanglement on the survivorship.
#' 
#' @return \code{events} a 464 by 26 data frame where each row
#' has the information on the entanglement event. I'm starting
#' with \code{tangleOut} and to that am adding 5 columns:
#' \describe{
#' \item{dtime}{Integer denoting the maximum value in \code{deathyr} which is
#'     a matrix of estimated or fixed deaths for all animals in the population.
#'     This is a key variable because it allows us to calculate survivorship}
#' \item{knownD}{logical: If there is no uncertainty in death \code{and} the 
#'     death is less than \code{dcut}, then this animal is known to be dead. 
#'     n.b. this can be tested against the input data from NEAq}
#' \item{presD}{logical: If there is uncertainty in death \code{and} the 
#'     death is less than \code{dcut}, then this animal is presumed dead}
#' \item{presA}{logical: If there is uncertainty in death \code{and} the 
#'     death is greater than \code{dcut}, then this animal is presumed alive}
#' \item{ewindmonyrID}{Integer noting the end of the entanglement window}
#' }
#'
#' @example makeEvents() 
makeEvents <- function(){
  
  tSub <- tangleOut
  
  # for some reason, two animals contain duplicated info: 1249 & 1980
  # so I'm going to remove one of them
  idx <- which(tSub$EGNo == 1249 & tSub$EventNo == 2)
  tSub <- tSub[-idx[2],]
  idx <- which(tSub$EGNo == 1980 & tSub$EventNo == 2)
  tSub <- tSub[-idx[2],]
  
  # find the last Entanglement event
  events <- tSub %>% dplyr::group_by(EGNo) %>% dplyr::top_n(n=1, EventNo) %>% dplyr::arrange(EGNo)
  idx <- events$EGNo %in% ID
  events <- events[idx, ]
  
  
  events$dtime <- NA
  events$knownD <- FALSE
  events$presD <- FALSE
  events$presA <- FALSE

  for (i in 1:length(unique(events$EGNo))) {
  
    id <- which(ID == unique(events$EGNo)[i])
    dsub <- deathyr[id, ]
    events$dtime[i] <- which.max(dsub)
  
    if (unique(events$EGNo)[i] %in% deadTable$SightingEGNo) {
      events$knownD[i] <- TRUE
    }
  
    if (any(dsub != ng) & events$dtime[i] < dcut) {
      events$presD[i] <- TRUE
    }
  
    if (any(dsub == ng) & events$dtime[i] > dcut) {
      events$presA[i] <- TRUE
    }
  
  }
  events <- mutate(events, ewindmonyrID = match(ewindmonyr, myName))
  events
}