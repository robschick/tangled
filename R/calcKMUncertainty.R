#' Data frame of 95% uncertainty quantiles for survivorship.
#'
#' \code{calcKMUncertainty} ingests the list of survivorship information
#' and returns the 2.5, 0.5 and 97.5 quantiles. The goal is to 
#' create the data needed to plot the ribbon around the median estimate
#' of survivorship for entangled animals. 
#' 
#' The processing idea is that we ingest a list, convert it to a big 
#' data frame. And then use dplyr to group by the different sex and 
#' severity combinations, and for each time interval, calculate the 
#' summary statistics
#' 
#' @param kmlines A list (size \code{nboot}) containing information about survivorship.
#' Each component of the list contains a data frame, which has these 10 columns
#' \describe{
#'    \item{interval}{The time slice for which we are recording survivorship. In this
#'    case it is a one year interval.}
#'    \item{atRiskStart}{The number of animals at risk at the start of the interval.}
#'    \item{censored}{The number of animals censored on the interval.}
#'    \item{diedOnInterval}{The number of animals that died on the interval.}
#'    \item{atRiskEnd}{The number of animals at risk at the end of the interval.}
#'    \item{propSurv}{The proportion of animals that survived the interval. 
#'    Calculated as \code{atRiskEnd / atRiskStart}}
#'    \item{gender}{The sex of the animal: 'M' == Male; 'F' == Female.}
#'    \item{psurv}{Survivorship - the cumulative product of proportion of animals that survive.}
#'    \item{sev}{The label of the entanglement severity. Values include in increasing
#'    order of badness: minor, moderate, severe.}
#'    \item{group}{Simply a vector denoting what iteration each slice corresponds 
#'    to.}
#' }
#' @return The output will be a data frame of survival information based on the
#' imputed death times. This will include 5 columns:
#' \describe{
#'     \item{gender}{Sex of the animal.}
#'     \item{sev}{The label of the entanglement severity. Values include in increasing
#'    order of badness: minor, moderate, severe.}
#'     \item{interval}{The time slice for which we are recording survivorship. In this
#'    case it is a one year interval.}
#'     \item{low}{2.5% Quantile of survivorship.}
#'     \item{med}{50% Quantile of survivorship.}
#'     \item{high}{97.5% Quantile of survivorship.}
#' }
#' @example 
#' calcKMUncertainty(kmlines)
#' 
calcKMUncertainty <- function(kmlines) {
  
  kmdf  <- as.data.frame(data.table::rbindlist(kmlines))
  kmout <- kmdf %>% 
    dplyr::group_by(gender, sev, interval) %>% 
    summarise(low = quantile(psurv, probs = 0.025),
              med = quantile(psurv, probs = 0.5),
              high = quantile(psurv, probs = 0.975))
  
  kmout 
}

