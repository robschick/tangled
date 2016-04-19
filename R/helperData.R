#' Health anomaly between individual health and population median health
#' 
#' This is a matrix of differences between individual health 
#' and population level health. I calculate this anomaly by first 
#' calculating the median health of the target groups (Adult males,
#' and young and old juveniles), and then subtracting that value
#' from the median health of each individual.
#' 
#' @format A matrix of dimensions 696 by 564; each row contains the estimates
#' of health anomaly for an individual. Each column is a month.
'anom'

#' Estimated health of individual animals
#' 
#' This is a matrix that of individual health estimated by the model
#' in Schick et al. 2013 PLoS-ONE.
#' 
#' @format A matrix of dimensions 696 by 564; each row contains the estimates
#' of health for an individual. Each column is a month.
'healthmean'

#' Estimated death months of individual animals
#' 
#' This is a matrix that of individual death months estimated by the model
#' in Schick et al. 2013 PLoS-ONE.
#' 
#' @format A matrix of dimensions 696 by 564; each row contains the estimates
#' of possible or known death times for an individual. Each column is a month.
'deathyr'

#' Gender of each individual right whale
#' 
#' @format A vector of length 696 comprised of 'M' for males and 'F' for females
'gender'

#' Data frame of known deaths of individual right whales
#' 
#' This comes from the New England Aquarium and is used to set the death time
#' and places for whales that are known to have died.
#' 
#' @format A 42 by 8 data frame containing this columns:
#' \describe{
#'    \item{SightingEGNo}{Integer of the unique identifier of the individual whale}
#'    \item{SightingYear}{integer Year of death}
#'    \item{SightingMonth}{Integer Month of death}
#'    \item{SightingDay}{Integer Day of death}
#'    \item{Latitude}{Double Latitude position of observed death}
#'    \item{Longitude}{Double Longitude position of observed death}
#'    \item{AreaCode}{Character noting the macro-scale geographic location of the death}
#'    \item{RegionCode}{Character noting the micro-scale geographic location of the death}
#' }
'deadTable'

#' ID vector of EGNo's for all individual right whales in the data base
#' 
#' This vector is generated and curated by the New England Aquarium. 
#' Each value in this vector is a unique and unchanging identifier 
#' of individual right whales
#' 
#' @format A vector of length 696
'ID'

#' Scalar noting when the sightings data are complete through
#' 
#' Right now this is set based on advice from Philip Hamilton (NEAq)
#' and sets the cutoff date for when the modelling 'ends.' The modelling
#' doesn't really end here, but this is the threshold we use for censoring
#' 
#' @format A vector of length 1
'dcut'

#' Length of the modelling
#' 
#' This is simply calculated with \code{length(myName)} and shows the 
#' temporal extent of the modelling.
#' 
#' @format A vector of length 1
'nt'

#' Number of Gibbs steps used in the input rda file
#' 
#' This simply shows how long we ran the Gibbs model
#' for, and since we often store moments, this \code{ng}
#' object is useful to get values that make sense for 
#' subsequent analysis.
#' 
#' @format A vector of length 1
'ng'

#' Month-year vector used to mark time in the model
#' 
#' This starts at January 1970, and goes through September, 2016,
#' or roughly the temporal extent of modelling. Values are like '1-1970'
#' 
#' @format A vector of length 696
'myName'

#' Vector denoting the first month (in integer form) of animal sighting
#' 
#' Unlike \code{myName} this takes an integer form, e.g. 110, where 
#' \code{myName[firstSight[6]] == "2-1979"}. This is used throughout
#' the code to help extract values from the health and other matrices
#' @format A vector of length 696
'firstSight'

#' Vector denoting the last month (in integer form) of animal sighting
#' 
#' Unlike \code{myName} this takes an integer form, e.g. 110, where 
#' \code{myName[lastSight[6]] == "10-1986"}. This is used throughout
#' the code to help extract values from the health and other matrices
#' @format A vector of length 696
'lastSight'