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

#' ID vector of EGNo's for all individual right whales in the data base
#' 
#' This vector is generated and curated by the New England Aquarium. 
#' Each value in this vector is a unique and unchanging identifier 
#' of individual right whales
#' 
#' @format A vector of length 696
'ID'

#' Length of the modelling
#' 
#' This is simply calculated with \code{length(myName)} and shows the 
#' temporal extent of the modelling.
#' 
#' @format A vector of length 1
'nt'

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