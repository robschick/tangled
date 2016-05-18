#' Create valid and usable variables describing the start/stop dates of entanglement windows
#' 
#' The primary goal of this is to clean up the entanglement data a bit. We do this by
#' adding in information in the comments about each event, and coding that into a new
#' variable. We also extract the sex from the comments, and the age of the animal.
#' 
#' @return \code{tangled} a data frame containing the cleaned entanglement data
#' @export
makeTangle <- function(){
	library(stringr)
	library(lubridate)

	# I'm setting up the intervals during which the event must have occurred. Here's how Amy Knowlton defines them (Email dated 7/11/2011):
	# Ah yes, these terms start and end date are confusing and don’t really mean what they suggest.
	# Start date is actually the date the animal was seen and known not to have entanglement scars.
	# End date is the first date the animal was detected with new entanglement scars (or carrying gear).
	# So by subtracting start date from end date, this gives you the timeframe within which the entanglement interaction must have occurred.

	tangled$StartDate <- ymd(tangled$StartDate)
	tangled$EndDate <- ymd(tangled$EndDate)

	idx <- which(tangled$StartDate <= ymd('1900-01-02'))
	tangled[idx, c('StartDate')] <- NA # I'm doing this because these are fictitious, i.e. there is no real known start date. So I want to zero out anything that was based on those start dates.


	comments <- str_split(tangled$EntanglementComment, ";")

	tangled$AgeSex <- rep(NA, nrow(tangled))
	tangled$TimeFrame <- rep(NA, nrow(tangled))
	tangled$Severity <- rep(NA, nrow(tangled))
	
	# Most of these are from entanglement scars, but there are cases that have gear. The next bit is to find 	them and mark those with a 1 (0 otherwise)
	gear.idx <- which(!is.na(str_match(tangled$EntanglementComment, "GEAR")))
	tangled$gear <- rep(0, nrow(tangled))
	tangled[gear.idx, 'gear'] <- 1

	minor.idx <- which(!is.na(str_match(tangled$EntanglementComment, "MINOR")))
	tangled[minor.idx, 'Severity'] <- 'minor'
	
	moderate.idx <- which(!is.na(str_match(tangled$EntanglementComment, "MODERATE")))
	tangled[moderate.idx, 'Severity'] <- 'moderate'
	
	severe.idx <- which(!is.na(str_match(tangled$EntanglementComment, "SEVERE")))
	tangled[severe.idx, 'Severity'] <- 'severe'
	
    dead.idx <- which(!is.na(str_match(tangled$EntanglementComment, "DEAD")))
	tangled$LiveStatus <- rep("ALIVE", nrow(tangled))
	tangled[dead.idx, 'LiveStatus'] <- 'DEAD'
	
			
	for(i in 1:nrow(tangled)){
		tangled$AgeSex[i] <- comments[[i]][1]
		tangled$TimeFrame[i] <- comments[[i]][2]
	}
	
	agesex <- str_split(tangled$AgeSex," female")
	males <- str_trim(str_match(tangled$AgeSex,' male'))
	females <- str_trim(str_match(tangled$AgeSex,' female'))

	fdx <- which(females == 'female', arr.ind = T)
	tangled$Gender <- rep(NA, nrow(tangled))
	tangled[fdx, 'Gender'] <- females[fdx]

	mdx <- which(males == 'male', arr.ind = T)
	tangled[mdx, 'Gender'] <- males[mdx]
	return(tangled)	
}