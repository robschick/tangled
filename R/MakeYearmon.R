#' to make a year format from monYr for the right whale plotting
#' 
#' Goal of this script is to make proper date-formatted year month
#' data from input time data
#' @export
makeYearmon <- function(timedat){
	if(is.null(nrow(timedat))){	months <- timedat[1]} else {	months <- timedat[,1]}
	if(is.null(nrow(timedat))){	years <- timedat[2]} else {years <- timedat[, 2]}
	dates <- paste(months, '01', years, sep = '/')
	yearMonth <- as.Date(dates, '%m/%d/%Y')
	return(yearMonth)
}