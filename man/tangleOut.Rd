% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{tangleOut}
\alias{tangleOut}
\title{Information about entanglement events - filtered and modified for use in analysis}
\format{
A data frame of 910 rows and 22 columns:
\describe{
 \item{EGNo}{4 number code that identifies individual right whales}
 \item{EventNo}{Integer labelling the consecutive number of each entanglement event}
 \item{StartDate}{A time variable describing the estimated first date of the 
 entanglement event} 
 \item{EndDate}{A time variable describing the end of the entanglement event}  
\item{Severity}{One of three categories of worsening injury: minor, moderate, severe. These are standard definitions created by NEA}
\item{gear}{Integer variable describing whether or not the animal is carrying gear as part of the entanglement: 0 for no gear; 1 for gear}
\item{StartDateWindow}{Date of the altered start of the window using rules outlined below}
\item{EndDateWindow}{Date of the end of the window - same as \code{EndDate}}
\item{LastDatewGear}{Date of the last photographic observation of the whale where it was seen carrying gear}
\item{LineGone}{Date of the first time the animal was confirmed to be free of gear}
\item{firstSevere}{Date of the first (if any) severe entanglement event experienced by the individual whale. This is used as a flag to make sure we do not include any subsequent events in the health calculations, i.e. once an animal has had a severe event, they are forever impaired.}
\item{recov12months}{Date of 12 months past the entanglement event. Note that this means different things for different animals. For animals that are known to carry gear, this is 12 months past either \code{LastDatewGear} or \code{LineGone} (whichever is finite); for non-gear carrying whales this is 12 months beyond \code{EndDateWindow}}
\item{firstCalf}{Date format denoting January of the first year during which the whale was observed with a calf. This is used in establishing the before/after calf status of the animal for documenting health}
\item{firstCalfidx}{Integer corresponding to the location in \code{myName} of the date in \code{firstCalf}}
\item{smonyr}{Character version of \code{StartDate} designed to intersect with \code{myName} to peel off the health values in the matrix of estimated heatlh, e.g. '2-1986'}
\item{emonyr}{Character version of \code{EndDate} designed to intersect with \code{myName} to peel off the health values in the matrix of estimated heatlh, e.g. '2-1986'}
\item{swindmonyr}{Character version of \code{StartDateWindow} designed to intersect with \code{myName} to peel off the health values in the matrix of estimated heatlh, e.g. '2-1986'}
\item{ewindmonyr}{Character version of \code{EndDateWindow} designed to intersect with \code{myName} to peel off the health values in the matrix of estimated heatlh, e.g. '2-1986'}
\item{fsevmonyr}{Character version of \code{firstSevere} designed to intersect with \code{myName} to peel off the health values in the matrix of estimated heatlh, e.g. '2-1986'}
\item{rec12monyr}{Character version of \code{recov12months} designed to intersect with \code{myName} to peel off the health values in the matrix of estimated heatlh, e.g. '2-1986'}
\item{afterCalf1}{Logical of whether or not the entanglement event is after the first calving event (\code{firstCalf})}
}
}
\usage{
tangleOut
}
\description{
Whereas \code{tangleAll} contains the raw information on the entanglement events, \code{tangleOut} is designed to be used in the analysis of the effects of entanglement on health. This means that the 'windows' have been created according to different rules, the gear status information has been added, and the indices have been added for the windows to facilitate data extraction from the health data as part of the model output.
}
\details{
Starting with the \code{StartDate} and \code{EndDate} from the raw data we have the entire window/duration when the animal may have been entangled. However, in reality, these windows can be really long - most often just associated with sighting gaps in the animal's history. In certain cases, then, it makes sense to pare down these windows into something a bit shorter. We have generated a set of rules for both the starting dates as well as the ending dates; the rules are as follows:

For any window that is greater than or equal to 3 months, the window gets shortened to 3 months. We do this by evaluating the duration between \code{StartDateWindow} and \code{EndDateWindow}. If that value exceeds 3 months, then a new start date is calculated by subtracting 3 months from the end date.
 
For any window >= 1 & <2 months, the window gets shortened to 1 month. Processing is as above.

For any window < 1 month, the window gets lengthened to 1 month. Processing is as above.

For any window >= 2 & <3 months, the window gets set to 2 month. Processing is as above.
}
\keyword{datasets}
