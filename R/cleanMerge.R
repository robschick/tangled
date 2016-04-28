#' Clean up the entanglement time data for geary-carrying animals and merge it to \code{tangle}
#' 
#' Goal of this is to pull in the entanglement time for gear carrying animals, 
#' and merge it with the entanglement event data for animals that are non-gear
#' carrying. The data come from Amy Knowlton, New England Aquarium.
#' 
#' @param etime - the timing of entanglement information for geary carrying 
#'   animals.
#' @param tangle - the entanglement event data
#' @examples 
#' cleanMerge(etime, tangle)
cleanMerge <- function(etime, tangle){
  
  tvec <- tangle$EntanglementComment
  tdx  <- str_match(tvec, 'GEAR')
  tdat <- tangle[which(tdx == "GEAR"),]
  tndat <- tangle[which(is.na(tdx)),]
  tangleOut <- merge(tdat, etime,  by = c('EGNo', 'EventNo'))
  tangleOut$EndDate <- tangleOut$EndDate + tangleOut$min.time.carrying.gear * (24 * 60 * 60)
  colx <- which(colnames(tangleOut) %in% colnames(etime)[!colnames(etime) %in% colnames(tangle)])
  tangleOut <- tangleOut[,-colx]
  tangleOut <- rbind(tndat, tangleOut)
  return(tangleOut)

  }