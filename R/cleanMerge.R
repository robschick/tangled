# Clean up the entanglement time data from Amy and merge it to tangle
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