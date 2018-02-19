tangleOut$sidx <- match(tangleOut$swindmonyr, myName)
tangleOut$eidx <- match(tangleOut$ewindmonyr, myName) 

tangleOut$range <- NA
for(i in seq_along(tangleOut$EGNo)){
  tangleOut$range[i] <- list(as.integer(tangleOut[i, 'sidx']):as.integer(tangleOut[i, 'eidx']))
}

cand <- vector('list', max(seq_along(tangSub$EGNo)))
for(i in seq_along(tangSub$EGNo)){
  entRng <- as.integer(tangSub[i, 'sidx']):as.integer(tangSub[i, 'eidx']) # range of entangled animal's survival window
  
  for (j in seq_along(tangleOut)){
    candSub <- NA
    range1 <- unlist(tangleOut[j,'range'])
    
    if !any(range1 %in% entRng) {
      candSub <- c(candSub, tangleOut$EGNo[i])
    }
    
  }
  
  cand[[i]] <- candSub
  
}

