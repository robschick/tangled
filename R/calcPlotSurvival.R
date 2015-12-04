rm(list = ls())
load("~/Rdev/tangled/data/eg_2015_newData_JUVTRUE__50000_wkspc.rdata")
library(RColorBrewer)
ngg <- g-1

#calculate one survival vector for one animal
i <- 304

meanSurv <- rep(NA, 660)

for(i in 1:length(meanSurv)) {
  svec <- 1 - cumsum(deathyr[i,]/ngg)[-(nt+1)]
  t1   <- min(which(apply(rsum[i,,],1,sum)/ngg > 0))
  svec[1:(t1-1)] <- 0
  ti <- which(svec > 0)
  meanSurv[i] <- prod(svec[ti])
}
