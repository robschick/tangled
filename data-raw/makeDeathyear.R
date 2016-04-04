# All this does is read in a gibbs output, and save the deathyr object
load(file = 'data/eg_203_ng_50000_BIG_25000.rdata')
save(deathyr, file = 'data/deathyr.rda')
rm(list = ls())