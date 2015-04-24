# to test and make sure the assembly is correct for entanglement
# case 1: 3-6 months between LDWG and Line Gone & 3-6 months between StartDate & EndDate
id1 <- which(tangleOut$gear == 1 & tangleOut$postDec6moTF == TRUE & tangleOut$predDectWindow3TF == TRUE)
id1 <- 56
tangleOut[id1,]


# case 2: 3-6 months between LDWG and Line Gone & > 6 months between StartDate & EndDate
id1 <- which(tangleOut$gear == 1 & tangleOut$postDec6moTF == TRUE & tangleOut$predDectWindow3TF == FALSE)
id1 <- 56
tangleOut[id1,]

# can use the table function to make sure that the numbers in gearInj make sense
table(tangleOut$gearInj)