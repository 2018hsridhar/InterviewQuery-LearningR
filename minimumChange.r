# Change making function
# sort our cents

library(plyr)

minimum_change <- function(cents){
    # WTF combine() functions for vector init
    denoms = c(10,5,25,1)
    ii <- order(denoms)
    denoms = denoms[ii]
    denomIdx = length(denoms)
    centsNeeded <- 0
    while(denomIdx >= 1){
        curDenom = denoms[denomIdx]
        numFit = floor(cents / curDenom)
        if(numFit >= 1){
            centsNeeded <- centsNeeded + numFit
            cents <- cents - (curDenom * numFit)
        }
        denomIdx <- denomIdx - 1
    }
	return(centsNeeded)
}
