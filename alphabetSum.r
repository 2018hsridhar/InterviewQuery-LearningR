# Sum(ordinalPosition(letter)) in standard english alpha
# ordinal positions are 1-indexed
# 
library(plyr)
library(purrr)

sum_alphabet <- function(words){
    alphaSums <- map(words,alphaSumFunc)    
    return(alphaSums)
}

# require(roperators)
# library(roperators)
#  package, this offers a incrementor function in the format of %+=%. In the X example above, this would be x %+=% 1

# character vector length 1 gaaah
alphaSumFunc <- function(word){
    # vector of single characters?
    myLetters <- strsplit(word,"")[[1]]
    alphaSum = Reduce("+",map(myLetters, deltaFunc))
    if(is.null(alphaSum)){
        alphaSum = 0
    }
    return(alphaSum)
}

deltaFunc <- function(letter){
    if(length(letter) == 0){
        return(0)
    }
    baseInt = utf8ToInt('a')
    delta = utf8ToInt(letter) - baseInt + 1
    return(delta)
}
