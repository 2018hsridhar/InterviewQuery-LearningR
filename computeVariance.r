# '''
# https://www.interviewquery.com/questions/compute-variance
# Why focus on sample variance?
# 
# '''
# library(purrr)
# library(plyr)

get_variance <- function(test_list){
    sampleMean = mean(test_list, na.rm = TRUE)
    numberOfObservations = length(test_list)
    data = test_list
#apply rnorm() function to each value in vector
    # data %>% map(function(x) ((x - sampleMean)^2))
    # print(data)
    sum = 0
    for(val in data){
        sum <- sum + ((val - sampleMean)^2)
    }
    sampleVariance <- (sum / numberOfObservations)
    roundedSV = round(sampleVariance, digits = 2)
	return(roundedSV)
}
