# https://www.interviewquery.com/questions/valid-anagram
library(plyr)
library(stringr)

is_anagram <- function(string_1,string_2){
  status = TRUE
  # vector of strings : not on a single vector
  # matrix -> vector co-ercion
  if(string_1 == string_2){
    return(FALSE)
  }
  sortedOne <- str_sort(as.vector(str_split_fixed(string_1, pattern = "", n = nchar(string_1))))
  sortedTwo <- str_sort(as.vector(str_split_fixed(string_2, pattern="", n=nchar(string_2))))
  if(length(sortedOne) != length(sortedTwo)){
    status = FALSE
  } else {
    for ( i in 1:length(sortedOne)){
      charOne = sortedOne[i]
      charTwo = sortedTwo[i]
      if(charOne != charTwo){
        status = FALSE
        break
      }
    }
  }
	return(status)
}
