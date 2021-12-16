library(tidyverse)
library(collections)

dd <- readLines(here::here("data/day_10"))
push_char <- c("(", "[", "{","<")
pop_char <- c(")", "]", "}", ">")
bad_chars <- sapply(1:length(dd), function(j) {
  aa <- unlist(str_split(dd[j], pattern = ""))
  ee <- stack()
  if(aa[1] %in% pop_char) {
    return(aa[1])
  } else {
    ee$push(aa[1])
  }
  for(i in 2:length(aa)) {
    if(aa[i] %in% push_char) {
      ee$push(aa[i])
    } else {
      match_char <- ee$pop()
      if(which(match_char == push_char) != which(aa[i] == pop_char)) {
        return(aa[i])
      } 
    }
  }
  return(NA)
})

scores <- c(3, 57, 1197, 25137)
sapply(bad_chars[!is.na(bad_chars)], function(j) {
  scores[which(j == pop_char)]
}) %>% 
  sum() #first star!


scores <- c(1, 2, 3, 4)
j <- 1
sapply(1:length(dd), function(j) {
  aa <- unlist(str_split(dd[j], pattern = ""))
  ee <- stack()
  if(aa[1] %in% pop_char) { #check whether first char is already a mistake
    return(aa[1])
  } else {
    ee$push(aa[1])
  }
  for(i in 2:length(aa)) {
    if(aa[i] %in% push_char) {
      ee$push(aa[i])
    } else {
      match_char <- ee$pop()
      if(which(match_char == push_char) != which(aa[i] == pop_char)) {
        return(NA) #corrupt string, returning NA
      } 
    }
  }
  
  tot_score <- 0
  while(ee$size() > 0) {
    tot_score <-  5 * tot_score + scores[which(ee$pop() == push_char)]  
  }
  return(tot_score)
  
}) %>% 
  median(na.rm = T) #second_star!