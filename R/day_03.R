library(tidyverse)

dd <- readLines(here::here("data/day_03")) 
len <- length(dd)
len2 <- str_length(dd[1]) - 1

mat <- strsplit(dd, split = "") %>% 
  unlist() %>% 
  as.integer() %>% 
  matrix(ncol = len2 + 1, byrow = T) 

sums <- colSums(mat)

sum(round(sums/len) * 2^(len2:0)) * sum((1 - round(sums/len)) * 2^(len2:0)) #first star!

ox_mat <- mat

i <- 1
while(is.matrix(ox_mat)) {
  tabvals <- table(ox_mat[,i])
  ifelse(tabvals[2] >= tabvals[1], remove <- 1, remove <- 0)
  ox_mat <- ox_mat[ox_mat[,i] == remove,]
  i <- i + 1
}

co_mat <- mat

i <- 1
while(is.matrix(co_mat)) {
  tabvals <- table(co_mat[,i])
  ifelse(tabvals[1] <= tabvals[2], remove <- 0, remove <- 1)
  co_mat <- co_mat[co_mat[,i] == remove,]
  i <- i + 1
}

sum(ox_mat * 2^(len2:0)) * sum(co_mat * 2^(len2:0)) #second star!
