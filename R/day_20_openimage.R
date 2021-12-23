library(tidyverse)
library(OpenImageR)

#get the code and starting image
dd <- data.frame(x = readLines("data/day_20_real"))
dd <- mutate(dd, x = str_replace_all(x, "\\.", "0") %>% str_replace_all("#", "1"))
code <- as.integer(str_split_fixed(paste(filter(dd, cumsum(x == "") == 0) %>% pull(x), collapse = ""), "", n = 512))

N <- str_length(last(dd$x))

im <- filter(dd, cumsum(x == "") == 1) %>% 
  filter(str_length(x) == N) %>%  
  pull(x) %>% 
  str_split("") %>% 
  unlist() %>% 
  as.integer() %>% 
  matrix(byrow = T, ncol = N)

#pads the matrix to have 5 additional rows of zeros
im <- padding(im, N + 10, N + 10, fill = 0)$data

kernel <- matrix(2^(0:8), nrow = 3, byrow = T)

for(j in 1:50) {
  #convolutin with the kernel, keeps the partial inner products
  octals <-convolution(im, kernel, mode = "full")
  N <- nrow(octals)

  #these values aren't correct, they should be 511 or 0 but aren't due to partial inner products
  octals <- octals[,-c(1,2,N -1,N)]
  octals <- octals[-c(1,2,N-1,N),]
  im <- matrix(code[octals  + 1], nrow = nrow(octals))
  im <- padding(im, N + 2, N + 2, fill = im[1,1])$data
}

sum(im) #second star!