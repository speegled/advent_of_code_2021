library(tidyverse)
library(matrixcalc)

#get the code and start image
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

#buffer start image with zeros
for(i in 1:3) {
  im <- cbind(cbind(0, im), 0)
  im <- rbind(rbind(0, im), 0)
}

for(j in 1:50) {
  N <- nrow(im)
  #apply convolution to each row, circular avoids NA and works since all buffers are the same
  conv <- t(apply(im, 1, function(x) {stats::filter(x, filter = c(1,2,4), sides = 2, circular = T)}))
  
  #paste the convolutions together to get 3 digit base 8 number for each entry in matrix
  octals <- paste0(shift.down(conv, fill = conv[1,1]), conv, shift.up(conv, fill = conv[1,1]))
  
  #convert to integer, add 1 and get new pixel from code; organize in matrix
  im <- matrix(code[sapply(octals, function(x) strtoi(x, base = 8)) + 1], nrow = N)
  
  #buffer new image with correct values
  im <- cbind(cbind(im[1,1], im), im[1,1])
  im <- rbind(rbind(im[1,1], im), im[1,1])
}

sum(im) #second star!