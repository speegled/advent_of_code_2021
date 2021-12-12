dd <- readLines(here::here("data/day_11"))
library(tidyverse)

#set up matrix bordered with NA
oct <- c(rep(NA, 10), unlist(str_split(dd, "")), rep(NA, 10)) %>% 
  as.integer() %>% 
  matrix(byrow = T, nrow = 12)
oct <- cbind(rep(NA, 12), oct, rep(NA, 12))
ncol <- ncol(oct) 
orig_oct <- oct #save for part 2

#not using row/col indexing
get_adjacencies <- function(x) {
  r <- c(x + 1, x - 1)
  c(r, r + ncol, r- ncol, x + ncol, x - ncol, x)
}

flashes <- 0
for(i in 1:100) {
  oct <- oct + 1
  exploded <- which(oct == 10)
  oct[which(oct == 10)] <- oct[which(oct == 10)] + 1
  flashes <- flashes + length(exploded)
  changed <- length(exploded) > 0
  while(changed) {
    changed <- FALSE
    new_exploded <- integer(0)
    for(vals in exploded) {
      oct[get_adjacencies(vals)] <- oct[get_adjacencies(vals)] + 1
      new_exploded <- unique(c(new_exploded, which(oct == 10)))
      oct[which(oct == 10)] <- oct[which(oct == 10)] + 1
      changed <- TRUE
    }
    exploded <- new_exploded
    flashes <- flashes + length(unique(exploded))
  }
  oct[which(oct >= 10)] <- 0
}
flashes #first star

i <- 1
repeat {
  oct <- oct + 1
  exploded <- which(oct == 10)
  oct[which(oct == 10)] <- oct[which(oct == 10)] + 1
  flashes <- length(exploded)
  changed <- length(exploded) > 0
  while(changed) {
    changed <- FALSE
    new_exploded <- integer(0)
    for(vals in exploded) {
      oct[get_adjacencies(vals)] <- oct[get_adjacencies(vals)] + 1
      new_exploded <- unique(c(new_exploded, which(oct == 10)))
      oct[which(oct == 10)] <- oct[which(oct == 10)] + 1
      changed <- TRUE
    }
    exploded <- new_exploded
    flashes <- flashes + length(unique(exploded))
  }
  oct[which(oct >= 10)] <- 0
  if(flashes == 100) {
    print(i + 100) #second star (add 100 from first loop :-)
    break
  }
  i <- i + 1
}
