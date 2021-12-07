library(tidyverse)
dd <- data.frame(x = readLines(here::here("data/day_04")))
nums <- dd$x[1] %>% str_split(",", simplify = T) %>% as.integer()

num_boards <- (nrow(dd) - 1)/6
boards <- filter(dd, !str_detect(x, ","), str_detect(x, "[0-9]")) %>%  
  mutate(x = str_remove(x, "^ ")) %>% 
  separate(x, into = letters[1:5]) %>%
  mutate_all(as.integer) %>%
  as.matrix(byrow = T) %>% 
  t() %>% 
  array(dim = c(5, 5, num_boards)) #get data into array

i <- 1
while(all(card_results != 5)) {
  boards[boards == nums[i]] <- "X"
  card_results <- apply(pmax(apply(boards, c(1, 3), function(x) sum(x == "X")), apply(boards, c(2,3), function(x) sum(x == "X"))), 2, max)
  i <- i + 1
}

sum(as.integer(boards[,,which(card_results == 5)]), na.rm = T) * nums[i - 1] #first star!

bb <- integer(0)
while(any(card_results != 5)) {
  bb[i] <- first(which(card_results != 5)) #use first to suppress warnings
  boards[boards == nums[i]] <- "X"
  card_results <- apply(pmax(apply(boards, c(1, 3), function(x) sum(x == "X")), apply(boards, c(2,3), function(x) sum(x == "X"))), 2, max)
  i <- i + 1
}

sum(as.integer(boards[,,last(bb)]), na.rm = T) * nums[i - 1] #second star!










