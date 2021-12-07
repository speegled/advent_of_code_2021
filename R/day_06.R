library(tidyverse)
dd <- data.frame(x = c(0:8, readLines(here::here("data/day_06"))))
fish <- dd %>% 
  separate_rows(x) %>% 
  count(x) %>% 
  mutate(n = n - 1)

num_days <- 256 #change to 80 for first star!

for(i in 1:num_days) { 
  fish_0 <- fish$n[1]
  fish$n[1:8] <- fish$n[2:9]
  fish$n[7] <- fish$n[7] + fish_0
  fish$n[9] <- fish_0
}  

print(sum(fish$n), digits = 20) #second star!
