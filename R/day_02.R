library(tidyverse)
dd <- data.frame(x = readLines(here::here("data/day_02")))
dd <- dd %>% 
  separate(x, into = c("direction", "magnitude"), convert = T)
dd <- dd %>% 
  mutate(movement = ifelse(str_detect(direction, "up|back"), -magnitude, magnitude))
depth <- filter(dd, str_detect(direction, "do|up")) %>% pull(movement) %>% sum()
length <- filter(dd, str_detect(direction, "forw|back")) %>% pull(movement) %>% sum()
depth * length #first star!

select(dd, direction, magnitude) %>% 
  mutate(aim = cumsum(case_when(
           direction == "down" ~ magnitude,
           direction == "up" ~ -magnitude,
           TRUE ~ 0L #didn't know case_when was strict about types
         )),
         depth = cumsum( case_when (
           direction == "forward" ~ magnitude * aim,
           TRUE ~ 0L
         )),
         length = cumsum(ifelse(direction == "forward", magnitude, 0))) %>% 
  summarize(ans = last(depth) * last(length)) #second star!
