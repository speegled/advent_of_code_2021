library(tidyverse)
dd <- data.frame(x = readLines(here::here("data/day_05")))

#get data into data frame with starting and ending x,y values + type of line
ddlines <- dd %>% 
  separate(x, into = c("start_x", "start_y", "end_x", "end_y")) %>% 
  mutate(line = 1:n()) %>% 
  group_by(line) %>% 
  mutate(type = (any(c(start_x == end_x, start_y == end_y))))

#only lines of vertical or horizontal type created using expand.grid, then count
ddlines %>% 
  filter(type) %>% 
  summarize(expand.grid(seq(start_x, end_x), seq(start_y, end_y))) %>% 
  ungroup() %>% 
  count(Var1, Var2) %>% 
  count(n) %>% 
  filter(n > 1) %>% 
  pull(nn) %>% 
  sum #first star!

#struggled to use ifelse to create lines of both types inside of summarize, switched to map_df
ddfull <- purrr::map_df(ddlines$line, function(x) {
  if(ddlines$type[x]) {
    expand.grid(seq(ddlines$start_x[x], ddlines$end_x[x]), seq(ddlines$start_y[x], ddlines$end_y[x]))
  } else{
    data.frame(Var1 = seq(ddlines$start_x[x], ddlines$end_x[x]),
               Var2 = seq(ddlines$start_y[x], ddlines$end_y[x]))
  }
})

#same count as before
ddfull %>% 
  count(Var1, Var2) %>% 
  count(n) %>% 
  filter(n > 1) %>% 
  pull(nn) %>% 
  sum() #second star!
