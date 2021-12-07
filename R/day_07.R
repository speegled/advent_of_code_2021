library(tidyverse)
dd <- data.frame(x = readLines(here::here("data/day_07")))
dd <- dd %>% 
  separate_rows(x, convert = T)

crab_fuel <- function(j) {
  sum(abs(dd$x - j))
}
optimize(crab_fuel, lower = mean(dd$x) - sd(dd$x), upper = mean(dd$x) + sd(dd$x))$objective #first star!


crab_fuel_2 <- function(j) {
  n <- abs(dd$x - j)
  sum((n^2 + n)/2)
  #sum((abs((dd$x - j))^2 + abs(dd$x - j))/2)
}
real_answer <- optimize(crab_fuel_2, lower = mean(dd$x) - sd(dd$x), upper = mean(dd$x) + sd(dd$x))
min(ff(ceiling(real_answer$minimum)), ff(floor(real_answer$minimum))) #second star!
