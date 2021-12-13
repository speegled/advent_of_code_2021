dd <- data.frame(x = readLines("data/day_13_prac"))
library(tidyverse)
dd <- dd %>% mutate(type = cumsum(x == ""))
ee <- dd %>% 
  filter(type == 1) %>% 
  select(-type)
dd <- dd %>% 
  filter(type == 0, x != "") %>% 
  select(-type)
dd <- dd %>% 
  separate(x, into = c("col", "row"), sep = ",", convert = T) %>% 
  mutate(across(.fns = function(x) x + 1))
mat <- matrix(0, nrow = max(dd$row), ncol = max(dd$col))

for(i in 1:nrow(dd)) {
  mat[dd$row[i], dd$col[i]] <- 1
}

fold_mat <- function(mat, val, coord) {
  if(coord == "x") {
    mat <- t(mat)
  }
  for(i in 1:val) {
    mat[i,] <- mat[i,] | mat[nrow(mat) - i + 1,]
  }
  mat <- mat[1:val,]
  if(coord == "x") {
    mat <- t(mat)
  }
  mat
}

ee <- ee %>% 
  separate(x, into = c("direction", "value"), sep = "=", convert = T) %>% 
  drop_na() %>% 
  mutate(direction = str_extract(direction, "[xy]$"),
         value = value + 1)

purrr::reduce2(.x = ee$value,.y = ee$direction, .f = fold_mat, .init = mat)
