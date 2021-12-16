library(tidyverse)

dd_perm <- data.frame(readLines("data/day_12") %>% 
  str_split_fixed(pattern = "-", n = 2))

edges <- bind_rows(data.frame(dd_perm), 
                   data.frame(X1 = dd_perm$X2, 
                              X2 = dd_perm$X1))%>% 
  rename(from = X1, to = X2) %>% 
  filter(from != "end" & to != "start") #all edges in form of from -> to

var_names <- letters #hack to rename the variables after joining so I always know which one to join by
non_repeats <- unique(edges$from[str_length(edges$from) < 3 & !str_detect(edges$from, "[A-Z]")]) #these vertices cannot be repeated

dd_perm <- filter(edges, from == "start") #all paths start at start
names(dd_perm) <- c(var_names[1:(ncol(dd_perm) - 1)], "from") #the last variable is the last vertex in the current path

bad_row_star_1 <- function(vec) {
  any(table(vec[vec %in% non_repeats]) > 1) 
} #after joining, remove any row that contains repeated non-repeatable vertices

bad_row_star_2 <- function(vec) {
  counts <- table(vec[vec %in% non_repeats]) 
  any(counts > 2) || (sum(counts == 2) > 1)
} #remove row that repeats vertex 3 times or if two vertices are repeated twice

star <- 1
dd <- dd_perm
repeat{
  dd <- left_join(dd, edges) #all continued paths
  names(dd) <-  c(var_names[1:(ncol(dd) - 1)], "from") #hack to know which name is important
  if(star == 1) {
    remove <- which(apply(as.matrix(dd), 1, bad_row_star_1))
  } else {
    remove <- which(apply(as.matrix(dd), 1, bad_row_star_2))
  }

  if(length(remove) > 0) {
    dd <- dd[-remove,] #remove all of the bad rows
  }
  if(all(is.na(dd$from))) {
    break; 
  } #when no rows can be continued, we are done
}
nrow(dd)