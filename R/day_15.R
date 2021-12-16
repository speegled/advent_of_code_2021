library(tidyverse)

mat <- readLines("data/day_15") %>% 
  str_split("") 

# mat <- lapply(1:20, function(x) {
#   mat[[x]][1:20]
# })

ncol <-  length(mat[[1]])  
nrow <- length(mat)
mat <- unlist(mat) %>% 
  as.integer() %>% 
  matrix(byrow = T, ncol = ncol) 
mat
mat_original <- mat
transitions <- data.frame(start = rep(1:(nrow * ncol), each = 4)) %>%
  group_by(start) %>% 
  mutate(finish = start + c(1, -1, ncol, -ncol)) %>% 
  filter(finish > 0 & !( start %% nrow == 0 & finish %% nrow == 1 ) & !( start %% nrow == 1 & finish %% nrow == 0) & finish <= nrow * ncol) %>% 
  mutate(score_finish = mat[finish]) 

paths <- data.frame(end = 1,
                    tot_score = 0)

for(i in 1:400) {
  paths <- left_join(paths, transitions, by = c("end" = "start")) %>%
    mutate(tot_score = tot_score + score_finish) %>%
    group_by(finish) %>%
    slice_min(order_by = tot_score, with_ties = FALSE) %>%
    select(tot_score, finish) %>%
    rename(end = finish) %>%
    ungroup()
  if(i %% 150 == 0) {
    print(filter(paths, end == nrow * ncol))
  }
} #first star

new_nrow <- nrow * 5
new_ncol <- ncol * 5

add_mat <- function(x, j) {
  a <- (x + j) %% 10
  ifelse(x + j >= 10, a + 1, a)
}

mat <- matrix(0, nrow = new_nrow, ncol = new_ncol)

for(i in 0:4) {
  for(j in 0:4) {
    mat[1:nrow + j * nrow, 1:ncol + i * ncol] <- add_mat(mat_original, i + j)
  }
}
mat
norm <- function(x) {
  row <- ifelse(x %% ncol == 0, ncol, x %% ncol)
  col <- ceiling(x/nrow - .001/nrow)
  (row + col) + 10
}


paths <- data.frame(end = 1,
                    tot_score = 0)


transitions <- data.frame(start = rep(1:(new_nrow * new_ncol), each = 4)) %>%
  group_by(start) %>% 
  mutate(finish = start + c(1, -1, new_ncol, -new_ncol)) %>% 
  filter(finish > 0 & !( start %% new_nrow == 0 & finish %% new_nrow == 1 ) & !( start %% new_nrow == 1 & finish %% new_nrow == 0) & finish <= new_nrow * new_ncol) %>% 
  mutate(score_finish = mat[finish])


old_paths <- paths

for(i in 1:1000) {
  paths <- left_join(paths, transitions, by = c("end" = "start")) %>% 
    mutate(tot_score = tot_score + score_finish) %>%  
    group_by(finish)  %>% 
    slice_min(order_by = tot_score, with_ties = FALSE) %>% 
    select(tot_score, finish) %>% 
    rename(end = finish) %>% 
    ungroup()
  paths <- left_join(paths, old_paths, by = "end") %>% 
    filter(tot_score.x < tot_score.y | is.na(tot_score.y)) %>% 
    rename(tot_score = tot_score.x) %>% 
    select(-tot_score.y)
  old_paths <- bind_rows(paths, old_paths) %>% 
    group_by(end) %>% 
    slice_min(tot_score, with_ties = FALSE)
  print(nrow(paths))
  print(filter(old_paths, end == new_nrow * new_ncol))
} #second start takes several hours

filter(old_paths, end == new_nrow * new_ncol)

