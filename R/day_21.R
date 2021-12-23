library(tidyverse)

`mod` <- function(x, y = 10) {
  ifelse(x %% y == 0, y, x %% y)
}

num_rolls <- 6
p1_start <- 8
p2_start <- 3
p1_pos <- c(p1_start, mod(p1_start + 6, 10))
p1_sum <-p1_pos[2]
p2_pos <- c(p2_start, mod(p2_start + 15, 10))
p2_sum <- p2_pos[2]

update_pos <- function(p) {
  c(p[2], diff(p) - 2 + p[2])
}

update_sum <- function(current, pos) {
  current + mod(pos[2], 10)
}

while(p2_sum < 1000) {
  p1_pos <- update_pos(p1_pos)
  p2_pos <- update_pos(p2_pos)
  p1_sum <- update_sum(p1_sum, p1_pos)
  if(p1_sum >= 1000) {
    num_rolls <- num_rolls + 3
    break
  }
  p2_sum <- update_sum(p2_sum, p2_pos)
  num_rolls <- num_rolls + 6
}

if(p1_sum >= 1000) {
   num_rolls * p2_sum
} else {
  num_rolls * p1_sum
}

end_score <- 21
get_all_games <- function(startpos) {
  rolls <- 3:9
  rr <- data.frame(pos_before_turn = startpos, roll = rolls, num_rolls = 0, all_rolls = "")
  rr <- rr %>% 
    mutate(pos_after_turn = mod(pos_before_turn + roll, 10),
           score = pos_after_turn,
           num_rolls = num_rolls + 1,
           all_rolls = paste(all_rolls, roll)) %>%  
    select(-pos_before_turn, -roll) %>% 
    rename(pos_before_turn = pos_after_turn)
  rr
  rolls <- paste(rolls, collapse = " ")
  while(any(rr$score < end_score)) {
    rr <- rr %>% 
      mutate(roll = rolls) %>% 
      mutate(roll = ifelse(score >= end_score, "0", rolls) ) %>% 
      separate_rows(roll, convert = T) %>% 
      mutate(pos_after_turn = mod(pos_before_turn + roll, 10),
             score = score + pos_after_turn,
             num_rolls = ifelse(roll == 0, num_rolls, num_rolls + 1),
             all_rolls = paste(all_rolls, roll)) %>%  
      select(-pos_before_turn, -roll) %>% 
      rename(pos_before_turn = pos_after_turn)
  }
  rr
}

p1 <- get_all_games(8)
p2 <- get_all_games(3)
str <- " 3 0 0"
p1
p2

facs <- expand.grid(1:3, 1:3, 1:3) %>% 
  apply(1, sum) %>% 
  table()
facs <- c(0, 0, facs)

multiplier <- Vectorize(function(str) {
  prod(facs[ (as.integer(str_split(str, " ")[[1]][-1]))  ])
}, vectorize.args = "str")

p1 <- p1 %>% 
  mutate(num_universes = multiplier(all_rolls)) %>% 
  mutate(player = 1)

p2 <- p2 %>% 
  mutate(num_universes = multiplier(all_rolls)) %>% 
  mutate(player = 2)

pp1 <- p1 %>% 
  group_by(num_rolls) %>% 
  summarize(n_ending = sum(num_universes))

pp2 <- p2 %>% 
  group_by(num_rolls) %>% 
  summarize(n_ending = sum(num_universes))




if(min(pp2$num_rolls) > 1) {
  pp2 <- bind_rows(data.frame(num_rolls = 1:(min(pp2$num_rolls) - 1), n_ending = 0), pp2)
}
if(min(pp1$num_rolls) > 1) {
  pp1 <- bind_rows(data.frame(num_rolls = 1:(min(pp1$num_rolls) - 1), n_ending = 0), pp1)
}

pp1
pp2
162 + 264 * 27 + 17 * 27^2
567 + 438 * 27 + 10 * 27^2
worlds <- c(27)
i <- 1
pp1
for(i in 1:nrow(pp1)) {
  worlds <- c(worlds, (worlds[i] - pp1$n_ending[i] ) * 27 )
}
worlds[1:10]
pp1$worlds_continuing <- worlds[-1]
pp1
27^4
27^4 - (27^3 - 4608) - 249542 

c(27, 27^2, 27^3 - 4608, 27^4 - 27^3 - 4608 - 249542 )
worlds <- c(27)
for(i in 1:nrow(pp2)) {
  worlds <- c(worlds, (worlds[i] - pp2$n_ending[i] ) * 27 )
}
pp2$worlds_continuing <- worlds[-1]

pp1
pp2

p1wins <- 0
p2wins <- 0

pp1
pp2

print(sum(pp1$n_ending[3:10] * pp2$worlds_continuing[2:9]/27), digits = 20) #second star!
print(sum(pp2$n_ending[3:10] * pp1$worlds_continuing[3:10]/27), digits = 20)

