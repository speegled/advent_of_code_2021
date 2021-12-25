library(tidyverse)
dd <- readLines("data/day_25_real")

`mod` <- function(x, y = 10) {
  ifelse(x %% y == 0, y, x %% y)
}

ncol <- str_length(dd[1])
nrow <- length(dd)


move_right <- function(x, y = ncol) {
  ifelse(x > nrow * (y - 1), x - nrow * (ncol - 1), x + nrow)
}

move_down <- function(x, y = nrow) {
  ifelse(x %% y == 0, x - y + 1, x + 1)
}


mat <- matrix(unlist(str_split(dd, pattern = "")), ncol = ncol, byrow = T)
right_movers <- which(mat == ">")
down_movers <- which(mat == "v")
open <- setdiff(1:(nrow * ncol), union(right_movers, down_movers))                

open_mat <- mat
open_mat <- ifelse(mat == ".", TRUE, FALSE)
sum(open_mat)
i <- 1
repeat {
  can_move_right <- right_movers[open_mat[move_right(right_movers)]]
  right_movers[open_mat[move_right(right_movers)]] <- move_right(can_move_right)
  open_mat[move_right(can_move_right)] <- FALSE
  open_mat[can_move_right] <- TRUE
  
  can_move_down <- down_movers[open_mat[move_down(down_movers)]]
  down_movers[open_mat[move_down(down_movers)]] <- move_down(can_move_down)
  open_mat[move_down(can_move_down)] <- FALSE
  open_mat[can_move_down] <- TRUE
  
  if(length(can_move_down) + length(can_move_right) == 0) {
    break
  }
  if(i %% 1000 == 0) {
    print(i)
  }
  i <- i + 1
}
i

