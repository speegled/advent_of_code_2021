dd <- readLines(here::here("data/day_09"))
library(tidyverse)
library(matrixcalc)
dat <- str_split(paste0(9, dd, 9), "")
ncol <- length(dat[[1]])
mat <- matrix(as.integer(c(rep(10, ncol), unlist(dat), rep(10, ncol))), ncol = ncol, byrow = T)
low_points <- mat < pmin(shift.right(mat), shift.down(mat), shift.left(mat), shift.up(mat))
sum(mat[low_points] + 1)


sinks <- which(low_points)
basin <- sinks

basin_lengths <- sapply(sinks, function(x) {
  basin <- x
  basin_size <- 1
  new_bs <- 2
  while(new_bs > basin_size) {
    basin_size <- length(basin)
    add_to_basin <- integer(0)
    for(i in 1:length(basin)) {
      if(shift.right(mat)[basin[i]] > mat[basin[i]] && shift.right(mat)[basin[i]] < 9) {
        add_to_basin <- c(add_to_basin, basin[i] - nrow(mat))
      }
      if(shift.left(mat)[basin[i]] > mat[basin[i]] && shift.left(mat)[basin[i]] < 9) {
        add_to_basin <- c(add_to_basin, basin[i] + nrow(mat))
      }
      if(shift.down(mat)[basin[i]] > mat[basin[i]] && shift.down(mat)[basin[i]] < 9) {
        add_to_basin <- c(add_to_basin, basin[i] - 1)
      }
      if(shift.up(mat)[basin[i]] > mat[basin[i]] && shift.up(mat)[basin[i]] < 9) {
        add_to_basin <- c(add_to_basin, basin[i] + 1)
      }
    }
    basin <- unique(c(basin, add_to_basin))
    new_bs <- length(basin)
  }
  length(basin)
})

prod(sort(basin_lengths, decreasing = T)[1:3])


