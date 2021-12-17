library(stringr)
x <- unglue::unglue_data(readLines("data/day_17"), 
                         patterns = "target area: x={low_x}..{high_x}, y={low_y}..{high_y}", 
                         convert = T)
abs(x$low_y) * (abs(x$low_y) - 1) / 2 #first star!

vy <- x$low_y:abs(x$low_y) #possible initial velocity y
vx <- 1:(x$high_x)         #possible initial velocity x

times_hits_y <- function(x) {
  a <- cumsum(x:low_y)
  which(a >= low_y & a <= high_y)
}
times_hits_x <- function(x) {
  a <- cumsum(c(x:0, rep(0, 2 * abs(low_y))))
  which(a >= low_x & a <= high_x)
}

outer_fun <- Vectorize(function(x, y){
  length(intersect(times_hits_x(x), times_hits_y(y)))
}, vectorize.args = c("x", "y"))

sum(outer(vx, vy, outer_fun) > 0)
