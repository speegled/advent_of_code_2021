dat <- unglue::unglue_data(readLines("data/day_17"), 
                         patterns = "target area: x={low_x}..{high_x}, y={low_y}..{high_y}", 
                         convert = T)
abs(dat$low_y) * (abs(dat$low_y) - 1) / 2 #first star!

vy <- dat$low_y:abs(dat$low_y) #possible initial velocity y
vx <- 1:(dat$high_x)         #possible initial velocity x

times_hits_y <- function(x) {
  a <- cumsum(x:dat$low_y)
  which(a >= dat$low_y & a <= dat$high_y)
}
times_hits_x <- function(x) {
  a <- cumsum(c(x:0, rep(0, 2 * abs(dat$low_y))))
  which(a >= dat$low_x & a <= dat$high_x)
}

outer_fun <- Vectorize(function(x, y){
  length(intersect(times_hits_x(x), times_hits_y(y)))
}, vectorize.args = c("x", "y"))

sum(outer(vx, vy, outer_fun) > 0)
