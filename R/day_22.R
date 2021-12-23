library(tidyverse)
dd <- data.frame(x = readLines("data/day_22"))
dd <- dd %>% 
  separate(x, sep = " ", into = c("status", "coord")) %>% 
  separate(coord, sep = ",", into = c("x", "y", "z")) %>% 
  mutate(across(matches("[xyz]"), function(x) str_remove_all(x, "[xyz]="))) %>%
  separate(x, into = c("x1", "x2"), sep = "\\.\\.", convert = T) %>% 
  separate(y, into = c("y1", "y2"), sep = "\\.\\.", convert = T) %>% 
  separate(z, into = c("z1", "z2"), sep = "\\.\\.", convert = T) 

aaa <- data.frame(Var1 = integer(0),
                 Var2 = integer(0),
                 Var3 = integer(0))
i <- 1
for(i in 1:nrow(dd)) {
  if(dd$status[i] == "on") {
    aaa <- union(aaa, expand.grid(dd$x1[i]:dd$x2[i], dd$y1[i]:dd$y2[i], dd$z1[i]:dd$z2[i]))
  } else {
    aaa <- setdiff(aaa, expand.grid(dd$x1[i]:dd$x2[i], dd$y1[i]:dd$y2[i], dd$z1[i]:dd$z2[i]))
  }
}
nrow(aaa) #first star

make_cube <- function(x1, x2, y1, y2, z1, z2) {
  #browser()
  if(missing(x1)) {
    return(     data.frame(x1 = integer(0),
                           x2 = integer(0),
                           y1 = integer(0),
                           y2 = integer(0),
                           z1 = integer(0),
                           z2 = integer(0))  )
  }
  if(x1 <= x2 && y1 <= y2 && z1 <= z2) {
    return(   data.frame(x1 = x1, x2 = x2, y1 = y1, y2 = y2, z1 = z1, z2 = z2) )
  } else {
    data.frame(x1 = integer(0),
               x2 = integer(0),
               y1 = integer(0),
               y2 = integer(0),
               z1 = integer(0),
               z2 = integer(0)) 
  }
}

cubediff <- function(c1, c2) { #ci = c(x1, x2, y1, y2, z1, z2) this is c1 \setminus c2
  #browser()
  if(!intersects(c1, c2)) {
    return(make_cube(c1[1], c1[2], c1[3], c1[4], c1[5], c1[6]))
  }
  minx <- c1[1] 
  maxx <- c1[2]
  miny <- c1[3]
  maxy <- c1[4]
  minz <- c1[5]
  maxz <- c1[6]
  cubes <- data.frame(x1 = integer(0),
                      x2 = integer(0),
                      y1 = integer(0),
                      y2 = integer(0),
                      z1 = integer(0),
                      z2 = integer(0))
  if(c2[2] < maxx && c2[2] >= minx) {
    cubes <- bind_rows(cubes, make_cube(c2[2] + 1, maxx, miny, maxy, minz, maxz))
    maxx <- c2[2]
  }
  if(c2[1]> minx && c2[1] <= maxx) {
    cubes <- bind_rows(cubes, make_cube(minx, c2[1] - 1, miny, maxy, minz, maxz))
    minx <- c2[1]
  }
  if(c2[4] < maxy && c2[4] >= miny) {
    cubes <- bind_rows(cubes, make_cube(minx, maxx, c2[4] + 1, maxy, minz, maxz))
    maxy <- c2[4]
  }
  if(c2[3] > miny && c2[3] <= maxy) {
    cubes <- bind_rows(cubes, make_cube(minx, maxx, miny, c2[3] - 1, minz, maxz))
    miny <- c2[3]
  }
  if(c2[6] < maxz && c2[6] >= minz) {
    cubes <- bind_rows(cubes, make_cube(minx, maxx, miny, maxy, c2[6] + 1, maxz))
    maxz <- c2[6]
  }
  if(c2[5] > minz && c2[5] <= maxz) {
    cubes <- bind_rows(cubes, make_cube(minx, maxx, miny, maxy, minz, c2[5] - 1))
    minz <- c2[5]
  }
  cubes
}

cubeint <- function(c1, c2) {
  #browser()
  make_cube(max(c1[1], c2[1]), min(c1[2], c2[2]), max(c1[3], c2[3]), min(c1[4], c2[4]), max(c1[5], c2[5]), min(c1[6], c2[6]))
}

contains <- function(c1, c2) { #true if c1 contains c2
  c2 <- c2 * c(1, -1, 1, -1, 1, -1)
  c1 <- c1 * c(1, -1, 1, -1, 1, -1)
  all(c1 <= c2)
}

seg_intersect <- function(a, b) {
  (a[1] <= b[2] && a[2] >= b[1]) || ( b[1] <= a[2] && b[2] >= a[1] )
}

intersects <- function(c1, c2) {
  all(seg_intersect(c1[1:2], c2[1:2]), seg_intersect(c1[3:4], c2[3:4]), seg_intersect(c1[5:6], c2[5:6]))
}

any_intersects <- function(aa, new_cube) {
  for(x in 1:nrow(aa)) {
    cube_x <- c(aa$x1[x], aa$x2[x], aa$y1[x], aa$y2[x], aa$z1[x], aa$z2[x])
    if(intersects(cube_x, new_cube)) {
      return(TRUE)
    }
  }
  FALSE
}

any_subsets <- function(aa, new_cube) {
  for(x in 1:nrow(aa)) {
    cube_x <- c(aa$x1[x], aa$x2[x], aa$y1[x], aa$y2[x], aa$z1[x], aa$z2[x])
    if(contains(cube_x, new_cube) || contains(new_cube, cube_x)) {
      return(TRUE)
    }
  }
  FALSE
}

all_disjoint <- function(aa, new_cube) {
  for(x in 1:nrow(aa)) {
    cube_x <- c(aa$x1[x], aa$x2[x], aa$y1[x], aa$y2[x], aa$z1[x], aa$z2[x])
    if(intersects(cube_x, new_cube)) {
      return(FALSE)
    }
  }
  TRUE
}

#here comes the fun!

old_cubes <- make_cube( dd$x1[1], dd$x2[1], dd$y1[1], dd$y2[1], dd$z1[1], dd$z2[1] )
for(i in 1:nrow(dd)) {
  new_cube <- make_cube(dd$x1[i], dd$x2[i], dd$y1[i], dd$y2[i], dd$z1[i], dd$z2[i])
  if(dd$status[i] == "on") {
    for(x in 1:nrow(old_cubes)) {
      cube_x <- c(old_cubes$x1[x], old_cubes$x2[x], old_cubes$y1[x], old_cubes$y2[x], old_cubes$z1[x], old_cubes$z2[x])
      cube_x
      new_cube <- purrr::map_df(1:nrow(new_cube), function(j) {
        new_cube_j <- c(new_cube$x1[j], new_cube$x2[j], new_cube$y1[j], new_cube$y2[j], new_cube$z1[j], new_cube$z2[j])
        bind_rows(cubeint(new_cube_j, cube_x), cubediff(new_cube_j, cube_x))
      }) 
    }
    re <- integer(0)
    for(j in 1:nrow(new_cube)) {
      if(any_subsets(old_cubes, new_cube[j,])) {
        re <- c(re, j)
      }
    }
    if(length(re) > 0) {
      new_cube <- new_cube[-re,]
    }
    old_cubes <- bind_rows(old_cubes, new_cube)
  } else { #status == "off"
    new_cube_vec <- c(new_cube$x1[1], new_cube$x2[1], new_cube$y1[1], new_cube$y2[1], new_cube$z1[1], new_cube$z2[1])
    old_cubes <- purrr::map_df(1:nrow(old_cubes), function(j) {
      old_cube_j <- c(old_cubes$x1[j], old_cubes$x2[j], old_cubes$y1[j], old_cubes$y2[j], old_cubes$z1[j], old_cubes$z2[j])
      cubediff(old_cube_j, new_cube_vec)
    })
  }
  print(i)
  print(sum(apply(distinct(old_cubes), 1, function(x) (x[2] - x[1]  + 1) * (x[4] - x[3]  + 1) * (x[6] - x[5]  + 1)  ) ), digits = 20)
}

print(sum(apply(distinct(old_cubes), 1, function(x) (x[2] - x[1]  + 1) * (x[4] - x[3]  + 1) * (x[6] - x[5]  + 1)  ) ), digits = 20) #second star!
