library(tidyverse)
dd <- data.frame(x = readLines("data/day_19"))
dd <- dd %>% 
  separate(x, into = c("x", "y", "z"), sep =  ",") %>% 
  mutate(scanner = cumsum(str_detect(x, "scanner"))) %>% 
  drop_na() %>% 
  mutate_all(as.integer)

#hack to create SU(3), stored as a list in a. is there an R package that has specia groups?
perm <- matrix(c(0,0,1,1,0,0,0,1,0), nrow = 3)
rot <- matrix(c(0, 1, 0, -1, 0, 0, 0, 0, 1), nrow = 3)
rot2 <- matrix(c(1, 0, 0, 0, 0, 1, 0, -1, 0), nrow = 3)
library(matrixcalc)

a <- list(perm)
for(i in 1:1000) {
  i <- sample(-3:3, 1)
  j <- sample(-2:2, 1)
  k <- sample(-2:2, 1)
  aa <- matrix.power(perm, i) %*% matrix.power(rot, j) %*% matrix.power(rot2, k)
  if(! any(sapply(1:length(a), function(x) {
    all(a[[x]] == aa)
  }))) {
    a[[length(a) + 1]] <- aa
  }
}

df <- data.frame(scanner1 = integer(0),
                 scanner2 = integer(0),
                 position_of_scanner2_in_scanner1_coords = character(0),
                 r = integer(0))

outer_fun <- function(x, y, verbose = FALSE) {
    lapply(1:length(a), function(r) {
      data.frame(x - y %*% a[[r]], r = r)
    })
}

library(future.apply)
plan("multisession")

N <- max(dd$scanner)

pairs <- bind_rows(future_lapply(1:N, function(u) {
  lapply(1:N, function(v) {
    scanner1 <- dd %>% filter(scanner == u) 
    scanner1 <- as.matrix(scanner1[,1:3])
    scanner2 <- dd %>% filter(scanner == v)
    scanner2 <- as.matrix(scanner2[,1:3])
    
    poss <- data.frame(X1 = integer(0),
                       X2 = integer(0),
                       X3 = integer(0),
                       r = integer(0))
    
    for(i in 1:nrow(scanner1)) {
      for(j in 1:nrow(scanner2)) {
        poss <- bind_rows(poss, outer_fun(scanner1[i,], scanner2[j,]))
      }
    }
    
    poss <- poss %>% 
      group_by(X1, X2, X3) %>% 
      summarize(n = n(), r = first(r)) %>% 
      ungroup() %>% 
      arrange(desc(n))
    if(max(poss$n) >= 12) {
      return(data.frame(scanner1 = u,
                        scanner2 = v,
                        position_of_scanner2_in_scanner1_coords = paste0(poss$X1[1], ",", poss$X2[1], ",", poss$X3[1]),
                        r = poss$r[1]))  
    } else {
      return(df)
    }
  })
}))

pairs <- pairs %>% 
  filter(scanner1 != scanner2)
pairs <- pairs %>% 
  separate(position_of_scanner2_in_scanner1_coords, into = c("x", "y" , "z"), sep = ",", remove = F, convert = T)

while(nrow(pairs) < max(pairs$scanner1)^2){
  skeleton <- filter(pairs, FALSE)
  for(i in 1:nrow(pairs)) {
    for(j in 1:nrow(pairs)) {
      if(pairs$scanner1[j] == pairs$scanner2[i]) {
        #print(nrow(pairs))
        temp <- data.frame(scanner1 = pairs$scanner1[i], scanner2 = pairs$scanner2[j])
        vals <- c(pairs$x[j], pairs$y[j], pairs$z[j]) %*% a[[pairs$r[i]]] +c(pairs$x[i], pairs$y[i], pairs$z[i])
        mat <- a[[pairs$r[j]]] %*% a[[pairs$r[i]]]
        r <- which(sapply(1:length(a), function(p) {
          all(a[[p]] == mat)
        }))
        temp <- mutate(temp,
                       x = vals[1],
                       y = vals[2],
                       z = vals[3],
                       position_of_scanner2_in_scanner1_coords = paste0(x, ",", y, ",", z),
                       r = r
        )
        skeleton <- bind_rows(skeleton, temp) %>% 
          distinct()
      }
    }
    if(nrow(skeleton) > 38^2 - 0.5) {
      pairs <- bind_rows(pairs, skeleton) %>% 
        distinct()
      break
    }
    print(nrow(skeleton))
  }
  pairs <- bind_rows(pairs, skeleton) %>% 
    distinct()
}

pairs <- arrange(pairs, scanner1, scanner2)
ss <- filter(pairs, scanner1 == 1)
dd <- mutate(dd, x1 = 0, y1 = 0, z1 = 0)
for(i in 1:nrow(dd)) {
  vals <- c(dd$x[i], dd$y[i], dd$z[i]) %*% a[[pairs$r[dd$scanner[i]]]] + c(pairs$x[dd$scanner[i]], pairs$y[dd$scanner[i]], pairs$z[dd$scanner[i]])
  dd$x1[i] <- vals[1]
  dd$y1[i] <- vals[2]
  dd$z1[i] <- vals[3]
}
dd %>% 
  select(x1, y1, z1) %>% 
  distinct() %>% 
  count()

md <- 0
for(i in 1:38) {
  for(j in 1:38) {
    dist <- abs(ss$x[i] - ss$x[j]) + abs(ss$y[i] - ss$y[j]) + abs(ss$z[i] - ss$z[j])
    if(dist > md) {
      md <- dist
    }
  }
}
md
