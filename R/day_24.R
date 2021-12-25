#be sure to switch line 64 for the first star!
library(tidyverse)
library(future.apply)
plan("multisession", workers = 9)
load("data/three_pos.rda") #this is the z score after the first three interations through the loop. created by make_three_pos.R

dd <- data.frame(x = readLines("/Users/speegled/Documents_No_Icloud/R_projects/advent_of_code_2021/data/day_24"))
dd <- dd %>% 
  separate(x, into = c("command", "x", "y"), sep = " ") #sep = " " ARRRRRRGH

three_poss <- three_poss %>% 
  filter(w1 > 0, w2 > 0, w3 > 0)
aaa <- 1
bbb <- 1
ccc <- 2
for(aaa in 1:9) { #switch this to 9:1 along with the others for the first star, or just wait
  for(bbb in 1:9) {
    ddd <- future_sapply(1:9, function(ccc) {
      poss <- filter(three_poss, w1 == aaa, w2 == bbb, w3 == ccc)
      for(m in 4:14) {
        poss <- poss %>% mutate(wnew = paste(1:9, collapse = ","), .before = z) %>% 
          separate_rows(wnew, convert = T)
        
        poss <- eval(parse(text = paste0("rename(poss, w", m, "= wnew)")))
        if(m < 14) {
          new_z <- sapply(1:nrow(poss), function(k) { #this is a summary of what each batch of instructions does
            range <- which(dd$command == "inp")[c(m, m + 1)] + c(1, -1)
            w <- as.numeric(poss[k, ncol(poss) - 1 ])
            z <- as.numeric(poss[k, "z"])
            x <- 0
            y <- 0
            ar1 <- as.integer(dd$y[range[1] + 4])
            ar2 <- as.integer(dd$y[range[1] + 3])
            ar3 <- as.integer(dd$y[range[1] + 14])
            
            x <- (((z %% 26) + ar1) != w)
            z <- trunc(z/ar2)
            z <- (25 * x + 1) * z + (w + ar3) * x
            z
          })
        } else { #the last batch is different
          new_z <- sapply(1:nrow(poss), function(k) {
            w <- as.numeric(poss[k, ncol(poss) - 1 ])
            z <- as.numeric(poss[k, "z"])
            x <- 0
            y <- 0
            
            t1 <- ((z %% 26) - 14) != w
            z <- trunc(z/26) * (25 * t1 + 1) + (w + 4) * t1
            z
          })
        }
        
        poss$z <- new_z
        
        if(anyDuplicated(poss$z)) {
          poss$value <- poss %>% 
            select(starts_with("w")) %>% 
            apply(1, function(x) paste0(x, collapse = "")) %>% 
            as.numeric()
          
          poss <- poss %>% 
            group_by(z) %>% 
            slice_min(value) %>% #switch to slice_max for the first star!
            ungroup() %>% 
            select(-value)
        } 
      }
      
      if(any(poss$z == 0)) {
        poss_2 <- filter(poss, z == 0)
        print(paste0(poss_2[,1:14], collapse = "")) #second star!
      }
      min(abs(poss$z))
    })
    print(c(aaa, bbb, min(ddd)))
  }
}
plan("sequential")