library(tidyverse)

dd <- data.frame(x = readLines("/Users/speegled/Documents_No_Icloud/R_projects/advent_of_code_2021/data/day_24"))
dd <- dd %>% 
  separate(x, into = c("command", "x", "y"), sep = " ") #sep = " " ARRRRRRGH


poss <- data.frame(z = 0)

for(m in 1:3) {
  poss <- poss %>% mutate(wnew = paste(0:9, collapse = ","), .before = z) %>% 
    separate_rows(wnew, convert = T)
  poss <- eval(parse(text = paste0("rename(poss, w", m, "= wnew)")))
  if(m < 14) {
    range <- which(dd$command == "inp")[c(m, m + 1)] + c(1, -1)
  } else {
    range <- c(236, 252)
  }
  
  for(k in 1:nrow(poss)) {
    w <- as.numeric(poss[k, ncol(poss) - 1 ])
    z <- as.numeric(poss[k, "z"])
    x <- 0
    y <- 0
    for(i in range[1]:range[2]) {
      com <- dd$command[i]
      v1 <- dd$x[i]
      v2 <- dd$y[i]
      if(com == "inp") {
        eval(parse(text = paste(v1, "<-", val[j])))
        j <- j + 1
      } else {
        if(com == "mul") {
          eval(parse(text = paste(v1, "<-", v1, "*", v2)))  
        } else {
          if(com == "add") {
            eval(parse(text = paste(v1, "<-", v1, "+", v2))) 
          } else {
            if(com == "div") {
              eval(parse(text = paste(v1, "<- trunc(", v1, "/", v2, ")"))) 
            } else {
              if(com == "mod") {
                eval(parse(text = paste(v1, "<-", v1, "%%", v2)))
              } else {
                eval(parse(text = paste(v1, "<-", v1, "==", v2)))
              }
            }
          }
        }
      }
    }
    poss[k, "z"] <- z
  }
  
  poss$value <- poss %>% 
    select(starts_with("w")) %>% 
    apply(1, function(x) paste0(x, collapse = "")) %>% 
    as.numeric()
  
  poss <- poss %>% 
    group_by(z) %>% 
    slice_max(value) %>% 
    ungroup() %>% 
    select(-value)
}

three_poss <- poss
save(three_poss, "data/three_pos.rda")