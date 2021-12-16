library(tidyverse)
library(matrixcalc)
dat <- readLines("data/day_14")
dd <- data.frame(x = dat) %>% 
  filter(str_detect(x, "->")) %>% 
  separate(x, into = c("source", "insert"), sep = " -> ") %>% 
  mutate(val1 = paste0(str_extract(source, "^[A-Z]{1}"), insert),
         val2 = paste0(insert, str_extract(source, "[A-Z]{1}$")))

str <- unlist(str_split(dat[1], ""))
start_char <- str[1]
end_char <- last(str)

chars <- paste0(str[-20], lead(str)[-20])

mat <- matrix(0, nrow = 100, ncol = 100, dimnames = list(row = dd$source, col = dd$source))

for(i in 1:100) {
  mat[dd$val1[i], dd$source[i]] <- 1
  mat[dd$val2[i], dd$source[i]] <- mat[dd$val2[i], dd$source[i]] + 1
}

start_vec <- table(factor(chars, levels = dd$source))

final <- matrix.power(mat, 40) %*% start_vec #change to 10 for first star!
data.frame(final) %>% 
  rownames_to_column() %>% 
  separate(rowname, into = c("c1", "c2"), sep = 1) %>% 
  pivot_longer(cols = c(c1, c2)) %>% 
  group_by(value) %>% 
  summarize(vount = sum(final)) %>% 
  mutate(start = value %in% c(start_char, end_char)) %>%  
  mutate(vount = ifelse(start, (vount + 1)/2, vount/2)) %>% 
  summarize(diff = max(vount) - min(vount)) 
