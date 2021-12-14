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
chars <- character(0)
for(i in 2:length(str)) {
  chars <- c(chars, paste0(str[i-1], str[i]))
}

all_chars <- sort(dd$source)

which_chars <- Vectorize(function(x) {
  which(x == all_chars)
})

dd <- mutate(dd,
             which1 = which_chars(val1),
             which2 = which_chars(val2)) %>% 
  arrange(source)

mat <- matrix(0, nrow = nrow(dd), ncol = nrow(dd))
for(i in 1:nrow(mat)) {
  mat[dd$which1[i], i] <- 1
  mat[dd$which2[i], i] <- 1
}
row.names(mat) <- all_chars
start_vec <- table(factor(which_chars(chars), levels = 1:nrow(dd)))

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