library(tidyverse)
dd <- data.frame(x = readLines("data/day_20_real"))
dd <- mutate(dd, x = str_replace_all(x, "\\.", "0") %>% str_replace_all("#", "1"))

code <- as.integer(str_split_fixed(paste(filter(dd, cumsum(x == "") == 0) %>% pull(x), collapse = ""), "", n = 512))

N <- str_length(last(dd$x))

im <- filter(dd, cumsum(x == "") == 1) %>% 
  filter(str_length(x) == N) %>%  
  pull(x) %>% 
  str_split("") %>% 
  unlist() %>% 
  as.integer() %>% 
  matrix(byrow = T, ncol = N)

for(i in 1:3) {
  im <- cbind(cbind(0, im), 0)
  im <- rbind(rbind(0, im), 0)
}

get_vec <- function(im, i, j) {
  paste0(im[i + 1, j + 1], im[i + 1, j +2], im[i + 1, j + 3], 
         im[i + 2, j + 1], im[i + 2, j +2], im[i + 2, j + 3],
         im[i + 3, j + 1], im[i + 3, j +2], im[i + 3, j + 3], collapse = "")
}

enhance_image <- function(im, M) {
  for(j in 1:M) {
    N <- nrow(im)
    newmat <- matrix(NA, nrow = N, ncol = N)
    for(i in 0:(ncol(im)  - 3)) {
      for(j in 0:(nrow(im) - 3)) {
        vec <- get_vec(im, i, j)
        newmat[i + 2, j + 2] <- code[strtoi(vec, base = 2L) + 1]
      }
    }
    infinite_char <- newmat[2,2]
    newmat[is.na(newmat)] <- infinite_char
    im <- newmat
    im <- cbind(cbind(infinite_char, im), infinite_char)
    im <- rbind(rbind(infinite_char, im), infinite_char)
  }
  im
}

sum(enhance_image(im, 2)) #first star
sum(enhance_image(im, 50)) #second star