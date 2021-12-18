library(tidyverse )

explode <- function(dd) {
  pos <- min(which(((dd$depth[1:(nrow(dd) - 1)] >= 5) & dd$depth[1:(nrow(dd) - 1)] == dd$depth[2:nrow(dd)])))
  if(pos == 1) {
    dd$x[pos + 2] <- dd$x[pos + 1] + dd$x[pos  + 2]
  } else {
    if(pos + 2 > nrow(dd)) {
      dd$x[pos - 1] <- dd$x[pos] + dd$x[pos - 1]
    } else {
      dd$x[pos - 1] <- dd$x[pos] + dd$x[pos - 1]
      dd$x[pos + 2] <- dd$x[pos+ 1] + dd$x[pos + 2]
    }
  }
  dd$x[pos] <- 0
  dd$depth[pos] <- dd$depth[pos] - 1
  dd <- dd[-(pos + 1),]
  dd
}

split <- function(str) {
  val <- as.integer(str_extract(str, "[0-9]{2}"))
  val <- paste0("[", as.character(floor(val/2)), ",", as.character(ceiling(val/2)), "]")
  str <- str_replace(str, "[0-9]{2}", val)
  str
}

write_string <- function(ee) {
  while(max(ee$depth) > 0) {
    pos <- min(which(((ee$depth[1:(nrow(ee) - 1)] == max(ee$depth)) & ee$depth[1:(nrow(ee) - 1)] == ee$depth[2:nrow(ee)])))
    ee$x[pos] <- paste0("[",ee$x[pos],",",ee$x[pos + 1], "]")
    ee$depth[pos] <- ee$depth[pos] - 1
    ee <- ee[-(pos + 1),]
  }
  ee$x
}

write_data_frame <- function(str) {
  ee <- data.frame(x = unlist(str_split(str, "[,\\[\\]]"))) %>% rownames_to_column()
  str2 <- str_replace_all(str, pattern = "[0-9]{2}", "1")
  full_string <- unlist(unlist(str_split(str2, ",")) %>% str_split(""))
  ee <- mutate(ee, depth = cumsum(full_string == "[") - cumsum(full_string == "]"))
  ee <- filter(ee, str_detect(x, "[0-9]"))
  ee$x <- as.integer(ee$x)
  ee
}

canexplode <- function(dd) {
  any(dd$depth >= 5) && nrow(dd) >= 3
}
cansplit <- function(dd) {
  any(dd$x >= 10)
}

magnitude <- function(str) {
  `-` <- function(x, y) {
    3 * x + 2 * y
  }
  x <- stringr::str_replace_all(str, "\\[", "(") %>% 
    stringr::str_replace_all("\\]", ")") %>% 
    stringr::str_replace_all(",", "-")
  eval(parse(text = x))
}

simplify <- function(dd) {
  while(canexplode(dd) || cansplit(dd)) {
    if(canexplode(dd)) {
      dd <- explode(dd)
    } else {
      dd <- write_data_frame(split(write_string(dd)))
    }
  }
  dd
}

dat <- readLines("data/day_18")
ss <- dat[1]

for(i in 2:length(dat)){
  ss <- paste0("[", ss, ",", dat[i], "]")
  ss <- write_string(simplify(write_data_frame(ss)))
}
magnitude(ss) #first star!

purrr::reduce(dat[-1], .f = add, .init = dat[1])

add <- Vectorize(function(s1, s2) {
  ss <- paste0("[", s1, ",", s2, "]")
  ss <- write_string(simplify(write_data_frame_compact(ss)))
  magnitude(ss)
}, vectorize.args = c("s1", "s2"))

sums <- outer(dat, dat, add)
max(sums) #second star!
