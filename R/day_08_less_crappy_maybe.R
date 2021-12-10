library(tidyverse)
library(igraph)

get_graph <- function(vec) {
  dd <- vec
  dd <- paste(dd, collapse = " ")
  
  vals <- utf8ToInt(dd) - 96
  ppp <- matrix(0, ncol = 7, nrow = 10)
  i <- 1
  k <- 1
  while(k <= length(vals)) {
    j <- 1
    while(vals[k] != -64) {
      ppp[i, vals[k]] <- 1
      k <- k + 1
    }
    i <- i + 1
    k <- k + 1
  }
  
  edges <- as.data.frame(which(ppp == 1, arr.ind = T))
  edges <- bind_rows(
    data.frame(row = 1:10, col = 11:20),
    edges
  )
  edges <- mutate(edges,
                  row = factor(row, labels = as.character(0:9)),
                  col = factor(col, labels = c(letters[1:7], 0:9)))
  graph_from_edgelist(as.matrix(edges))
}

get_index <- function(code, val) {
  alphabetize <- function(ss) {
    ss <- str_remove(ss, " $")
    sapply(unlist(str_split(ss, " ")), function(x) {
      str_split(x, "") %>% 
        unlist() %>% 
        sort() %>% 
        paste0(collapse = "")
    })
  }
  code <- alphabetize(code)
  val <- alphabetize(val)
  sapply(val, function(x) {
    which(x == code)
  })
}


dd <- c("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg ")
base_graph <- get_graph(dd)

dd <- data.frame(x = readLines("data/day_08"))

sapply(1:nrow(dd), function(a) {
  ee <- dd$x[a]

  ee <- str_remove(ee, "\\|.*")
  new_graph <- get_graph(ee)
  
  correspondence_vertex <- isomorphisms(base_graph, new_graph)[[1]]
  attr(correspondence_vertex, "name")
  as.integer(correspondence_vertex)
  val <- str_remove(dd$x[a], ".*\\| ")
  val
  correspondence_vertex[get_index(ee, val)] - 1
  as.integer(paste(correspondence_vertex[get_index(ee, val)] - 1, collapse = ""))
})

sum(.Last.value)
