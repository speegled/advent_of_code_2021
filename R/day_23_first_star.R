library(tidyverse)
library(igraph)
x <- readLines("data/day_23")
x[5] <- paste0("##", str_remove_all(x[5], " "), "##")
x[4] <- paste0("##", str_remove_all(x[4], " "), "##")

mat <- matrix(unlist(str_split(x, "")), nrow = 5, byrow = T)
moves <- c(1, -1, 5, -5)

mat_empty <- mat
mat_empty[mat %in% LETTERS] <- "."
mat_empty[mat_empty == "."] <- which(mat_empty == ".")
score_vec <- c(1, 1, 10, 10, 100, 100, 1000, 1000)
ss <- score_vec
possible_moves_vec <- c(7, 12, 22, 32, 42, 52, 57)
home_positions <- matrix(c(18, 19, 18, 19, 28, 29, 28, 29, 38, 39, 38, 39, 48, 49, 48, 49), ncol = 2, byrow = T) #change
home_vec <- c(18, 19, 28, 29, 38, 39, 48, 49)

nr <- nrow(mat)
home_val <- c(27, 27, 37, 37, 47, 47, 57, 57)
home_vals <- list(c("18", "19"), c("28", "29"), c("38", "39"), c("48", "49"))
possible_moves_vec <- as.character(c(7, 12, 22, 32, 42, 52, 57))


gg <- data.frame(vertices = as.numeric(mat_empty[str_detect(mat_empty, "[0-9]")]))
gg$vertices
edges <- character(0)
for(i in gg$vertices) {
  edges[i] <- " "
  if((mat_empty[i - 1]) %in% gg$vertices ) {
    edges[i] <- paste(edges[i], i - 1)
  }
  if((mat_empty[i +1]) %in% gg$vertices ) {
    edges[i] <- paste(edges[i], i + 1)
  }
  if((mat_empty[i + nrow(mat_empty)]) %in% gg$vertices ) {
    edges[i] <- paste(edges[i], i + nrow(mat_empty))
  }
  if((mat_empty[i - nrow(mat_empty)]) %in% gg$vertices ) {
    edges[i] <- paste(edges[i], i - nrow(mat_empty))
  }
} 

gg$edges <- edges[!is.na(edges)]
gg$vertices

gg <- separate_rows(gg, edges, convert = F) 
gg <- filter(gg, edges != "") %>% 
  mutate(vertices = as.character(vertices))
gg <- igraph::graph_from_edgelist(as.matrix(gg), directed = FALSE)

gg <- simplify(gg)

#' get all open positions

find_home_base <- function(bb, pos, type) {
  if(missing(type)) {
    type = bb$am[bb$pos == pos]
  }
  
  hh <- case_when(
    type == "A" ~ home_vals[[1]],
    type == "B" ~ home_vals[[2]],
    type == "C" ~ home_vals[[3]],
    type == "D" ~ home_vals[[4]]
  )
  other_positions <- bb$pos[bb$am != type & bb$pos != pos]
  
  hh
}

cover <- function(bb, pos, debug = F) {
  #covers up all of the home bases that need to be covered.
  if(debug) {
    browser()
  }
  
  cv <- c("18", "28", "38", "48")
  
  type <- bb$am[bb$pos == as.integer(pos)]
  hh <- find_home_base(bb, pos, type)
  positions <- setdiff(as.character(bb$pos), pos) #where all the other amphipods are located
  other_positions <- bb$pos[bb$am != type & bb$pos != pos] 
  if(pos %in% hh && rje::is.subset(other_positions, hh)) { #already in home base, and home base is all of correct type
    return("empty")
  }
  if(length(setdiff(hh, other_positions)) == length(hh)) { #if home row is open, then don't cover no matter where start position is
    cv <- setdiff(cv, hh)  
  }
  if(pos %in% hh && !rje::is.subset(hh, other_positions)) { #if in own home along with wrong type of amphipod, then don't cover own home
    cv <- setdiff(cv, hh)
  }
  if(pos %in% setdiff(unlist(home_vals), hh)) { #if in other home, then don't cover other home
    home_row <- which(pos == unlist(home_vals))
    cv <- setdiff(cv, home_positions[home_row, 1])
  }
  setdiff(cv, pos) #never want to remove the point itself; not sure if this would happen anyway
}


find_moves <- Vectorize(function(bb, row, pos = NA, debug = F) {
  if(debug) {
    browser()
  }
  if(is.na(pos)) {
    pos <- as.character(bb$pos[row])
  }
  
  if(bb$moves[row] > 1) {
    return(character(0))
  }
  
  positions <- setdiff(as.character(bb$pos), pos) #where all the other amphipods are located
  gg2 <- delete_edges(gg, E(gg) [from(positions)])
  nbhd <- setdiff(attr(neighborhood(gg2, order = 20, nodes = pos)[[1]], which = "name"), pos)
  
  type <- bb$am[row]
  other_positions <- bb$pos[bb$am != type] 
  hb <- find_home_base(bb, pos)
  if(bb$moves[row] == 0) {
    if(length(intersect(other_positions, hb)) == 0) {
      nbhd <- intersect(nbhd, union(find_home_base(bb, pos), possible_moves_vec)) 
      if(length(intersect(nbhd, find_home_base(bb, pos))) > 0) {
        return(as.character(max(as.integer(intersect(nbhd, find_home_base(bb, pos))))))
      }
    } else {
      nbhd <- intersect(nbhd, possible_moves_vec)
    }
  } else if (length(intersect(other_positions, hb)) == 0) {
    nbhd <- max(as.integer(intersect(nbhd, find_home_base(bb, pos))))
    if(is.finite(nbhd)) {
      return(as.character(nbhd))
    } else {
      return(character(0))
    }
  } else {
    return(character(0))
  }
  return(nbhd)
}, vectorize.args  = "row")


move_far <- memoise::memoise(function(bb = bb, row, end_pos, debug = F, count = F, ss) {
  end_pos <- as.character(end_pos)
  if(bb$pos[row] %in% possible_moves_vec && end_pos %in% possible_moves_vec) {
    stop("cannot move within hallway")
  }
  if(end_pos %in% c("23", "37", "51", "65")) {
    stop("cannot stop outside of room")
  }
  if(debug) {browser()}
  i <- 0
  
  
  positions <- setdiff(as.character(bb$pos), as.character(bb$pos[row]))
  gg2 <- delete_edges(gg, E(gg) [from(positions)])
  path_to_vert <- shortest_paths(gg2, from = as.character(bb$pos[row]), to = end_pos, output = "vpath")$vpath[[1]]
  if(length(path_to_vert) == 0) {
    stop(paste0(row, end_pos, "no path found"))
  }
  
  bb$pos[row] <- as.integer(end_pos)
  bb$scores[row] <- bb$scores[row] + (length(path_to_vert) - 1) * ss[row]
  if(bb$pos[row] %in% find_home_base(bb, bb$pos[row])) {
    bb$moves[row] <- bb$moves[row] + 1
  } 
    
  bb$moves[row] <- bb$moves[row] + 1
  

  #print_mat(bb)
  bb
})

reset <- function() {
  bb <- data.frame(am = rep(LETTERS[1:4], each = 2),
                   copy = 1:8,
                   pos = as.vector(sapply(LETTERS[1:4], function(let) which(mat == let)))
  )
  bb <- bb %>% mutate(
    first_left = 6,
    first_right = 58,
    home_open = FALSE,
    home_base = rep(c(17, 27, 37, 47), each = 2),
    scores = 0,
    can_go_home = FALSE,
    moves = 0,
    not_trapped_in_home = ifelse(pos %% 10 == 8, TRUE, F))
  bb
}

print_mat <- function(bb) {
  mat_empty[bb$pos] <- paste0(rep(LETTERS[1:4], each = 2), 1:8) #change
  print(mat_empty)
}





print_mat(bb)

library(future.apply)
plan("sequential")
plan("multisession", workers = 12)



bb <- reset() %>% 
  select(am, pos, scores, moves)
best_score <- Inf
for(i in 1:20) {
  bb <- data.table::rbindlist(future_lapply(1:(nrow(bb)/(8)), function(rr) {
    bb_temp <- bb[ ((rr - 1) * 8 + 1) : (rr * 8 ), ]
    ff <- find_moves(bb_temp, 1:8)
    if(length(unlist(ff)) == 0) {
      # if(length(intersect(bb_temp$pos, home_vec)) == 8) { #change this
      #   print(sum(bb_temp$scores))
      #   # if(sum(bb$scores) < best_score) {
      #   #   best_score <<- sum(bb_temp$scores)
      #   #   print(best_score)
      #   # }
      #  }
      return( data.frame(am = character(0), pos = integer(0), scores = numeric(0), moves = numeric(0))  )
    }
    bind_rows(lapply(1:8, function(x) {
      if(length(ff[[x]]) == 0) {
        return(data.frame(am = character(0), pos = integer(0), scores = numeric(0), moves = numeric(0)))
      }
      bind_rows(lapply(ff[[x]], function(pos) {
        move_far(bb_temp, x, pos, ss = ss)
      }))
    }))
  }))

  bb <- bb %>% 
    mutate(game = rep(1:(n()/8),  each = 8)) %>% 
    group_by(game) %>% 
    mutate(pp = paste(pos, collapse = "-"), tot_score = sum(scores)) %>% 
    ungroup() %>% 
    group_by(pp, tot_score) %>% 
    mutate(row = 1:n()) %>% 
    filter(row < 9) %>% 
    group_by(pp) %>% 
    slice_min(tot_score) %>% 
    ungroup() %>% 
    select(am, pos, scores, moves)
  
  if(i > 10) {
    tot_scores <- future_lapply(1:(nrow(bb)/(8)), function(rr) {
      bb_temp <- bb[ ((rr - 1) * 8 + 1) : (rr * 8 ), ]
      ff <- find_moves(bb_temp, 1:8)
      if(length(unlist(ff)) == 0) {
        if(length(intersect(bb_temp$pos, home_vec)) == 8) { #change this
          return(sum(bb_temp$scores))
          # if(sum(bb$scores) < best_score) {
          #   best_score <<- sum(bb_temp$scores)
          #   print(best_score)
          # }
        }
      }
      return(numeric(0))
    })
    if(min(unlist(tot_scores))< best_score) {
      best_score <- min(unlist(tot_scores))
      bb <- bb %>% 
        mutate(game = rep(1:(n()/8),  each = 8)) %>% 
        group_by(game) %>% 
        mutate(pp = paste(pos, collapse = "-"), tot_score = sum(scores)) %>% 
        ungroup() %>% 
        filter(tot_score < best_score) %>% 
        select(am, pos, scores, moves)
    }
  }
  if(nrow(bb) == 0) {
    break
  }
  print(i)
}



print_mat(bb[9:16,])









bb <- data.table::rbindlist(future_lapply(1:(nrow(bb)/(8)), function(rr) {
  if(rr %% 1000 == 0) {
    print(rr)
  }
  bb_temp <- bb[ ((rr - 1) * 8 + 1) : (rr * 8 ), ]
  ff <- find_moves(bb_temp, 1:8)
  if(length(unlist(ff)) == 0) {
    if(length(intersect(bb$pos, home_vec)) == 8) { #change this
      if(sum(bb$scores) < best_score) {
        best_score <<- sum(bb_temp$scores)
        print(best_score)
      }
    }
    return( data.frame(am = character(0), pos = integer(0), scores = numeric(0), moves = numeric(0))  )
  }
  bind_rows(lapply(1:8, function(x) {
    if(length(ff[[x]]) == 0) {
      return(data.frame(am = character(0), pos = integer(0), scores = numeric(0), moves = numeric(0)))
    }
    bind_rows(lapply(ff[[x]], function(pos) {
      move_far(bb_temp, x, pos, ss = ss)
    }))
  }))
}))
bb <- bb %>% 
  mutate(game = rep(1:(nrow(bb)/8), each = 8)) %>% 
  group_by(game) %>% 
  mutate(unique_game = paste(pos, collapse = "-")) %>% 
  group_by(game) %>% 
  mutate(tot_score = sum(scores)) %>% 
  ungroup() %>% 
  group_by(unique_game) %>% 
  slice_min(tot_score) %>% 
  ungroup() %>% 
  distinct(am, pos, scores, tot_score, unique_game, moves) %>% 
  select(am, pos, scores, moves) %>% 
  as.data.frame()

sum(bb$am == LETTERS[c(1,1,2,2,3,3,4,4)])

find_moves(bb[(105* 8 + 1): (106 * 8),], row = 8, debug  = T)

bb <- bb[1:(100 * 8),]


slice_min(scores, with_ties = FALSE) %>% 
  arrange(unique_game)

ungroup() %>% 
  select(-game) %>% 
  arrange(unique_game)
bb <- distinct(bb)
bb %>% 
  group_by(unique_game) %>% 
  mutate(n = n()) %>% 
  arrange(desc(n)) 



bb <- reset()
find_moves(bb, 1:8)
bb
bb <- move_far(bb, 8, 42, debug = F)
print_mat(bb)
find_moves(bb, 1:8)
bb <- move_far(bb, 6, 57, F)
find_moves(bb, 1:8, debug = F)
print_mat(bb)
bb <- move_far(bb, 8, 49)
find_moves(bb, 1:8)
print_mat(bb)
bb <- move_far(bb, 3, 12)
print_mat(bb)
find_moves(bb, 1:8)
bb <- move_far(bb, 7, 48)
print_mat(bb)
find_moves(bb, 1:8, debug = F)
bb <- move_far(bb, 5, 39)
find_moves(bb, 1:8)

bb <- move_far(bb, 7, 57)
find_moves(bb, 1:8)
bb <- move_far(bb, 2, 19)
find_moves(bb, 1:8)
bb <- move_far(bb, 1, 18)
find_moves(bb, 1:8)
bb <- move_far(bb, 8, 52)
bb <- move_far(bb, 6, 38)
bb <- move_far(bb, 8, 49)
find_moves(bb, 1:8)
bb <- move_far(bb, 7, 48)
find_moves(bb, 1:8)