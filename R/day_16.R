library(tidyverse)

bin2dec <- function(x) {
  l <- length(x) - 1
  sum(x * 2 ^(l:0))
}

ddparse <- function(x, recursive_level, debug = FALSE) {
  if(debug) {
     browser()
  }
  recursive_level <- recursive_level + 1
  packet_version <- bin2dec(x[1:3])
  packet_type <- bin2dec(x[4:6])
  x <- x[-(1:6)]
  
  if(packet_type == 4) {
    val <- integer(0)
    while(x[1] == 1) {
      val <- c(val, x[2:5])
      x <- x[-(1:5)]
    }
    values <- bin2dec(c(val, x[2:5]))
    x <- x[-(1:5)]
    
    df <<- bind_rows(df, data.frame(packet_version, packet_type, length_type = NA, values, recursive_level))
    return(x)
  }
  
  if(packet_type != 4) {
    length_type <- x[1]
    x <- x[-1]
    if(length_type == 0) {
      total_length_of_subpackets <- bin2dec(x[1:15])
      x <- x[-(1:15)]

      short_x <- x[1:total_length_of_subpackets]
      x <- x[-(1:total_length_of_subpackets)]
      df <<- bind_rows(df, data.frame(packet_version, packet_type, length_type, values = NA, recursive_level))
      repeat {
        short_x <- ddparse(short_x, recursive_level = recursive_level, debug)
        if(length(short_x) == 0) {
          return(x)
        }
      }
    }
    if(length_type == 1) {
      number_subpackets <- bin2dec(x[1:11])
      x <- x[-(1:11)]
      df <<- bind_rows(df, data.frame(packet_version, packet_type, length_type, values = NA, recursive_level))
      for(i in 1:number_subpackets) {
        x <- ddparse(x, recursive_level, debug)
      }
      return(x)
    }
  }
}

x <- BMS::hex2bin(readLines("data/day_16"))
df <- data.frame(packet_version = numeric(0),
                 packet_type = numeric(0),
                 length_type = numeric(0),
                 values = numeric(0),
                 recursive_level = numeric(0))
ddparse(as.integer(x), 0, debug = FALSE)
sum(df$packet_version) #first star!

repeat {
  max_recursion <- max(df$recursive_level)
  eq <- which(df$recursive_level == max_recursion - 1)
  j <- 1
  i <- 1
  ind <- as.list(eq)
  for(i in 1:length(eq)) {
    while(df$recursive_level[eq[i] + j] > max_recursion - 1 && eq[i] + j <= nrow(df)) {
      ind[[i]] <- c(ind[[i]], df$values[ind[[i]][1] + j])
      j <- j + 1
      df$recursive_level[eq[i] + j]
    }
    j <- 1
  }

  for(i in 1:length(ind)) {
    ind[[i]][1] <- df$packet_type[eq[i]]
    if(df$packet_type[eq[i]] == 4) {
      ind[[i]] <- c(ind[[i]], df$values[eq[i]])  
    }
  }

  values <- sapply(ind, function(x) {
    case_when(
      x[1] == 0 ~ sum(x[-1]),
      x[1] == 1 ~ prod(x[-1]),
      x[1] == 2 ~ min(x[-1]),
      x[1] == 3 ~ max(x[-1]),
      x[1] == 4 ~ x[2],
      x[1] == 5 ~ as.numeric(x[2] > x[3]),
      x[1] == 6 ~ as.numeric(x[2] < x[3]),
      x[1] == 7 ~ as.numeric(x[2] == x[3])
    )
  })
  df$values[eq] <- values
  df <- filter(df, recursive_level < max_recursion)
  if(nrow(df) == 1) {
    break
  }
}
 
df$values #second star!
