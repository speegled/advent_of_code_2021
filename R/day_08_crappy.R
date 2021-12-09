library(tidyverse)

dd <- c("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg ")
dd <- paste(dd, collapse = " ")

vals <- utf8ToInt(dd) - 96
vals
ppp <- matrix(0, ncol = 7, nrow = 10)
vals
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

colSums(ppp)
rowSums(ppp)
rowSums(mmm)
colSums(mmm)

get_perms(ppp, mmm)

get_perms <- function(pp, mm, type = "cols") {
  if(type == "rows") {
    pp <- t(pp)
    mm <- t(mm)
  }
  col_pp <- colSums(pp)
  col_mm <- colSums(mm)
  outer_col <- outer(col_mm, col_pp, function(x, y) {x == y})
  col_indices <- lapply(1:nrow(outer_col), function(x) which(outer_col[x,]))
  col_indices <- as.matrix(expand.grid(col_indices))
  col_indices <- col_indices[apply(col_indices, 1, function(x) length(unique(x)) == ncol(col_indices)),]
  col_indices
}

dd <- data.frame(x = readLines("dat"))
a <- 1
sapply(1:nrow(dd), function(a) {
  dd <- dd$x[a]
  
  puz <- str_remove(dd, "\\|.*")
  vals <- utf8ToInt(puz) - 96
  mmm <- matrix(0, ncol = 7, nrow = 10)
  vals
  i <- 1
  k <- 1
  while(k <= length(vals)) {
    j <- 1
    while(vals[k] != -64) {
      mmm[i, vals[k]] <- 1
      k <- k + 1
    }
    i <- i + 1
    k <- k + 1
  }
  colSums(mmm)
  rowSums(mmm)
  
  
  
  row_perms <- get_perms(mmm, ppp, type = "rows")
  col_perms <- get_perms(mmm, ppp)

  for(i in 1:nrow(col_perms)) {
    col_mat <- as(as.integer(col_perms[i,]), "pMatrix")
    for(j in 1:nrow(row_perms)) {
      row_mat <- as(as.integer(row_perms[j,]), "pMatrix")
      if(all(solve(row_mat) %*% ppp %*% col_mat == mmm)){
        print(a)
        rrow <- row_mat
        ccol <- col_mat
        break
      }
    }
  }
  
  solve(rrow) %*% ppp %*% ccol == mmm
  solve(row_mat) %*% ppp %*% col_mat == mmm
  ans <- str_remove(dd, ".*\\| ")
  ss <- puz
  alphabetize <- function(ss) {
    ss <- str_remove(ss, " $")
    sapply(unlist(str_split(ss, " ")), function(x) {
      str_split(x, "") %>% 
        unlist() %>% 
        sort() %>% 
        paste0(collapse = "")
    })
  }
  puz <- alphabetize(puz)
  ans <- alphabetize(ans)
  row_vals <- sapply(ans, function(x) {
    which(x == puz)
  })
  
  
  int_val <- sapply(row_vals, function(x) {
    which(rrow %*% c(rep(0, x - 1), 1, rep(0, 10 - x)) == 1) - 1
  }) %>% paste0(collapse = "") %>% as.integer()

  print(int_val)
})
sum(.Last.value)
