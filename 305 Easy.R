
# Challenge 305 Easy ------------------------------------------------------

base_perm <- function(base, index = NULL, value = NULL) {
  if(length(index) != 0) {
    n <- floor(logb((base-index*(1-base)), base))
    n_start <- (base-base^n)/(1-base)
    aval <- lapply(1:n, function(x) return(0:(base-1)))
    grid <- expand.grid(aval)
    row <- index-n_start+1
    return(cat(unlist(rev(grid[row,])), "\n"))
  }
  if(length(value) != 0) {
    value <- rev(as.numeric(strsplit(value, "")[[1]]))
    n <- length(value)
    n_start <- (base-base^n)/(1-base)
    aval <- lapply(1:n, function(x) return(0:(base-1)))
    grid <- expand.grid(aval)
    row <- 0
    stop <- T
    while(stop) {
      row <- row+1
      if(all(grid[row,] == value))
        stop <- F
    }
    return(cat(row+n_start-1, "\n"))
  }
}

base_perm(2, 54)
base_perm(2, value = "111000111")
