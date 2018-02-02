
# Challenge #227 Easy -----------------------------------------------------

gcd <- function(x, y) {
  r <- x%%y
  return(ifelse(r, gcd(y, r), y))
}

c277 <- function(x, y) {
  d < gcd(x,y)
  return(c(x/d, y/d)
}

## BONUS

bonus <- function(x, y) {
  ind <- strsplit(c(x, y), "")
  for(i in seq_along(ind[[1]])) {
    poss.y <- which(ind[[1]][i] == ind[[2]])
    if(length(poss.y) != 0)
      ind[[1]][i] <- 1; ind[[2]][poss.y[1]] <- 1
  }
  for(i in seq_along(ind)) {
    if(all(ind[[i]] == "1")) {
      ind[[i]] <- 1
      next
    }
    ones <- which(ind[[i]] == "1")
    ind[[i]] <- ind[[i]][-ones]
  }
  return(c(paste0(ind[[1]], collapse = ""), paste0(ind[[2]], collapse = "")))
}
