
# Challenge 239 Easy ------------------------------------------------------

threes <- function(x) {
  out <- list()
  while(x > 1) {
    if(x %% 3 == 0) {
      out[[length(out)+1]] <- c(x, 0)
      x <- x/3
      next
    }
    one <- ifelse((x+1) %% 3 == 0, 1, -1)
    out[[length(out)+1]] <- c(x, one)
    x <- (x+one)/3
  }
  out[[length(out)+1]] <- c(x, NA)
  return(out)
}

output <- threes(31337357)

matrix(unlist(output), ncol = 2, byrow = T)

