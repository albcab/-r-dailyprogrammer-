
# Challenge 318 Intermediate ----------------------------------------------

nba <- function(x) {
  x <- strsplit(x, "\n")[[1]]
  n <- length(x)
  games <- list(x[-1])
  for(i in 2:n)
    games[[i]] <- x[-i]
  round <- sample(c(rep(T, n/2), rep(F, n/2)), n)
  replace <- matrix(c(sample(c(1:n)[round], n/2), sample(c(1:n)[!round], n/2)), nrow = 2)
  replace <- rbind(replace, replace)
  season <- list()
  for(i in 1:(2*(n-1))) {
    season[[i]] <- vector()
    cant <- c(x[round])
    for(j in which(round)) {
      away <- sample(games[[j]][!(games[[j]] %in% cant)], 1)
      season[[i]] <- c(season[[i]], paste(x[j], "-", away))
      games[[j]] <- games[[j]][-which(games[[j]] == away)]
      cant <- c(cant, away)
    }
    round <- !round
    if(nrow(replace) >= i)
      round[replace[i,]] <- !round[replace[i,]]
  }
  return(for(i in seq_along(season)) {
    cat(sprintf("Round %s", i), "\n")
    cat("\n")
    cat(season[[i]], sep = "\n")
    cat("\n")
  })
}
