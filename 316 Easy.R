
# Challenge 316 Easy ------------------------------------------------------

knight <- function(x) {
  moves <- matrix(c(-1,-2,1,-2,-1,2,1,2,-2,-1,2,-1,-2,1,2,1), ncol = 2, byrow = T)
  d_moves <- matrix(data = 0, ncol = 2)
  count <- 0
  sums <- x
  while(!all(apply(d_moves, 2, sum) == x)) {
    count <- count +1
    pars <- apply(moves, 1, function(y) {
      # if(all((sums-y) == 0))
      #   return(0)
      # if(any((sums-y) == 0))
      #   return(1000)
      if(any(abs(sums-y) == 1) & any(abs(sums-y) == 2))
        return(0)
      return(sum(abs(sums-y)))
    })
    d_moves <- rbind(d_moves, moves[which.min(pars),])
    sums <- sums - d_moves[count+1,]
  }
  return(list(Objective = x, Moves = count, Optional = d_moves[-1,]))
}

knight(c(0,1))
knight(c(3,7))
knight(c(1,1))
knight(c(-3,-3))
knight(c(100,100))
