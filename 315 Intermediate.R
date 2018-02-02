
# Challenge 315 Intermediate ----------------------------------------------

change <- function(x, player, board) {
  row <- c(x[1]-1, x[1], x[1]+1) 
  col <- c(x[2]-1, x[2], x[2]+1)
  if(any(row == 0))
    row[which(row == 0)] <- nrow(board)
  if(any(row == nrow(board)+1))
    row[which(row == nrow(board)+1)] <- 1
  if(any(col == 0))
    col[which(col == 0)] <- ncol(board)
  if(any(col == ncol(board)+1))
    col[which(col == ncol(board)+1)] <- 1
  check <- expand.grid(row, col)
  spots <- apply(check, 1, function(x) return(board[x[1], x[2]]))
  if(any(player == c("1", "2"))) {
    opponet <- c("1", "2")[c("1", "2") != player]
    me <- sum(spots == player)
    op <- sum(spots == opponet)
    none <- sum(spots == "0")
    if(op > me)
      return(opponet)
    else
      return(ifelse(me %in% c(3, 4), player, "0"))
  } else {
    p1 <- sum(spots == "1")
    p2 <- sum(spots == "2")
    if(sum(p1, p2) == 3)
      return(ifelse(p1 > p2, "1", "2"))
    else
      return("0")
  }
}

life <- function(board) {
  p1 <- which(board == "1", arr.ind = T)
  p2 <- which(board == "2", arr.ind = T)
  none <- which(board == "0", arr.ind = T)
  full <- rbind(p1, p2, none)
  newp1 <- apply(p1, 1, change, "1", board)
  newp2 <- apply(p2, 1, change, "2", board)
  newnone <- apply(none, 1, change, "0", board)
  newfull <- c(newp1, newp2, newnone)
  for(i in 1:nrow(full))
    board[full[i,1], full[i,2]] <- newfull[i]
  return(board)
}

gameoflife <- function(h, w, n) {
  game <- list(matrix(sample(c("0", "1", "2"), h*w, T), nrow = h, ncol = w))
  for(i in 1:n)
    game[[i+1]] <- life(game[[i]])
  return(game)
}

library(animation)

forgif <- gameoflife(50,50,21)

saveGIF(
  for(i in seq_along(forgif)) {
    forgif[[i]] <- matrix(as.numeric(forgif[[i]]), ncol = ncol(forgif[[i]]))
    image(forgif[[i]], col = c("black", "red", "blue"), axes = F)
  }, movie.name = "315int.gif"
)
